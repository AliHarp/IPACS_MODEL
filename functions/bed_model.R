# Visit-based model (for P2 + P3 pathways) ------------------------------------

# Set up ----------------------------------------------------------------------

# Select bed-based scenarios
# AMY: This is same operation as visit_model, only different is "!" - could
# change to function for both
df_list <- list(list("bed_scenarios", scenarios),
                list("arr_rates_bed", arr_scenarios),
                list("costs_bed", costs))
for (x in df_list){
  assign(x[[1]], x[[2]] %>% filter(!str_detect(node, "P1")))
}

# # AMY: Same as visit model - just different dataframes + names
# # Also NIQ is dtoc, rather than inq and nctr
init_occ <- as.list(bed_scenarios$occ)
init_niq <- as.list(bed_scenarios$dtoc)
srv_dist<-as.list(bed_scenarios$los_dist)
cap <- as.list(bed_scenarios$capacity)
loss <- as.list(rep(0, nrow(bed_scenarios)))

# Parameters for sampling length of stay when distribution == rlnorm
# AMY: Same as visit_model, just change dataframe names
srv_params <- bed_scenarios %>%
  separate(los_params, into = c("mu", "sigma"), sep = ",", convert = TRUE) %>%
  select(mu, sigma) %>%
  unname() %>%
  t() %>%
  data.frame() %>%
  as.list()

# Parameters for sampling length of stay when distribution == norm
# AMY: Same as visit model, just different dataframe names
mean_los_bed <- as.list(bed_scenarios$mean_los)
sd_los_bed <-as.list(rep(sd_los, nrow(bed_scenarios)))

# Select arrivals, date and scenario, then pivot so each row is a date
# and arrivals on that date, with columns for each scenario
# AMY: Same as visit model, just different dataframe names
arr_rates <- arr_rates_bed %>%
  select(arrivals, date, S) %>%
  pivot_wider(names_from = S, values_from = arrivals) %>%
  arrange(date) %>%
  as.data.frame()

# Create vector with each scenario name (dput is just to print to screen)
bed_pathway_vector <- dput(colnames(arr_rates %>% select(-date)))

# Functions for sampling from LOS distribution
rtdist <- function(n, params) {
  do.call(paste0("r", node_srv_dist), c(list(n = n), params))
}
ptdist <- function(q, params) {
  do.call(paste0("p", node_srv_dist), c(list(q = q), params))
}
qtdist <-function(p, params) {
  do.call(paste0("q", node_srv_dist), c(list(p = p), params))
}

# Simulation function ----------------------------------------------------------
simfn <- function(runs) {
  # Set seed to number of current run (from 1:nruns)
  set.seed(runs)
  
  # Duration set to number of dates in arrivals
  DUR <- nrow(node_arr_rates)
  
  # Create dataframe to store information about stay for each patient
  cal <- data.frame(id = integer(),
                    time = numeric(),
                    event = character(),
                    wait = numeric())
  
  # Setup initial conditions
  if (node_init_occ>0) {
    # Get LOS for each person in pathway by sampling from rlnorm with LOS params
    # Sample for the number of people already in pathway (node_init_occ)
    # Mean and SD of LOS distribution given by node_srv_params
    init_serv_arr_times <- rtdist(n = node_init_occ, params = node_srv_params)
    
    # As they are already in system, want to make LOS shorter, so sample from
    # uniform distribution between 1 and the LOS just created
    init_serv_end_times <- sapply(init_serv_arr_times,
                                  function(x) runif(n = 1, min = 1, max = x))
    
    # Add patients to cal
    cal <- rbind(cal, data.frame(id = 1:node_init_occ,
                                 time = init_serv_end_times,
                                 event = "endsrv",
                                 wait = NA))
  }
  
  # Select column that is not date (can't use name as different for each
  # scenario and location
  arr_col_ind <- -which(names(node_arr_rates) %in% c("date"))
  
  # For each date, get number of arrivals by sampling from poisson distribution,
  # and round to nearest integer
  day_arr_times <- sapply(1:nrow(node_arr_rates), function(x)
    round(rpois(n = 1,
                lambda = node_arr_rates[x, arr_col_ind])))
  
  # Find proportion of days where arrivals < 0
  arr_neg <- sum(day_arr_times < 0) / length(day_arr_times)
  
  # If < 0, reset to 0
  day_arr_times[which(day_arr_times < 0)] <- 0
  
  # For each day of simulation, sample values between 0 and 1 from uniform
  # distribution. Number of samples is the number of arrivals that day (0+).
  # Adds the day (x) - 1 to that calculation - so arrivals for day 1 are
  # between 0 and 1, arrivals for day 19 are between 18 and 19, etc.
  arr_times <- unlist(sapply(1:length(day_arr_times), function(x) {
    sort(runif(n = day_arr_times[x],
               min = 0,
               max = 1) + x - 1)
  }))
  
  # Add patient details to cal
  cal <- rbind(cal, data.frame(
    id = (node_init_occ + 1):(node_init_occ + length(arr_times)),
    time = arr_times,
    event = "arrival",
    wait = NA))
  
  # Create tx, which will hold the time
  tx <- 0
  
  # Create res
  # AMY: What is this?
  # AMY: Above is all for 99 days, not 100 days. Does visits based have day 0?
  # I previously thought this might be fine
  res <- data.frame(time = 0:(DUR),
                    occ = NA,
                    niq = NA,
                    arr_admit = 0,
                    arr_no_admit = 0,
                    mean_wait = 0)
  
  niq <- node_init_niq # number in queue
  occ <- node_init_occ # occupancy (number in unit)
  res$niq[1] <- niq
  res$occ[1] <- occ

  while (tx <= (DUR) & nrow(cal) > 0) {
    # Indices of arrival or endsrv events with time > tx, then find index
    # of event with minimum of those times
    ind1 <- which(cal$time > tx & cal$event %in% c("arrival", "endsrv"))
    ind <- ind1[which.min(cal$time[ind1])]

    # Keep record of niq, occ and tx
    # AMY: WHY?
    niq_old <- niq
    occ_old <- occ
    tx_old <- tx

    # Set tx to the minimum time
    tx <- cal$time[ind]
    
    # Break loop if time is greater than simulation duration or nrow(cal)=0
    if (tx > (DUR) | nrow(cal) == 0)
      break
    
    # Finds day by rounding up (i.e. 3.0 to 3.9999 would be day 4)
    # 3.0 would go to day 4, but as we sample arrival from 0 to 1, we wouldn't
    # necessarily be sure it is day 3 or day 4, so this is an assumption
    tx_day <- ceiling(tx)

    # For arrivals...
    if (cal$event[ind] == "arrival") {
      # If number occupying pathway is less than capacity...
      if (occ < node_cap) {
        # Admit patient
        res$arr_admit[tx_day] <- res$arr_admit[tx_day] + 1
        cal <- rbind(cal,  data.frame(id = cal$id[ind],
                                      time = tx,
                                      event = "startsrv",
                                      wait = NA))
        los <- do.call(paste0("r", node_srv_dist), c(list(n = 1), node_srv_params))
        cal <- rbind(cal, data.frame(id = cal$id[ind],
                                     time = tx + los,
                                     event = "endsrv",
                                     wait = NA))
        occ <- occ + 1

      # If no capacity...
      } else {
        res$arr_no_admit[tx_day] <- res$arr_no_admit[tx_day] + 1
        if (node_loss == FALSE) {
          # Patient wait in queue
          niq <- niq + 1
        } else {
          cal <- cal[-which(cal$id == cal$id[ind]), ]
        }
      }

    # For patients leaving...
    } else if (cal$event[ind] == "endsrv") {
      cal <- cal[-which(cal$id == cal$id[ind]), ]
      if (niq == 0 || occ > node_cap) {
        occ <- occ - 1
      } else {
        # Admit patient (backfill bed)
        los <- do.call(paste0("r", node_srv_dist), c(list(n = 1), node_srv_params))
        # Select patient who's been waiting longest (and has not started/finished service)
        #poss_ids<-setdiff(unique(cal$id),cal$id[which(cal$event=="startsrv")])
        poss_ids3 <-
          setdiff(unique(cal$id), cal$id[which(cal$event == "endsrv")])
        if (length(poss_ids3) > 0) {
          #poss_ids2<-setdiff(unique(cal$id),cal$id[which(cal$event=="startsrv"||cal$event=="endsrv")])
          waits <-
            data.frame(id = poss_ids3, waits = cal$time[which((cal$id %in% poss_ids3) &
                                                                (cal$event == "arrival"))] - tx)
          #waits<-data.frame(id=poss_ids3,waits=cal$time[which((cal$id %in% poss_ids3))]-tx)
          admit_id <- waits$id[which.min(waits$waits)]
          admit_wait <- waits$waits[which.min(waits$waits)]
          cal <- rbind(cal, data.frame(id = admit_id,
                                       time = tx,
                                       event = "startsrv",
                                       wait = admit_wait ))
          cal <- rbind(cal, data.frame(id = admit_id,
                                       time = tx + los,
                                       event = "endsrv",
                                       wait = admit_wait))
          niq <- niq - 1
        }
      }
    }
    cal <- cal[order(cal$time), ]
    #save results, extract performance measures
    wt_new <- (tx - tx_old) / tx
    res$niq[tx_day] <-
      ifelse(
        is.na(res$niq[tx_day]),
        (tx - floor(tx)) * niq_old + (ceiling(tx) - tx) * niq,
        wt_new * niq + (1 - wt_new) * res$niq[tx_day]
      )
    res$occ[tx_day] <-
      ifelse(
        is.na(res$occ[tx_day]),
        (tx - floor(tx)) * occ_old + (ceiling(tx) - tx) * occ,
        wt_new * occ + (1 - wt_new) * res$occ[tx_day]
      )
    res$mean_wait[tx_day] <- max(0, -mean(cal$wait, na.rm = TRUE))
  }

  res <- res %>%
    mutate(niq = ifelse(time == 1 & is.na(niq), 0, niq)) %>%
    mutate(occ = ifelse(time == 1 & is.na(occ), 0, occ)) %>%
    fill(niq) %>%
    fill(occ) %>%
    mutate(node = node, run = runs)
  res_arr_neg <- data.frame(node = node,
                            run = runs,
                            arr_neg = arr_neg)
  return(list(res, res_arr_neg))
}

# Run simulation ---------------------------------------------------------------
# Record time taken
start.time <- Sys.time()

# For each pathway main loop of results
RES <- lapply(1:(ncol(arr_rates) - 1), function(node) {
  # For that pathway and simulation, assign parameters (initial occupancy,
  # initial queue, arrival rates, LOS distribution parameters, capacity, loss
  # (balking condition))
  node_init_occ <- as.numeric(init_occ[[node]])
  node_init_niq <- as.numeric(init_niq[[node]])
  node_arr_rates <- arr_rates %>% select(date, bed_pathway_vector[node])
  node_srv_dist <- srv_dist[[node]]
  node_srv_params <- srv_params[[node]]
  node_cap <- cap[[node]]
  node_loss <- loss[[node]]
  
  # Intialisation for parallel processing
  # detectCores() but -1 as want to make you you have one left to do other
  # stuff on, then makecluster() to set the amount of clusters you want your
  # code to run on
  cl <- parallel::makeCluster(detectCores() - 1)

  # Create a cluster with all parameters needed for running simfn 
  clusterExport(cl = cl,
                varlist = c("node", "node_init_occ", "node_init_niq",
                            "node_arr_rates", "node_srv_dist",
                            "node_srv_params", "node_cap", "node_loss",
                            "rtdist", "ptdist", "qtdist"), 
                envir = environment())
  clusterEvalQ(cl = cl, c(library(tidyr), library(dplyr)))

  # Apply using parallel processing simfn for nruns time using information in cl
  tRES <- parLapply(cl, 1:nruns, simfn)
  stopCluster(cl)

  tRES1 <- do.call("bind_rows", lapply(1:length(tRES), function(x) tRES[[x]][[1]]))
  # tRES2 has cols with node (i.e. (1) pathway and scenario number, (2) run number, (3) arr_neg)

  tRES2 <- do.call("bind_rows", lapply(1:length(tRES), function(x) tRES[[x]][[2]]))

  return(list(tRES1, tRES2))
})

RES1 <- do.call("bind_rows", lapply(1:length(RES), function(x) RES[[x]][[1]]))
RES2 <- do.call("bind_rows", lapply(1:length(RES), function(x) RES[[x]][[2]]))

RES1$mean_wait[is.nan(RES1$mean_wait)] <- 0

# Print processing time
print(difftime(Sys.time(), start.time), quote = FALSE)

# Find average results for report ----------------------------------------------

RES1q <- RES1 %>% 
  pivot_longer(
    cols = c(occ, niq, arr_admit, arr_no_admit, mean_wait),
    names_to = "measure",
    values_to = "value") %>%
  group_by(node, time, measure) %>% summarise(mean = mean(value, na.rm = TRUE))

cap_size <- as.numeric()
for (x in 1:length(RES1q$node)) {
  cap_size[x] <- cap[[RES1q$node[x]]]
}
RES1q <- cbind(RES1q, cap_size)
colnames(RES1q)[5] <- "capacity"

beds_required <- lapply(1:length(bed_pathway_vector),
                        function(x)
                          RES1q$mean[(RES1q$node == x &
                                        RES1q$measure == "occ" &
                                        RES1q$time < nrow(arr_rates))])

niq_result <- lapply(1:length(bed_pathway_vector),
                     function(x)
                       RES1q$mean[(RES1q$node == x &
                                     RES1q$measure == "niq" &
                                     RES1q$time < nrow(arr_rates))])

wait_result <- lapply(1:length(bed_pathway_vector),
                      function(x)
                        RES1q$mean[(RES1q$node == x &
                                      RES1q$measure == "mean_wait" &
                                      RES1q$time < nrow(arr_rates))])

# Create cost columns
p2_beds_req <- beds_required[1:36]
p3_beds_req <- beds_required[37:72]
p2_beds_cost <- lapply(p2_beds_req, "*", costs_bed[[2]][1])
p3_beds_cost <- lapply(p3_beds_req, "*", costs_bed[[2]][2])
niq_cost <- lapply(niq_result, "*", costs_bed[[3]][1])
beds_cost_comm <- c(p2_beds_cost, p3_beds_cost)
beds_cost <- mapply("+", niq_cost, beds_cost_comm, SIMPLIFY = FALSE)

# Create output dataframe
MeansOutput <-cbind(
    data.frame(arr_rates$date[1:length(arr_rates$date)]),
    data.frame(round(data.frame(beds_required))),
    data.frame(round(data.frame(niq_result))),
    data.frame(round(data.frame(wait_result))),
    data.frame(round(data.frame(beds_cost)))
)
colnames <- cbind(c('date',
                    (paste0(bed_pathway_vector, "__occ")),
                    (paste0(bed_pathway_vector, "__niq")),
                    (paste0(bed_pathway_vector, "__wait")),
                    (paste0(bed_pathway_vector, "__cost"))))
colnames(MeansOutput) <- colnames

# Save to excel, with filename based on the input file
write.csv(MeansOutput,
          paste0(
            "outputs/bed_output_using_",
            gsub(".xlsx", "", input_filename),
            ".csv"
          ),
          row.names = FALSE)
