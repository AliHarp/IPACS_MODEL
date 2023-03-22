# Visit-based model (for P2 + P3 pathways) ------------------------------------

# Set up ----------------------------------------------------------------------

# Select bed-based scenarios
# AMY: This is same operation as visit_model, only different is "!" - could
# change to function for both
for (df in list("scenarios", "arr_scenarios", "costs")){
  df_name <- paste0(df, "_bed")
  assign(df_name, get(df) %>% filter(!str_detect(node, "P1")))
}

# AMY: Same as visit model - just different dataframes + names
# Also NIQ is dtoc, rather than inq and nctr
init_occ <- as.list(scenarios_bed$occ)
init_niq <- as.list(scenarios_bed$dtoc)
srv_dist <- as.list(scenarios_bed$los_dist)
cap <- as.list(scenarios_bed$capacity)
loss <- as.list(rep(0, nrow(scenarios_bed)))

# Parameters for sampling length of stay when distribution == rlnorm
# AMY: Same as visit_model, just change dataframe names
srv_params <- scenarios_bed %>%
  separate(los_params, into = c("mu", "sigma"), sep = ",", convert = TRUE) %>%
  select(mu, sigma) %>%
  unname() %>%
  t() %>%
  data.frame() %>%
  as.list()

# Parameters for sampling length of stay when distribution == norm
# AMY: Same as visit model, just different dataframe names
mean_los_bed <- as.list(scenarios_bed$mean_los)
sd_los_bed <- as.list(rep(sd_los, nrow(scenarios_bed)))

# Select arrivals, date and scenario, then pivot so each row is a date
# and arrivals on that date, with columns for each scenario
# AMY: Same as visit model, just different dataframe names
arr_rates <- arr_scenarios_bed %>%
  select(arrivals, date, S) %>%
  pivot_wider(names_from = S, values_from = arrivals) %>%
  arrange(date) %>%
  as.data.frame()

# Create vector with each scenario name (dput is just to print to screen)
bed_pathway_vector <- dput(colnames(arr_rates %>% select(-date)))

# Functions for sampling from LOS distribution
# AMY: Some not used (in clusterEXPORT but not scripts - so either change
# to be used in function or delete)
rtdist <- function(n, params) {
  do.call(paste0("r", node_srv_dist), c(list(n = n), params))
}
ptdist <- function(q, params) {
  do.call(paste0("p", node_srv_dist), c(list(q = q), params))
}
qtdist <- function(p, params) {
  do.call(paste0("q", node_srv_dist), c(list(p = p), params))
}

# Simulation function ----------------------------------------------------------
simfn <- function(runs) {
  # Set seed to number of current run (from 1:nruns)
  set.seed(runs)

  # Duration set to number of dates in arrivals
  dur <- nrow(node_arr_rates)

  # Create dataframe to store information about stay for each patient
  cal <- data.frame(id = integer(),
                    time = numeric(),
                    event = character(),
                    wait = numeric())

  # Setup initial conditions
  if (node_init_occ > 0) {
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
  day_arr_times <- sapply(seq_len(nrow(node_arr_rates)), function(x) {
    round(rpois(n = 1, lambda = node_arr_rates[x, arr_col_ind]))
  })

  # Find proportion of days where arrivals < 0
  arr_neg <- sum(day_arr_times < 0) / length(day_arr_times)

  # If < 0, reset to 0
  day_arr_times[which(day_arr_times < 0)] <- 0

  # For each day of simulation, sample values between 0 and 1 from uniform
  # distribution. Number of samples is the number of arrivals that day (0+).
  # Adds the day (x) - 1 to that calculation - so arrivals for day 1 are
  # between 0 and 1, arrivals for day 19 are between 18 and 19, etc.
  arr_times <- unlist(sapply(seq_along(day_arr_times), function(x) {
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
  # AMY: Above is all for 99 days, not 100 days. Does visits based have day 0?
  # I previously thought this might be fine
  res <- data.frame(time = 0:(dur),
                    occ = NA,
                    niq = NA,
                    arr_admit = 0,
                    arr_no_admit = 0,
                    mean_wait = 0)

  niq <- node_init_niq # number in queue
  occ <- node_init_occ # occupancy (number in unit)
  res$niq[1] <- niq
  res$occ[1] <- occ

  while (tx <= (dur) && nrow(cal) > 0) {
    # Indices of arrival or endsrv events with time > tx, then find index
    # of event with minimum of those times
    ind1 <- which(cal$time > tx & cal$event %in% c("arrival", "endsrv"))
    ind <- ind1[which.min(cal$time[ind1])]

    # Keep record of niq, occ and tx
    # AMY: Why?
    niq_old <- niq
    occ_old <- occ
    tx_old <- tx

    # Set tx to the minimum time
    tx <- cal$time[ind]

    # Break loop if time is greater than simulation duration or nrow(cal)=0
    if (tx > (dur) || nrow(cal) == 0)
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
        # AMY: I think tx_day 1 goes to res time 0 rather than 1?
        res$arr_admit[tx_day] <- res$arr_admit[tx_day] + 1

        # Add time when admitted to cal
        cal <- rbind(cal,  data.frame(id = cal$id[ind],
                                      time = tx,
                                      event = "startsrv",
                                      wait = NA))

        # Sample from rlnorm to get length of stay
        # AMY: Change to use function for this
        los <- do.call(paste0("r", node_srv_dist),
                       c(list(n = 1), node_srv_params))

        # Add time left service to cal
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
        los <- do.call(paste0("r", node_srv_dist),
                       c(list(n = 1), node_srv_params))
        # Select patient who's been waiting longest (and has not
        # started/finished service)
        poss_ids3 <- setdiff(unique(cal$id),
                             cal$id[which(cal$event == "endsrv")])
        if (length(poss_ids3) > 0) {
          waits <- data.frame(id = poss_ids3,
                              waits = cal$time[
                                which((cal$id %in% poss_ids3) &
                                        (cal$event == "arrival"))] - tx)
          admit_id <- waits$id[which.min(waits$waits)]
          admit_wait <- waits$waits[which.min(waits$waits)]
          cal <- rbind(cal, data.frame(id = admit_id,
                                       time = tx,
                                       event = "startsrv",
                                       wait = admit_wait))
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

  # res has columns: (1) time, (2) occ, (3) niq, (4) arr_admit,
  # (5) arr_no_admit, (6) mean_wait, (7) pathway and scenario number,
  # (8) run number
  res <- res %>%
    mutate(niq = ifelse(time == 1 & is.na(niq), 0, niq)) %>%
    mutate(occ = ifelse(time == 1 & is.na(occ), 0, occ)) %>%
    fill(niq) %>%
    fill(occ) %>%
    mutate(node = node, run = runs)

  # res_arr_neg has columns: (1) pathway and scenario number, (2) run number,
  # (3) arr_neg - proportion of days where arrivals < 0)
  res_arr_neg <- data.frame(node = node,
                            run = runs,
                            arr_neg = arr_neg)
  return(list(res, res_arr_neg))
}

# Run simulation ---------------------------------------------------------------
# Record time taken
start_time_bed <- Sys.time()

# For each pathway/location/scenario, run the simulation (nruns times)
# Saves results as sim_res, a list
sim_res <- lapply(1:(ncol(arr_rates) - 1), function(node) {
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
  tres <- parLapply(cl, 1:nruns, simfn)
  stopCluster(cl)

  # Bind together results from each run into one dataframe
  # For tres1 (res) and tres2 (res_arr_neg)
  tres1 <- do.call("bind_rows",
                   lapply(seq_along(tres), function(x) tres[[x]][[1]]))
  tres2 <- do.call("bind_rows",
                   lapply(seq_along(tres), function(x) tres[[x]][[2]]))

  return(list(tres1, tres2))
})

# Bind together results from each pathway and scenario
# For res1 (tres1/res) and res2 (tres2/res_arr_neg)
res1 <- do.call("bind_rows", lapply(seq_along(sim_res),
                                    function(x) sim_res[[x]][[1]]))
res2 <- do.call("bind_rows", lapply(seq_along(sim_res),
                                    function(x) sim_res[[x]][[2]]))

# Replace NA mean wait with 0
res1$mean_wait[is.nan(res1$mean_wait)] <- 0

# Print processing time
print(difftime(Sys.time(), start_time_bed), quote = FALSE)


# Find average results for report ----------------------------------------------
# Pivot dataframe so instead of columns for arr_admit, arr_no_admit, mean_wait,
# niq and occ, these are now rows (so from wide to long format)
res1q <- res1 %>%
  pivot_longer(
    cols = c(occ, niq, arr_admit, arr_no_admit, mean_wait),
    names_to = "measure",
    values_to = "value") %>%
  group_by(node, time, measure) %>%
  summarise(mean = mean(value, na.rm = TRUE))

# Add column to res1q with capacity, based on node number referencing to cap
cap_size <- as.numeric()
for (x in seq_along(res1q$node)) {
  cap_size[x] <- cap[[res1q$node[x]]]
}
res1q["capacity"] <- cap_size

# For length of bed_pathway_vector (which contains each location, pathway and
# scenario combination)....
# Go to each node number, and find mean of given metric, based on results from
# time 0 to 180 (not for time 181)
find_mean <- function(measure) {
  output_object <- lapply(seq_along(bed_pathway_vector), function(x) {
    res1q$mean[(res1q$node == x &
                  res1q$measure == measure &
                  res1q$time < nrow(arr_rates))]
  })
  return(output_object)
}
beds_required <- find_mean("occ")
niq_result <- find_mean("niq")
wait_result <- find_mean("mean_wait")

# Create cost columns ---------------------------------------------------------
# Seperate P2 and P3 occupancy (beds_req)
p2_beds_req <- beds_required[grep("P2", bed_pathway_vector)]
p3_beds_req <- beds_required[grep("P3", bed_pathway_vector)]

# Multiply occupancy by the costs provided by costs_bed
# This currently assumes that every location has same costs
p2_beds_cost <- lapply(p2_beds_req, "*", costs_bed %>%
                         filter(node == "P2_B") %>%
                         select(community_cost) %>%
                         as.double())
p3_beds_cost <- lapply(p3_beds_req, "*", costs_bed %>%
                         filter(node == "P3_B") %>%
                         select(community_cost) %>%
                         as.double())
niq_cost <- lapply(niq_result, "*", costs_bed %>%
                     filter(node == "P2_B") %>%
                     select(acute_dtoc) %>%
                     as.double())

# Combine P2 and P3 bed costs in a list
beds_cost_comm <- c(p2_beds_cost, p3_beds_cost)

# Add together NIQ costs and P2 and P3 bed costs
beds_cost <- mapply("+", niq_cost, beds_cost_comm, SIMPLIFY = FALSE)

# Save results in an output dataframe ------------------------------------------
# Create output dataframe with mean result for each measure for each
# pathway/scenario/location, with rows for each day (1-181)
meansoutput <- cbind(
    data.frame(arr_rates$date),
    data.frame(round(data.frame(beds_required))),
    data.frame(round(data.frame(niq_result))),
    data.frame(round(data.frame(wait_result))),
    data.frame(round(data.frame(beds_cost)))
)
colnames <- cbind(c("date",
                    (paste0(bed_pathway_vector, "__occ")),
                    (paste0(bed_pathway_vector, "__niq")),
                    (paste0(bed_pathway_vector, "__wait")),
                    (paste0(bed_pathway_vector, "__cost"))))
colnames(meansoutput) <- colnames

# Save to excel, with filename based on the input file
write.csv(meansoutput,
          paste0("outputs/bed_output_using_",
                 gsub(".xlsx", "", input_filename), ".csv"),
          row.names = FALSE)
