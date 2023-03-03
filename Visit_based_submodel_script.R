# Visit-based model (for P1 pathways)

source(here("functions", "visit_functions.R"))

# Manual parameters
# CHANGE: Moved to top of file (as manually input)
# ISR/IVR = initial service/visit rate
# ESR/FVR = end service rate/final visit rate
# AMY: Need to standardise names (ISR/IVR) (ESR/FVR/endSR)
# AMY: change to snake_case
sd_ISR <- 0.5
sd_ESR <- 0.5
SEED <- 1
warmup <- 0

# Extract visit-based scenarios (P1) from imported dataframes
# CHANGE: To reduce repetitive operations
df_list <- list(list("visit_scenarios", scenarios),
                list("arr_scenarios_v", arr_scenarios),
                list("costs_visit", costs))
for (x in df_list){
  assign(x[[1]], x[[2]] %>% filter(str_detect(node, "P1")))
}

# Create lists containing parameters from dataframes
# AMY: requires more comments
visit_init_occ <- as.list(visit_scenarios$occ)
visit_init_q <- as.list(visit_scenarios$dtoc)
visit_srv_dist <- as.list(visit_scenarios$los_dist)
visit_cap <- as.integer(as.list(visit_scenarios$capacity))
visit_loss <- as.list(rep(0, nrow(visit_scenarios)))

# If you have a log-normal distribution for length of stay
# CHANGE: Simplified
# AMY: could change mu and sigma to mean and SD to be consistent with language
# AMY: could provide better column labels
visit_srv_params <- visit_scenarios %>%
  separate(los_params, into = c("mu", "sigma"), sep = ",", convert = TRUE) %>%
  select(mu, sigma) %>%
  unname() %>%
  t() %>%
  data.frame() %>%
  as.list()

# if norm, AMY: Provide more descriptive comment (once certain on what this
# means)
# AMY: Not sure why round down the mean length of stay to an integer
# (e.g. mean_los[85] 18.7 goes to 18)
visit_param_dist <- as.list(as.integer(visit_scenarios$mean_los))
# Repeat "sd_los" for the number of rows in "visit_scenarios"
visit_param_sd <- as.list(rep(sd_los, nrow(visit_scenarios)))

# Select arrivals, date and scenario, then pivot so each row is a date
# and arrivals on that date, with columns for each scenario
arr_rates_visit_p1 <- arr_scenarios_v %>%
  select(arrivals, date, S) %>%
  pivot_wider(names_from = S, values_from = arrivals) %>%
  arrange(date)

# Create vector with each scenario name
# CHANGE: no hard coding of data column
# AMY: dput prints to screen, without it is same object, presume that print is
# needed
visit_pathway_vector <- dput(colnames(arr_rates_visit_p1 %>% select(-date)))

# Set the minimum and maximum length of stay (LOS) and initial service rate
# (ISR)
# CHANGE: removed conversion to list as not required
# AMY: concerned that it is replacing value of objects rather than making new
# AMY: need to understand why these are min and max LOS and ISR (as title was from existing script)
# AMY: change to snake_case
ISR <- as.integer(visit_scenarios$IVR)
endSR <- as.integer(visit_scenarios$FVR)

# Create lists with sd_ISR or sd_ESR repeated for number of visit scenarios
# AMY: change to snake_case
sd_ISR <- as.double(rep(sd_ISR, nrow(visit_scenarios)))
sd_ESR <- as.double(rep(sd_ESR, nrow(visit_scenarios)))

# CHANGE: Removed n_patients (just made visit_cap as.integer() when created),
# and replaced n_patients with visit_cap below

# Create n_slots, the number of visit slots available per day
# Based on an average visit rate (as from mean of ISR and endSR)
# multiplied by the capacity for P1 (visit_cap)
n_slots  <- visit_cap * mean(c(ISR, endSR))

# CHANGE: create sim_length in model_script by setting to as.integer() there,
# removing need for run_time object

# CHANGE: create nruns as integer to begin with, and use here (instead of
# nruns_all and setting as integer for each model with different object names)

visits_based_output <- NULL

####runs##################################
#z represents the index for all scenarios through all visit-based pathways

for (z in 1:length(visit_pathway_vector)) {
  # AMY: This changed from my version where you detected cores - is this faster
  cl <- parallel::makeCluster(1)
  registerDoSNOW(cl)
  run <- 1
  
  # Use foreach() to repeat operation for each run
  RESULTS <- foreach(run = 1:nruns, .combine = "rbind") %dopar% {
    set.seed(nruns * (SEED - 1) + run)
    
    # Output variables
    ent_sys <- 0 # number of entities that entered the system
    left_sys <- 0 # number of entities that left the system
    
    # Output after warm up period
    output <- data.frame(RUNX = integer(), # Run number x
                         node = character(), # Scenario
                         day = integer(), # Output per day
                         q_length = integer(), # Number of patients in queue
                         n_slots_used = numeric(),
                         patients_in_service = numeric(),
                         res_used = numeric(), # Used slots
                         res_idle = numeric(), # Idle slots
                         in_sys = numeric()) # Number of patients in system
    # Create blank rows (n= sim_length)
    output[sim_length,] <- NA
    
    # Create necessary data structures
    patients_initial <- create_patient_df(nrow = visit_init_occ[[z]])
    patients_inqueue <- create_patient_df(nrow = visit_init_q[[z]])
    patients <- create_patient_df(nrow = (sim_length + warmup) * 2)
    
    # Initialising counter for patients dataframe
    npat <- 0
    
    # List with required visit vectors for each patient
    req_visits <- list()
    
    # Create resources - *10 to make it sufficiently large
    resources <- matrix(data = n_slots[z], nrow = (sim_length+warmup)*10)
    
    #vector for storing waiting time, kept for each patient who left the system
    waittime_vec <- data.frame(
      RUNX = integer(),
      day_ = integer(),
      scen_ = character(),
      start_service = integer(),
      waittime = integer(),
      stringsAsFactors = T
    )
    
    id <- 0
    t <- 1
    
    # Creating set of initial condition patients that are already in the
    # system at day 1 (i.e. patients already in P1)
    for (j in 1:visit_init_occ[[z]]) {
      id <- id + 1
      npat <- npat + 1
      los <- dis_los2()
      templos <- dis_los()
      arrival_time <- t
      exit <- FALSE
      
      # AMY: had suggested change for this but wasn't happy with it
      patients_initial$id[npat] <- id
      patients_initial$los[npat] <- los
      patients_initial$arrival_time[npat] <- arrival_time
      patients_initial$start_service[npat] <- NA
      patients_initial$end_service[npat] <- NA
      patients_initial$wait_time[npat] <- 0
      patients_initial$exit[npat] <- exit
      
      init_slots <- dis_init_slots()
      end_slots <- dis_end_slots()
      
      # Want people already in system have shorter LOS and start from later
      # than init_slots - hence templos and los. LOS from dislos2() is a
      # shorter version, so get those
      # Create sequence from init_slots (initial number of visits) to
      # end_slots (final number of visits) of length templos (larger LOS)
      # Then get tail of that sequence, length of LOS (shorter LOS)
      # e.g. temp vector: 4 4 4 3 3 3 3 2 2 2 2 2 1 1 1
      # final vector : 2 1 1 1
      temp_visit_vector <- round(seq(from = init_slots,
                                     to = end_slots,
                                     length.out = templos))
      visit_vector <- tail(temp_visit_vector, los)
      
      req_visits[[id]] <- visit_vector
      
      #planning service, checking resources
      #temporary t for incrementing when no resources available
      tt <- t 
      
      while (is.na(patients_initial$start_service[npat]) == TRUE) {
        if (all((resources[((tt):((tt) + patients_initial$los[npat] - 1)), ] >= req_visits[[id]]) ==
                TRUE)) {
          patients_initial$start_service[npat] <- tt
          patients_initial$end_service[npat] <-
            patients_initial$start_service[npat] + (patients_initial$los[npat] - 1)
          
          #decrease capacity
          resources[((tt):((tt) + patients_initial$los[npat] - 1)), ] <-
            resources[((tt):((tt) + patients_initial$los[npat] - 1)), ] - req_visits[[id]]
        } else {
          tt <-
            tt + 1 #if no sufficient resources, check for starting on the next day
        }
      }
    }
    ent_sys <- ent_sys + npat
    
    #creating set of initial condition patients that are already in the system at day 1.
    for (j in 1:visit_init_q[[z]]) {
      id <- id + 1
      npat <- npat + 1
      los <- dis_los()
      arrival_time <- t
      exit <- FALSE
      patients_inqueue[npat,] <-
        c(id, los, arrival_time, NA, NA, 0, exit)
      
      #initial slots and creating required visits vector
      init_slots <- dis_init_slots()
      end_slots <- dis_end_slots()
      visit_vector <-
        round(seq(init_slots, end_slots, length.out = los)) #full visit seq
      req_visits[[id]] <- visit_vector
      
      #planning service, checking resources
      tt <-
        t #temporary t for incrementing when no resources available
      while (is.na(patients_inqueue$start_service[npat]) == TRUE) {
        if (all((resources[((tt):((tt) + patients_inqueue$los[npat] - 1)), ] >= req_visits[[id]]) ==
                TRUE)) {
          patients_inqueue$start_service[npat] <- tt
          patients_inqueue$end_service[npat] <-
            patients_inqueue$start_service[npat] + (patients_inqueue$los[npat] - 1)
          
          #decrease capacity
          resources[((tt):((tt) + patients_inqueue$los[npat] - 1)), ] <-
            resources[((tt):((tt) + patients_inqueue$los[npat] - 1)), ] - req_visits[[id]]
        } else {
          tt <-
            tt + 1 #if no sufficient resources, check for starting on the next day
        }
      }
    }
    patients <- rbind(patients_initial, patients_inqueue, patients)
    ent_sys <- ent_sys + npat
    
    #####simulation#####
    #  id<-0
    # t<-2
    for (t in 1:(sim_length + warmup)) {
      #arrivals to service
      narr <- round(rpois(1, arr_rates_visit_p1[t, z + 1]))
      if (narr > 0) {
        ent_sys <- ent_sys + narr
        
        #for each arrived patient
        for (j in 1:narr) {
          id <- id + 1
          npat <- npat + 1
          los <- dis_los()
          arrival_time <- t
          exit <- FALSE
          patients[npat,] <- c(id, los, arrival_time, NA, NA, 0, exit)
          
          #initial slots and creating required visits vector
          init_slots <- dis_init_slots()
          end_slots <- dis_end_slots()
          visit_vector <-
            round(seq(init_slots, end_slots, length.out = los))
          
          req_visits[[id]] <- visit_vector
          
          #planning service, checking resources
          tt <-
            t #temporary t for incrementing when no resources available
          
          while (is.na(patients$start_service[npat]) == TRUE) {
            print((tt):((tt) + patients$los[npat] - 1))
            if (all((resources[((tt):((tt) + patients$los[npat] - 1)), ] >= req_visits[[id]]) ==
                    TRUE)) {
              patients$start_service[npat] <- tt
              patients$end_service[npat] <-
                patients$start_service[npat] + (patients$los[npat] - 1)
              
              #decrease capacity
              resources[((tt):((tt) + patients$los[npat] - 1)), ] <-
                resources[((tt):((tt) + patients$los[npat] - 1)), ] - req_visits[[id]]
            } else {
              tt <-
                tt + 1 #if no sufficient resources, check for starting on the next day
            }
          }
        }
      }
      
      in_q <- which((patients$start_service > t) & (patients$id > 0))
      if (length(in_q) > 0) {
        patients[in_q, 6] <- patients[in_q, 6] + 1
      }
      #recording output from the day warm up period has finished
      if (t > warmup) {
        #only start recording after the warm up period
        if (npat > 0 & nrow(waittime_vec) > 0) {
          output[t - warmup,] <- c(
            RUNX = run,
            node = visit_pathway_vector[z],
            day = t,
            q_length = length(in_q),
            n_slots_used = n_slots[z] - (resources[t, ]),
            patients_in_service = (n_slots[z] - (resources[t, ])) /
              (mean(c(
                ISR[z], endSR[z]
              ))),
            res_used = 1 - (resources[t, ] / n_slots[z]),
            res_idle = resources[t, ] / n_slots[z],
            in_sys = (ent_sys - left_sys)
          )
          
        } else if (npat > 0 & nrow(waittime_vec) == 0) {
          output[t - warmup,] <- c(
            RUNX = run,
            node = visit_pathway_vector[z],
            day = t,
            q_length = length(in_q),
            n_slots_used = n_slots[z] - (resources[t, ]),
            patients_in_service = (n_slots[z] - (resources[t, ])) /
              (mean(c(
                ISR[z], endSR[z]
              ))),
            res_used = 1 - (resources[t, ] / n_slots[z]),
            res_idle = resources[t, ] / n_slots[z],
            in_sys = (ent_sys - left_sys)
          )
        } else {
          output[t - warmup,] <- c(
            RUNX = run,
            node = visit_pathway_vector[z],
            day = t,
            q_length = length(in_q),
            n_slots_used = n_slots[z] - (resources[t, ]),
            patients_in_service = (n_slots[z] - (resources[t, ])) /
              (mean(c(
                ISR[z], endSR[z]
              ))),
            res_used = 1 - (resources[t, ] / n_slots[z]),
            res_idle = resources[t, ] / n_slots[z],
            in_sys = (ent_sys - left_sys)
          )
        }
        
        #remove patients whose service has ended from the patients table
        remove <- which(patients$end_service == t)
        if (length(remove) > 0) {
          if (t >= warmup) {
            df <-
              data.frame(
                RUNX = run,
                day_ = t,
                scen_ = visit_pathway_vector[z],
                start_service = patients$start_service[remove],
                waittime = patients[remove, 6]
              )
            waittime_vec <-
              rbind(waittime_vec, df) #keeping waiting time
          }
          patients <- patients[-remove, ] #remove from patient list
          npat <- npat - length(remove)
          left_sys <- left_sys + length(remove)
        }
      }
    }
    list <- list(output, resources, waittime_vec)
    
    return(list)
  }
  stopCluster(cl)
  ###############################################
  
  #creating dataframe for summary info
  summary <- data.frame(
    LOS = integer(nruns),
    ISR = integer(nruns),
    nruns = integer(nruns),
    sim_length = integer(nruns),
    warm_up = integer(nruns),
    capacity = integer(nruns),
    mean_wait = numeric(nruns),
    q_length = numeric(nruns),
    res_used = numeric(nruns),
    res_idle = numeric(nruns),
    in_sys = numeric(nruns)
  )
  
  #splitting up RESULTS list in 3
  output <- RESULTS[, 1]
  out <- do.call(rbind, output)
  out[, c(5:9)] <- sapply(out[, c(5:9)], as.numeric)
  out[, c(1, 3:4)] <- sapply(out[, c(1, 3:4)], as.integer)
  #combining in one dataframe
  
  resources <- RESULTS[, 2]
  res <- do.call(cbind, resources)
  colnames(res) <- c(1:nruns)
  
  waittimes <- RESULTS[, 3]
  wait <- do.call(rbind, waittimes)
  
  #summary of all runs
  for (k in 1:nruns) {
    r.out <- which(out[, 1] == k)
    k.wait <- which(wait[, 1] == k)
    summary[k, ] <- c(
      LOS = 1 / visit_param_dist[[z]],
      ISR = ISR[z],
      nruns = nruns,
      sim_length = sim_length,
      warm_up = warmup,
      capacity = n_slots[z],
      mean_wait = round(mean(wait$waittime[k.wait]), 2),
      q_length = round(mean(out$q_length[r.out]), 2),
      res_used = round(mean(out$res_used[r.out]), 2),
      res_idle = round(mean(out$res_idle[r.out]), 2),
      in_sys = round(mean(out$in_sys[r.out]), 2)
    )
    
    #niq and occ by day (plus other measures if needed)
    ts_output <- out %>% group_by(day, node) %>%
      summarise(
        niq = mean(q_length),
        in_sys = mean(in_sys),
        n_slots_used = mean(n_slots_used),
        occ = mean(patients_in_service),
        mean_res_idle = mean(res_idle),
        mean_res_used = mean(res_used)
      ) %>%
      ungroup()
    
    #create cost columns
    loc = sapply(ts_output$node, function(x)
      paste(strsplit(x, "_")[[1]][1:2], collapse = '_'))
    ts_output$node <- loc
    ts_output <- left_join(ts_output, costs_visit, by = "node")
    ts_output <-
      ts_output %>% mutate(cost = (niq * acute_dtoc) + (n_slots_used * community_cost))
    ts_output <- cbind(ts_output)
    
    #waits by day
    ts_waits <- wait %>% group_by(day_, scen_) %>%
      summarise(wait = mean(waittime)) %>%
      ungroup()
    
    #for each scenario:
    ts_output <- cbind(ts_output, ts_waits)
  }
  #rowbind each scenario
  visits_based_output <- rbind(visits_based_output, ts_output)
  
}

ptvisits_niq <-
  visits_based_output[, c(1, 13, 3)] %>% pivot_wider(names_from = scen_, values_from =
                                                       niq)
ptvisits_niq <- subset(ptvisits_niq, select = -c(day))
ptvisits_occ <-
  visits_based_output[, c(1, 13, 6)] %>% pivot_wider(names_from = scen_, values_from =
                                                       occ)
ptvisits_occ <- subset(ptvisits_occ, select = -c(day))
ptvisits_wait <-
  visits_based_output[, c(1, 13, 14)] %>% pivot_wider(names_from = scen_, values_from =
                                                        wait)
ptvisits_wait <- subset(ptvisits_wait, select = -c(day))
ptvisits_costs <-
  visits_based_output[, c(1, 13, 11)] %>% pivot_wider(names_from = scen_, values_from =
                                                        cost)
ptvisits_costs <- subset(ptvisits_costs, select = -c(day))

colnames_v <-
  cbind(c('date', (paste0(
    visit_pathway_vector, "__niq"
  )),
  (paste0(
    visit_pathway_vector, "__occ"
  )),
  (paste0(
    visit_pathway_vector, "__wait"
  )),
  (paste0(
    visit_pathway_vector, "__cost"
  ))))
MeansOutput_v <-
  cbind(
    data.frame(arr_rates_visit_p1$date[1:length(arr_rates_visit_p1$date)]),
    data.frame(data.frame(ptvisits_niq)),
    data.frame(data.frame(ptvisits_occ)),
    data.frame(data.frame(ptvisits_wait)),
    data.frame(data.frame(ptvisits_costs))
  )
colnames(MeansOutput_v) <- colnames_v

# CHANGE: Save to csv, with filename based on the input file used rather than today's date
write.csv(MeansOutput_v,
          paste0(
            "outputs/visit_output_using_",
            gsub(".xlsx", "", input_filename),
            ".csv"
          ),
          row.names = FALSE)
