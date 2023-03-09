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
  results <- foreach(run = 1:nruns, .combine = "rbind") %dopar% {
    set.seed(nruns * (SEED - 1) + run)
    
    # Output variables
    ent_sys <- 0 # number of entities that entered the system
    left_sys <- 0 # number of entities that left the system
    
    # Create necessary data structures
    # Captures output after warmup
    # CHANGE: Removed patients_initial and patients_inqueue, as they are
    # redundant as you add rows with npat which increments, and as you
    # ultimately just bind them with patients anyway
    output <- create_output_df(nrow = sim_length)
    patients <- create_patient_df(nrow = (sim_length + warmup) * 10)
    # Stores wait times for patients who leave system
    waittime_vec <- create_wait_df()
    # List with required visit vectors for each patient
    req_visits <- list()
    # Create resources, *10 to make it sufficiently large
    resources <- matrix(data = n_slots[z], nrow = (sim_length+warmup)*10)
    
    # Initialising counter for patients dataframe, plus ID and t (day)
    npat <- 0
    id <- 0
    t <- 1

    # Creating set of initial condition patients that are already in the
    # system at day 1 (i.e. patients already in P1)
    for (j in 1:visit_init_occ[[z]]) {
      # Increment ID and npat
      id <- id + 1
      npat <- npat + 1

      # Create temporary LOS using dis_los() (so it is longer)
      # Then get shorter LOS using dis_los2() which trims the templos
      # Then get end_slots (final number of visits) and init_slots (initial
      # number). Create sequence, then sample from tail for length of the
      # shorter LOS. This means patients already in system have a shorter LOS
      # and start from a later point that init_slots.
      # Then save vector of required visits
      # e.g.
      # temp vector: 4 4 4 3 3 3 3 2 2 2 2 2 1 1 1
      # final vector: 2 1 1 1
      templos <- dis_los()
      los <- dis_los2(templos)
      init_slots <- dis_init_slots()
      end_slots <- dis_end_slots()
      temp_visit_vector <- round(seq(from = init_slots,
                                     to = end_slots,
                                     length.out = templos))
      visit_vector <- tail(temp_visit_vector, los)
      req_visits[[id]] <- visit_vector

      # Save information about patient (arrival time is t, exit is FALSE)
      patients$id[npat] <- id
      patients$los[npat] <- los
      patients$arrival_time[npat] <- t
      patients$start_service[npat] <- NA
      patients$end_service[npat] <- NA
      patients$wait_time[npat] <- 0
      patients$exit[npat] <- FALSE
      
      # Run function, and replace patients df and resources with objects
      # from the function (as function couldn't output individual objects)
      resources_list <- check_resources(df = patients)
      patients <- resources_list[[1]]
      resources <- resources_list[[2]]
    }
    
    # Increment ent_sys
    ent_sys <- ent_sys + npat
    
    # Create set of initial conditions for patients already waiting to go
    # into P1
    for (j in 1:visit_init_q[[z]]) {
      # Run add_patient, then save to objects from that output list
      add_patient_output <- add_patient()
      id <- add_patient_output[[1]]
      npat <- add_patient_output[[2]]
      req_visits <- add_patient_output[[3]]
      patients <- add_patient_output[[4]]
      resources <- add_patient_output[[5]]
    }
    
    # CHANGE: ent_sys + npat makes ent_sys too large, as npat is not
    # resetting each time
    ent_sys <- npat
    
    # Simulation
    for (t in 1:(sim_length + warmup)) {
      # Sample from poisson distribution to get number of arrivals
      # t is the day, and z+1 is the appropriate pathway/location/scenario
      narr <- round(rpois(n = 1,
                          lambda = as.numeric(arr_rates_visit_p1[t, z + 1])))
      
      # If there are arrivals...
      if (narr > 0) {
        ent_sys <- ent_sys + narr
        
        # For each arrived patient
        for (j in 1:narr) {
          # Increment ID and npat
          id <- id + 1
          npat <- npat + 1
          
          # Find LOS and required visits
          los <- dis_los()
          init_slots <- dis_init_slots()
          end_slots <- dis_end_slots()
          visit_vector <- round(seq(from = init_slots,
                                    to = end_slots,
                                    length.out = los))
          req_visits[[id]] <- visit_vector
          
          # Save information to patients dataframe
          patients$id[npat] <- id
          patients$los[npat] <- los
          patients$arrival_time[npat] <- t
          patients$start_service[npat] <- NA
          patients$end_service[npat] <- NA
          patients$wait_time[npat] <- 0
          patients$exit[npat] <- FALSE
          
          # Run function, and replace patients df and resources with objects
          # from the function (as function couldn't output individual objects)
          resources_list <- check_resources(df = patients)
          patients <- resources_list[[1]]
          resources <- resources_list[[2]]
        }
      }
      
      # Find patients in queue, increment wait time column by one day
      in_q <- which((patients$start_service > t) & (patients$id > 0))
      if (length(in_q) > 0) {
        patients[in_q, "wait_time"] <- patients[in_q, "wait_time"] + 1
      }
      
      # Recording output from the day warm up period has finished
      if (t > warmup) {
        # CHANGE: Removed three if else() as they all had the same action
        output[t - warmup,] <- c(
          RUNX = run,
          node = visit_pathway_vector[z],
          day = t,
          q_length = length(in_q),
          n_slots_used = n_slots[z] - (resources[t, ]),
          patients_in_service = (n_slots[z] - (resources[t, ])) /
            (mean(c(ISR[z], endSR[z]))),
          res_used = 1 - (resources[t, ] / n_slots[z]),
          res_idle = resources[t, ] / n_slots[z],
          in_sys = (ent_sys - left_sys))
        
        # Remove patients whose service has ended from the patients table
        remove <- which(patients$end_service == t)
        if (length(remove) > 0) {
          if (t >= warmup) {
            df <- data.frame(
              RUNX = run,
              day_ = t,
              scen_ = visit_pathway_vector[z],
              start_service = patients$start_service[remove],
              waittime = patients[remove, 6])
            waittime_vec <- rbind(waittime_vec, df) #keeping waiting time
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
  
  # Extract results from above (contains results from each run)
  # CHANGE: Simplified, removed some hard coding
  # results[,1] contains "output"
  out <- do.call(rbind, results[, 1]) %>%
    mutate_at(c("n_slots_used", "patients_in_service", "res_used",
                "res_idle", "in_sys"), as.numeric) %>%
    mutate_at(c("RUNX", "day", "q_length"), as.integer)
  
  # results[,2] contains "resources"
  res <- do.call(cbind, results[, 2])
  colnames(res) <- c(1:nruns)
  
  # results[,3] contains "waittimes"
  wait <- do.call(rbind, results[, 3])
  
  # Create dataframe for summary information
  summary <- create_summary_df(nruns)
  
  # Summary of all runs
  # CHANGE: Moved out of loop as didn't need to be in it as same each time
  summary$LOS <- 1 / visit_param_dist[[z]]
  summary$ISR <- ISR[z]
  summary$nruns <- nruns
  summary$sim_length <- sim_length
  summary$warm_up <- warmup
  summary$capacity <- n_slots[z]
    
  for (k in 1:nruns) {
    # Extract results for that run
    r.out <- which(out[, "RUNX"] == k)
    k.wait <- which(wait[, "RUNX"] == k)
    # Add results for that run
    # AMY: repetitive round(mean())
    summary[k, "mean_wait"] <- round(mean(wait$waittime[k.wait]), 2)
    summary[k, "q_length"] <- round(mean(out$q_length[r.out]), 2)
    summary[k, "res_used"] <- round(mean(out$res_used[r.out]), 2)
    summary[k, "res_idle"] <- round(mean(out$res_idle[r.out]), 2)
    summary[k, "in_sys"] <- round(mean(out$in_sys[r.out]), 2)
  }
  
  # CHANGE: Moved out of loop as didn't need to be in it as same each time
  # Groups by day (e.g. day 1) and node (e.g. P1_B_BCap_Blos_Barr)
  # Finds average results for each day
  ts_output <- out %>%
    group_by(day, node) %>%
    summarise(
      niq = mean(q_length),
      in_sys = mean(in_sys),
      n_slots_used = mean(n_slots_used),
      occ = mean(patients_in_service),
      mean_res_idle = mean(res_idle),
      mean_res_used = mean(res_used)
    ) %>%
    ungroup()
  
  # Create cost columns
  # CHANGE: Use unlist() as clearer than [[1]]
  # Extract first two parts of the scenario (e.g. "P1_LocB"
  # dropping "BCap_Bloc_BArr")
  loc <- sapply(ts_output$node, function(x)
    paste(unlist(str_split(x, "_"))[1:2], collapse="_"))
  
  # AMY:is there a simpler way of changing that scenario column
  # AMY: is cbind() to convert from tibble to dataframe? what is purpose?
  ts_output$node <- loc
  ts_output <- left_join(ts_output, costs_visit, by = "node") %>%
    mutate(cost = (niq * acute_dtoc) + (n_slots_used * community_cost))
  ts_output <- cbind(ts_output)
  
  #waits by day
  ts_waits <- wait %>%
    group_by(day_, scen_) %>%
    summarise(wait = mean(waittime)) %>%
    ungroup()
  
  # For each scenario:
  ts_output <- cbind(ts_output, ts_waits)
  
  # Rowbind each scenario
  visits_based_output <- rbind(visits_based_output, ts_output)
}

# Extract and pivot results for NIQ, OCC, wait and costs
ptvisits_niq <- visits_based_output %>%
  select(day, scen_, niq) %>%
  pivot_wider(names_from = scen_, values_from = niq) %>%
  select(-day)
ptvisits_occ <- visits_based_output %>%
  select(day, scen_, occ) %>%
  pivot_wider(names_from = scen_, values_from = occ) %>%
  select(-day)
ptvisits_wait <- visits_based_output %>%
  select(day, scen_, wait) %>%
  pivot_wider(names_from = scen_, values_from = wait) %>%
  select(-day)
ptvisits_costs <- visits_based_output %>%
  select(day, scen_, cost) %>%
  pivot_wider(names_from = scen_, values_from = cost) %>%
  select(-day)

# Combine those dataframes, along with the dates
MeansOutput_v <- cbind(
  data.frame(arr_rates_visit_p1$date),
  data.frame(data.frame(ptvisits_niq)),
  data.frame(data.frame(ptvisits_occ)),
  data.frame(data.frame(ptvisits_wait)),
  data.frame(data.frame(ptvisits_costs))
)

# Set column names
colnames_v <- cbind(c("date",
                      paste0(visit_pathway_vector,"__niq"), 
                      paste0(visit_pathway_vector,"__occ"),
                      paste0(visit_pathway_vector, "__wait"),
                      paste0(visit_pathway_vector, "__cost")))
colnames(MeansOutput_v) <- colnames_v

# CHANGE: Save to csv, with filename based on the input file used rather
# than today's date
output_filename <- paste0("outputs/visit_output_using_",
                          gsub(".xlsx", "", input_filename),
                          ".csv")
write.csv(MeansOutput_v, output_filename, row.names = FALSE)