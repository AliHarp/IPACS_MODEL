# Visit-based model (for P1 pathways) -----------------------------------------

# Set up ----------------------------------------------------------------------

source(here("functions", "visit_functions.R"))

# Extract visit-based scenarios (P1) from imported dataframes
df_list <- list(list("visit_scenarios", scenarios),
                list("arr_scenarios_v", arr_scenarios),
                list("costs_visit", costs))
for (x in df_list){
  assign(x[[1]], x[[2]] %>% filter(str_detect(node, "P1")))
}

# Create lists containing parameters from dataframes
visit_init_occ <- as.list(visit_scenarios$occ)
visit_init_q <- as.list(visit_scenarios$dtoc)
visit_srv_dist <- as.list(visit_scenarios$los_dist)
visit_cap <- as.integer(as.list(visit_scenarios$capacity))
visit_loss <- as.list(rep(0, nrow(visit_scenarios)))

# Parameters for sampling length of stay when have a log-normal distribution
visit_srv_params <- visit_scenarios %>%
  separate(los_params, into = c("mu", "sigma"), sep = ",", convert = TRUE) %>%
  select(mu, sigma) %>%
  unname() %>%
  t() %>%
  data.frame() %>%
  as.list()

# Parameters for sampling length of stay when have a normal distribution
visit_param_dist <- as.list(visit_scenarios$mean_los)
visit_param_sd <- as.list(rep(sd_los, nrow(visit_scenarios)))

# Select arrivals, date and scenario, then pivot so each row is a date
# and arrivals on that date, with columns for each scenario
arr_rates_visit_p1 <- arr_scenarios_v %>%
  select(arrivals, date, S) %>%
  pivot_wider(names_from = S, values_from = arrivals) %>%
  arrange(date)

# Create vector with each scenario name (dput is just to print to screen)
visit_pathway_vector <- dput(colnames(arr_rates_visit_p1 %>% select(-date)))

# Initial service rate and end service rate, and their standard deviation
# Create lists with sd_isr or sd_esr repeated for number of visit scenarios
isr <- as.integer(visit_scenarios$IVR)
end_sr <- as.integer(visit_scenarios$FVR)
sd_isr <- as.double(rep(sd_isr, nrow(visit_scenarios)))
sd_esr <- as.double(rep(sd_esr, nrow(visit_scenarios)))

# Create n_slots, the number of visit slots available per day. This is based
# on an average visit rate (as from mean of isr and end_sr) multiplied by the
# capacity for P1 (visit_cap)
n_slots  <- visit_cap * mean(c(isr, end_sr))

# Create object to store outputs from each scenario
visits_based_output <- NULL


# Model -----------------------------------------------------------------------

# Repeat for each scenario in visit-based pathways
for (z in seq_along(visit_pathway_vector)) {
  # detectCores() but -1 as want to make you you have one left to do other
  # stuff on. Cores are processors that can work on tasks. Then makecluster()
  # to set the amount of clusters you want your code to run on
  cl <- parallel::makeCluster(detectCores() - 1)
  registerDoSNOW(cl)
  run <- 1

  # Use foreach() to repeat operation for each run
  results <- foreach(run = 1:nruns, .combine = "rbind") %dopar% {
    set.seed(nruns * (temp_seed - 1) + run)

    # Output variables
    ent_sys <- 0 # number of entities that entered the system
    left_sys <- 0 # number of entities that left the system

    # Create necessary data structures
    # Captures output after warmup
    output <- create_output_df(nrow = sim_length)
    patients <- create_patient_df(nrow = (sim_length + warmup) * 10)

    # Stores wait times for patients who leave system
    waittime_vec <- create_wait_df()

    # List with required visit vectors for each patient
    req_visits <- list()

    # Create resources, *10 to make it sufficiently large
    resources <- matrix(data = n_slots[z], nrow = (sim_length + warmup) * 10)

    # Initialising counter for patients dataframe, plus ID and t (day)
    npat <- 0
    id <- 0
    t <- 1

    ### Initial conditions (already in P1) ------------------------------------
    # For each patient...
    for (j in 1:visit_init_occ[[z]]) {
      add_patient_output <- add_patient(in_system = TRUE)
      id <- add_patient_output[[1]]
      npat <- add_patient_output[[2]]
      req_visits <- add_patient_output[[3]]
      patients <- add_patient_output[[4]]
      resources <- add_patient_output[[5]]
    }

    ### Initial conditions (waiting to go to P1) ------------------------------
    # For each patient...
    for (j in 1:visit_init_q[[z]]) {
      add_patient_output <- add_patient(in_system = FALSE)
      id <- add_patient_output[[1]]
      npat <- add_patient_output[[2]]
      req_visits <- add_patient_output[[3]]
      patients <- add_patient_output[[4]]
      resources <- add_patient_output[[5]]
    }

    # Set ent_sys as npat
    ent_sys <- npat

    ### Simulation ------------------------------------------------------------
    for (t in 1:(sim_length + warmup)) {
      # Sample from poisson distribution to get number of arrivals
      # t is the day, and z+1 is the appropriate pathway/location/scenario
      narr <- round(rpois(n = 1,
                          lambda = as.numeric(arr_rates_visit_p1[t, z + 1])))

      # If there are arrivals...
      if (narr > 0) {
        ent_sys <- ent_sys + narr

        # For each arrived patient...
        for (j in 1:narr) {
          add_patient_output <- add_patient(in_system = FALSE)
          id <- add_patient_output[[1]]
          npat <- add_patient_output[[2]]
          req_visits <- add_patient_output[[3]]
          patients <- add_patient_output[[4]]
          resources <- add_patient_output[[5]]
        }
      }

      # Find patients in queue, increment wait time column by one day
      in_q <- which((patients$start_service > t) & (patients$id > 0))
      if (length(in_q) > 0) {
        patients[in_q, "wait_time"] <- patients[in_q, "wait_time"] + 1
      }

      # Recording output from the day warm up period has finished
      if (t > warmup) {
        output[t - warmup, ] <- c(
          RUNX = run,
          node = visit_pathway_vector[z],
          day = t,
          q_length = length(in_q),
          n_slots_used = n_slots[z] - (resources[t, ]),
          patients_in_service = (n_slots[z] - (resources[t, ])) /
            (mean(c(isr[z], end_sr[z]))),
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

  # Extract results from above (contains results from each run)...

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

  # Create dataframe for summary information from each run
  summary <- create_summary_df(nruns)
  summary$LOS <- 1 / visit_param_dist[[z]]
  summary$ISR <- isr[z]
  summary$nruns <- nruns
  summary$sim_length <- sim_length
  summary$warm_up <- warmup
  summary$capacity <- n_slots[z]

  for (k in 1:nruns) {
    # Extract results for that run
    r_out <- which(out[, "RUNX"] == k)
    k_wait <- which(wait[, "RUNX"] == k)
    # Add results for that run
    # AMY: repetitive round(mean())
    summary[k, "mean_wait"] <- round(mean(wait$waittime[k_wait]), 2)
    summary[k, "q_length"] <- round(mean(out$q_length[r_out]), 2)
    summary[k, "res_used"] <- round(mean(out$res_used[r_out]), 2)
    summary[k, "res_idle"] <- round(mean(out$res_idle[r_out]), 2)
    summary[k, "in_sys"] <- round(mean(out$in_sys[r_out]), 2)
  }

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

  # Create node column by extracting first two parts of the scenario
  # (e.g. "P1_LocB", dropping "BCap_Bloc_BArr")
  ts_output$node <- sapply(ts_output$node, function(x) {
    paste(unlist(str_split(x, "_"))[1:2], collapse = "_")})

  # Create costs column
  ts_output <- left_join(ts_output, costs_visit, by = "node") %>%
    mutate(cost = (niq * acute_dtoc) + (n_slots_used * community_cost))

  # Waits by day
  ts_waits <- wait %>%
    group_by(day_, scen_) %>%
    summarise(wait = mean(waittime)) %>%
    ungroup()

  # Combine ts_output and ts_waits, and then save the scenario results
  ts_output <- cbind(ts_output, ts_waits)
  visits_based_output <- rbind(visits_based_output, ts_output)
}


# Save model results ----------------------------------------------------------

# Extract and pivot results for NIQ, OCC, wait and costs, then append together,
# along with the dates
outcomes <- c("niq", "occ", "wait", "cost")
ptvisits_list <- list(arr_rates_visit_p1["date"])
for (i in seq_along(outcomes)){
  ptvisits_list[[i + 1]] <- visits_based_output[c("day", "scen_",
                                                  outcomes[i])] %>%
    pivot_wider(names_from = "scen_", values_from = as.name(outcomes[i])) %>%
    select(-day) %>%
    setNames(paste0(names(.), "__", outcomes[i]))
}
means_output_v <- do.call("cbind", ptvisits_list)

# Save output to csv with output filename based on input filename
output_filename <- paste0("outputs/visit_output_using_",
                          gsub(".xlsx", "", input_filename),
                          ".csv")
write.csv(means_output_v, output_filename, row.names = FALSE)
