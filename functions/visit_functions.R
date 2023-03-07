# Functions for visit

dis_los <- function() {
  # Samples from length of stay (LOS) distribution
  #
  # Calculates maximum LOS from mean and SD of LOS distribution.
  # If LOS is lnorm, sample from rlnorm distribution until you get a value
  # that is >=0 and <= the max LOS. Uses do.call() as we need to input
  # mean and SD for rlnorm() from a list
  #
  # AMY: These were default inputs but it was causing problems
  # los_dist - The distribution type (e.g. lnorm)
  # los_lnorm_mean_sd - The mean and SD for the lnorm distribution
  # los_norm_mean - The mean for the other distribution type
  # los_norm_sd - The SD for the other distribution type
  # AMY: Was that always norm?
  
  los_dist <- visit_srv_dist[[z]]
  los_lnorm_mean_sd <- visit_srv_params[[z]]
  los_norm_mean <- visit_param_dist[[z]]
  los_norm_sd <- visit_param_sd[[z]]
  
  max_los <- los_norm_mean + los_norm_sd * 3
  
  if (los_dist == "lnorm") {
    x <- round(do.call(rlnorm, c(list(1, los_lnorm_mean_sd))))
    while (x <= 0 | x >= max_los) {
      x <- round(do.call(rlnorm, c(list(1, los_lnorm_mean_sd))))
    }
  }
  else {
    x <- round(do.call(paste0("r", los_dist),
                       c(list(1, los_norm_mean, los_norm_sd)
                       )))
    while (x <= 0 | x >= max_los) {
      x <- round(do.call(paste0("r", los_dist),
                         c(list(1, los_norm_mean, los_norm_sd)
                         )))
    }
  }
  return(as.integer(x))
}


dis_los2 <- function() {
  # Samples from length of stay distribution, then samples between 1 and
  # that value, to reduce length of stay, for patients already in service
  sample_los <- dis_los()
  x <- round(runif(n = 1, min = 1, max = sample_los))
  return(as.integer(x))
}


dis_init_slots <- function() {
  # ISR (initial service rate)/IVR (initial visit rate) distribution
  # ISR is normal distribution with mean and SD specified
  # Sample from normal distribution using those values
  # Cannot be (a) less than 0, (b) more than mean+SD*3, or (c) more than n_slots
  x <- round(rnorm(n = 1, mean = ISR[z], sd = sd_ISR[z]))
  max_v <- ISR[z] + (sd_ISR[z] * 3)
  while (x <= 0 | x > max_v | x > n_slots[z]) {
    x <- round(rnorm(n = 1, mean = ISR[z], sd = sd_ISR[z]))
  }
  return(x)
}


dis_end_slots <- function() {
  # Sample from normal distribution for end visit rate
  # Using the appropriate mean and sd
  # Can't be less than 0 or max than mean+SD*3
  x <- round(rnorm(1, mean = endSR[z], sd = sd_ESR[z]))
  max_v <- endSR[z] + (sd_ESR[z] * 3)
  while (x <= 0 | x > max_v) {
    x <- round(rnorm(1, mean = endSR[z], sd = sd_ESR[z]))
  }
  return(x)
}


create_patient_df <- function(nrow){
  # Create blank dataframe for patients in visit simulation
  # Input: nrow
  df <- data.frame(
    id = integer(), # Patient ID
    los = integer(), # Length of stay
    arrival_time = integer(), # Day in the simulation the entity arrived
    start_service = integer(), # Day actual service started
    end_service = integer(), # Day service ended
    wait_time = integer(), # Number of days spent in the queue
    exit = logical(), # Boolean variable, TRUE if the entity has left the system
    stringsAsFactors = FALSE)
  df[nrow,] <- NA
  return(df)
}


create_output_df <- function(nrow){
  # Create blank dataframe for output for visit simulation after warm-up
  df <- data.frame(
    RUNX = integer(), # Run number x
    node = character(), # Scenario
    day = integer(), # Output per day
    q_length = integer(), # Number of patients in queue
    n_slots_used = numeric(),
    patients_in_service = numeric(),
    res_used = numeric(), # Used slots
    res_idle = numeric(), # Idle slots
    in_sys = numeric()) # Number of patients in system
  df[nrow,] <- NA
  return(df)
}


create_wait_df <- function(){
  # Create blank dataframe to store wait times
  df <- data.frame(
    RUNX = integer(),
    day_ = integer(),
    scen_ = character(),
    start_service = integer(),
    waittime = integer(),
    stringsAsFactors = T
  )
  return(df)
}


# # Save information to patients dataframe
# save_patient_info <- function(df, npat, id, los, arrival_time,
#                               start_service, end_service, wait_time,
#                               exit){
#   df$id[npat] <- id
#   df$los[npat] <- los
#   df$arrival_time[npat] <- arrival_time
#   df$start_service[npat] <- start_service
#   df$end_service[npat] <- end_service
#   df$wait_time[npat] <- wait_time
#   df$exit[npat] <- exit
#   return(df)
# }