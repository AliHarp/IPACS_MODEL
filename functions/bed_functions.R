# Functions for bed-based model ------------------------------------------------

# Function for sampling from LOS distribution
rtdist <- function(n, params) {
  do.call(paste0("r", node_srv_dist), c(list(n = n), params))
}

# Simulation function
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
  # scenario and location)
  arr_col_ind <- -which(names(node_arr_rates) %in% c("date"))
  
  # For each date, get number of arrivals by sampling from poisson distribution,
  # and round to nearest integer, returning array (e.g. if 181 days, then list
  # of 181 numbers, each of which is the number of arrivals for that day)
  day_arr_times <- sapply(seq_len(nrow(node_arr_rates)), function(x) {
    round(rpois(n = 1, lambda = node_arr_rates[x, arr_col_ind]))
  })
  
  # If any day with negative arrivals, reset to 0
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
        los <- rtdist(n = 1, params = node_srv_params)
        
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
        los <- rtdist(n = 1, params = node_srv_params)
        
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
  
  return(res)
}

# Multiply occupancy by the costs provided by costs_bed
# This currently assumes that every location has same costs
find_costs <- function(oc, node, cost_type){
  # Inputs:
  # oc = list with occupancy
  # node = chosen pathway and location
  # cost_type = community_cost or acute_dtoc
  costs <- lapply(oc, "*",
                  as.double(costs_bed[costs_bed["node"] == node, cost_type]))
  return(costs)
}

# Find mean for given measure for each node for each day
# For length of pathway_vector_bed (which contains each location, pathway and
# scenario combination)....
# Go to each node number, and find mean of given metric, based on results from
# time 0 to 180 (not for time 181)
find_mean <- function(measure) {
  output_object <- lapply(seq_along(pathway_vector_bed), function(x) {
    res1mean$mean[(res1mean$node == x &
                     res1mean$measure == measure &
                     res1mean$time < nrow(arr_rates_bed))]
  })
  return(output_object)
}