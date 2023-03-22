# Set up ----------------------------------------------------------------------
# Sim_length, scenarios and arr_scenarios are used for visit based and
# bed based simulation, so set-up in a separate script

# Create list of (1) dataframes to create, and (2) sheets to import from
# Then import each sheet and save to the relevant dataframe
input_list <- list(c("arrivals_all", "arrivals"),
                   c("init_conds", "initial conditions"),
                   c("capacity", "capacity"),
                   c("losA", "los"),
                   c("costs", "costs"))
for (x in input_list){
  assign(x[1], readxl::read_excel(here("model_inputs", input_filename),
                                  sheet = x[2]))
}

# Create mu, sigma and los_params columns in Python rather than excel formula
# after filter dataframe (to remove those columns if already exist)
losA[, -which(names(losA) %in% c("mu", "sigma", "los_params"))]
losA["mu"] <- log(losA["median"])
losA["sigma"] <- sqrt(2*(log(losA["mean_los"])-losA["mu"]))
losA["los_params"] <- with(losA, paste(mu, sigma, sep=" , "))

# Set run time as the number of unique dates in arrivals (used in visit-based)
sim_length <- as.integer(length(unique(arrivals_all$date)))

# Create scenarios
scenarios <- list(arrivals_all %>%
                    rename(sc_arr = scenario) %>%
                    distinct(node, sc_arr, .keep_all = TRUE),
                  capacity %>% rename(s_cap = scenario),
                  losA %>% rename(s_los = scenario),
                  init_conds) %>%
  reduce(merge, by = "node", all = TRUE) %>%
  mutate(S = paste0(node, "_",  s_cap, "_", s_los, "_", sc_arr)) %>%
  pivot_wider(names_from = measure, values_from = value)

# Create arr_scenarios
arr_scenarios <- list(arrivals_all %>% rename(sc_arr = scenario),
                      capacity %>% rename(s_cap = scenario),
                      losA %>% rename(s_los = scenario),
                      init_conds) %>%
  reduce(merge, by = "node", all = TRUE) %>%
  mutate(S = paste0(node, "_", s_cap, "_", s_los, "_", sc_arr)) %>%
  pivot_wider(names_from = measure, values_from = value)


# Function for setting up objects for both simulations ------------------------

setup_all <- function(model_type){
  # Set up for the simulation
  # Input: model - either "visit" or "bed"
  
  # Set parameters depending on model type
  if (model_type == "visit") {
    pathway <- "P1"
  } else if (model_type == "bed") {
    pathway <- c("P2|P3")
  } else {
    warning("Function input should be 'visit' or 'bed'")
  }
  
  # Extract appropriate rows from scenarios, arr_scenarios and costs
  scenarios <- scenarios %>% filter(str_detect(node, pathway))
  arr_scenarios <- arr_scenarios %>% filter(str_detect(node, pathway))
  costs <- costs %>% filter(str_detect(node, pathway))
  
  # Extract parameters from scenarios dataframe
  init_occ <- as.list(scenarios$occ)
  init_niq <- as.list(scenarios$dtoc)
  srv_dist <- as.list(scenarios$los_dist)
  cap <- as.integer(as.list(scenarios$capacity))
  loss <- as.list(rep(0, nrow(scenarios)))
  
  # Parameters for sampling length of stay when have a log-normal distribution
  srv_params <- scenarios %>%
    separate(los_params, into = c("mu", "sigma"), sep = ",", convert = TRUE) %>%
    select(mu, sigma) %>%
    unname() %>%
    t() %>%
    data.frame() %>%
    as.list()
  
  # Parameters for sampling length of stay when have a normal distribution
  mean_los <- as.list(scenarios$mean_los)
  sd_los <- as.list(rep(sd_los, nrow(scenarios)))
  
  # Select arrivals, date and scenario, then pivot so each row is a date
  # and arrivals on that date, with columns for each scenario
  arr_rates <- arr_scenarios %>%
    select(arrivals, date, S) %>%
    pivot_wider(names_from = S, values_from = arrivals) %>%
    arrange(date) %>%
    as.data.frame()
  
  # Create vector with each scenario name (dput is just to print to screen)
  pathway_vector <- dput(colnames(arr_rates %>% select(-date)))
  
  # Create list of names objects to return, use mget() to get the objects,
  # and create list with names specific to the model type
  return_names <- c("scenarios", "costs",
                    "init_occ", "init_niq", "srv_dist", "cap", "loss",
                    "srv_params", "mean_los", "sd_los",
                    "arr_rates", "pathway_vector")
  return_objects <- mget(return_names)
  return_new_names <- paste0(return_names, "_", model_type)
  return(list(return_new_names, return_objects))
}


