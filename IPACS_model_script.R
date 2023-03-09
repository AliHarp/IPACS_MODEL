library(doSNOW)
library(foreach)
library(tidyverse, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(parallel)
library(readxl)
library(here)

# Will need to remove these as not reproducible
rm(list = ls())
wd <- setwd("~/Documents/IPACS_MODEL")

# Set model parameters (standard deviation for length of stay distribution,
# and number of runs for simulation)
sd_los <- 3 #no info on sd - estimate provided
nruns <- as.integer(5)

# Create list of (1) dataframes to create, and (2) sheets to import from
# Then import each sheet and save to the relevant dataframe
# Use "here" package to create relative file path that works on all systems
# (i.e. no backslashes)
input_filename <- "IPACS_20230214_fix.xlsx"
input_list <- list(c("arrivals_all", "arrivals"),
                   c("init_conds", "initial conditions"),
                   c("capacity", "capacity"),
                   c("losA", "los"),
                   c("costs", "costs"))
for (x in input_list){
  assign(x[1], readxl::read_excel(here("model_inputs", input_filename),
                                  sheet = x[2]))
}

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

# Timer (to record how submodel scripts take to run)
# AMY: This gets replaced when you create start.time in bed_based I think
start_time <- Sys.time()

# Will need to change to running as functions rather than source()
source("Visit_based_submodel_script.R")
source("Bed_based_submodel_script.R")

# Print time taken for submodel scripts to run
print(difftime(Sys.time(), start_time), quote = FALSE)

# Produce word document report using RMarkdown
suppressWarnings({
  rmarkdown::render("rmdscript_ipacs_V3.Rmd",
                    output_file = "outputs/IPACS Report")
})
