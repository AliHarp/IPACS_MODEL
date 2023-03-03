# CHANGE: Changed to library (as stops if there is error, but require runs)
# CHANGE: Removed install.packages() as all should be present with renv
library(doSNOW)
library(foreach)
library(tidyverse, warn.conflicts=FALSE)
options(dplyr.summarise.inform=FALSE)
library(parallel)
library(readxl)
library(here)

# Will need to remove these as not reproducible
rm(list=ls())
wd <- setwd("~/Documents/IPACS_MODEL")

# CHANGE: Moved to top of file (as that's where you should put parameters you can change)
# Set model parameters (standard deviation for length of stay distribution,
# and number of runs for simulation)
# CHANGE: rename nruns_all as nruns. set as integer. therefore not needing nruns_p1 (nruns_p1 <- as.integer(nruns_all)) or nruns (nruns <- as.integer(nruns_all))
sd_los <- 3 #no info on sd - estimate provided
nruns <- as.integer(5)

# Import model parameters
# CHANGE: Input file now generated manually, so changed to manually input of filename
# CHANGE: Create list of (1) dataframes to create, and (2) sheets to import from
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
                                  sheet=x[2]))
}

# Set run time as the number of unique dates in arrivals (used in visit-based)
# CHANGE: was run_time then convert to integer to make sim_length - removed the middle step
sim_length <- as.integer(length(unique(arrivals_all$date)))

# CHANGE: made dplyr operations so it was clearer what was happening
# Would need further work to simplify further (is a bit repetitive still)
# Create scenarios
scenarios <- list(arrivals_all %>%
                    rename(sc_arr = scenario) %>%
                    distinct(node, sc_arr, .keep_all = TRUE),
                  capacity %>% rename(s_cap = scenario),
                  losA %>% rename(s_los = scenario),
                  init_conds) %>%
  reduce(merge, by="node", all=TRUE) %>%
  mutate(S = paste0(node, "_",  s_cap, "_", s_los, "_", sc_arr)) %>%
  pivot_wider(names_from = measure, values_from = value)

# Create arr_scenarios
arr_scenarios <- list(arrivals_all %>% rename(sc_arr = scenario),
                      capacity %>% rename(s_cap = scenario),
                      losA %>% rename(s_los = scenario),
                      init_conds) %>%
  reduce(merge, by="node", all=TRUE) %>%
  mutate(S = paste0(node, "_", s_cap, "_", s_los, "_", sc_arr)) %>%
  pivot_wider(names_from = measure, values_from = value)

# Timer (to record how submodel scripts take to run)
start.time<-Sys.time()

# Will need to change to running as functions rather than source()
source("Visit_based_submodel_script.R")
source("Bed_based_submodel_script.R")

# Print time taken for submodel scripts to run
print(difftime(Sys.time(),start.time),quote=FALSE)

# Produce word document report using RMarkdown
suppressWarnings({rmarkdown::render('rmdscript_ipacs_V3.Rmd',
                                    output_file=("outputs/IPACS Report"))})