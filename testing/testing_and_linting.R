library(diffdf)
library(here)
library(lintr)

# Clear workspace and set working directory
rm(list = ls())
wd <- setwd("~/Documents/IPACS_MODEL")

# Checking results are consistent ----------------------------------------------
# Looks for differences between latest visit output, and output from 
# original version of code
visit_new <- read.csv(here("outputs", "visit_output_using_IPACS_20230214_fix.csv"))
visit_test <- read.csv(here("testing", "visit_testing_using_IPACS_20230214_fix.csv"))
diffdf::diffdf(visit_new, visit_test)

# Looking for differences in stochastic output of visit model
stoch_visit_new <- read.csv(here(
  "stochastic_outputs", "stochastic_visit_output_using_IPACS_20230214_fix.csv"))
stoch_visit_test <- read.csv(here("testing", "stochastic_visit_testing_using_IPACS_20230214_fix.csv"))
diffdf::diffdf(stoch_visit_new, stoch_visit_test)

# Check for differences for bed based simulation
bed_new <- read.csv(here("outputs", "bed_output_using_IPACS_20230214_fix.csv"))
bed_test <- read.csv(here("testing", "bed_testing_using_IPACS_20230214_fix.csv"))
diffdf::diffdf(bed_new, bed_test)

# Looking for differences in stochastic output of bed model
stoch_bed_new <- read.csv(here(
  "stochastic_outputs", "stochastic_bed_output_using_IPACS_20230214_fix.csv"))
stoch_bed_test <- read.csv(here("testing", "stochastic_bed_testing_using_IPACS_20230214_fix.csv"))
diffdf::diffdf(stoch_bed_new, stoch_bed_test)

# Linting ---------------------------------------------------------------------
lint("ipacs_main_script.R")
lint(here("functions", "set_up.R"))
lint(here("functions", "visit_functions.R"))
lint(here("functions", "visit_model.R"))
lint(here("functions", "bed_functions.R"))
lint(here("functions", "bed_model.R"))
lint(here("functions", "ipacs_produce_report.Rmd"))
