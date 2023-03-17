library(diffdf)
library(here)
library(lintr)

# Looks for differences between latest visit output, and output from 
# original version of code
visit_new <- read.csv(here("outputs", "visit_output_using_IPACS_20230214_fix.csv"))
visit_test <- read.csv(here("testing", "visit_testing_using_IPACS_20230214_fix.csv"))
diffdf::diffdf(visit_new, visit_test)

# Check for differences for bed based simulation
bed_new <- read.csv(here("outputs", "bed_output_using_IPACS_20230214_fix.csv"))
bed_test <- read.csv(here("testing", "bed_testing_using_IPACS_20230214_fix.csv"))
diffdf::diffdf(bed_new, bed_test)

# Linting
lint("ipacs_main_script.R")
lint(here("functions", "set_up.R"))
lint(here("functions", "visit_functions.R"))
lint(here("functions", "visit_model.R"))
lint(here("functions", "bed_model.R"))
lint("ipacs_produce_report.Rmd")