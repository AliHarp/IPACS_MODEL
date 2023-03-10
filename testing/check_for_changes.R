library(diffdf)
library(here)
library(lintr)

# Looks for differences between latest visit output, and output from 
# original version of code
visit_new <- read.csv(here("outputs", "visit_output_using_IPACS_20230214_fix.csv"))
visit_test <- read.csv(here("testing", "visit_testing_using_IPACS_20230214_fix.csv"))
diffdf::diffdf(visit_new, visit_test)

# No longer check using copy from original code, due to bug fix
# Hence renamed visit_testing_prior_to_bug_fix_for_IPACS_20230214_fix.csv

# Linting
lint("ipacs_main_script.R")
lint(here("functions", "set_up.R"))
lint(here("functions", "visit_functions.R"))
lint(here("functions", "visit_model.R"))
lint(here("functions", "bed_model.R"))
lint("ipacs_produce_report.Rmd")