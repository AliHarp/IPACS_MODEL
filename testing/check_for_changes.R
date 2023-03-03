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
lint("IPACS_model_script.R")
lint("Visit_based_submodel_script.R")
lint("Bed_based_submodel_script.R")
lint("rmdscript_ipacs_V3.Rmd")