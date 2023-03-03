library(diffdf)
library(here)
library(lintr)

# Looks for differences between latest visit output, and output from 
# original version of code
visit_data_new <- read.csv(here("outputs", "visit_output_using_IPACS_20230214_fix.csv"))
feb_14 <- read.csv(here("testing", "Scenarios_Visit_based_output_2023-02-14.csv"))
diffdf::diffdf(visit_data_new, feb_14)

# Linting
lint("IPACS_model_script.R")
lint("Visit_based_submodel_script.R")
lint("Bed_based_submodel_script.R")
lint("rmdscript_ipacs_V3.Rmd")