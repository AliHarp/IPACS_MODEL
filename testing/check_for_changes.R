# Check output is still the same
library(diffdf)
library(here)

# Looks for differences between latest visit output, and output from 
# original version of code
visit_data_new <- read.csv(here("outputs", "visit_output_using_IPACS_20230214_fix.csv"))
feb_14 <- read.csv(here("testing", "Scenarios_Visit_based_output_2023-02-14.csv"))
diffdf::diffdf(visit_data_new, feb_14)