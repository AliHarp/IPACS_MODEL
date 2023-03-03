# Check output is still the same
library(diffdf)

# Comparing dataframe from 1st March to newest = MATCH 
visit_data_new <- read.csv(here("outputs", "visit_output_using_IPACS_20230214_fix.csv"))
visit_data_og <- read.csv(here("outputs", "visit_output_using_IPACS_20230214_fix_for_checks.csv"))
diffdf::diffdf(visit_data_new, visit_data_og)
# dim(setdiff(visit_data_new, visit_data_og))

# Comparing dataframe from Allison to latest = MATCH
feb_14 <- read.csv(here("outputs", "Scenarios_Visit_based_output_2023-02-14.csv"))
feb_24 <- read.csv(here("outputs", "Scenarios_Visit_based_output_2023-02-24.csv"))
diffdf::diffdf(feb_14, feb_24)
# dim(setdiff(feb_14, feb_24))

# Comparing now to previous
diffdf::diffdf(visit_data_new, feb_24)