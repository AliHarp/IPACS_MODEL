# IPACS model -----------------------------------------------------------------

# Import required libraries
library(doSNOW)
library(foreach)
library(tidyverse, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(parallel)
library(readxl)
library(here)

# Clear workspace and set working directory
rm(list = ls())
wd <- setwd("~/Documents/IPACS_MODEL")

# Name of file with model parameters (should be stored in model_inputs folder)
input_filename <- "IPACS_20230214_fix.xlsx"

# Set manual parameters
sd_los <- 3 # estimate of standard deviation for length of stay distribution
nruns <- as.integer(5) # number of runs for each simulation
sd_isr <- 0.5 # initial service/visit rate
sd_esr <- 0.5 # end service rate/final visit rate
temp_seed <- 1
warmup <- 0

# Run model - P1 (visit) then P2 and P3 (bed) simulation - and print time taken
start_time <- Sys.time()
source(here("functions", "set_up.R"))
source(here("functions", "visit_model.R"))
source(here("functions", "bed_model.R"))
print(difftime(Sys.time(), start_time), quote = FALSE)

# Produce word document report using RMarkdown
suppressWarnings({
  rmarkdown::render("ipacs_produce_report.Rmd",
                    output_file = "outputs/ipacs_report")
})
