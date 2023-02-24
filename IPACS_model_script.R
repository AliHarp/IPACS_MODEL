rm(list=ls())

# Need to add to renv:
# install.packages("flextable")

# CHANGE: Changed to library (as stops if there is error, but require runs)
# CHANGE: Removed install.packages() as all should be present with renv
library(doSNOW)
library(foreach)
library(tidyverse, warn.conflicts=FALSE)
options(dplyr.summarise.inform=FALSE)
library(parallel)
library(readxl)
library(here)

####################################################################################################################
#set working directory. Can use getwd() in console 
wd <- setwd("~/Documents/IPACS_MODEL")
input_file <- read.csv("IPACS input data.csv") 

# CHANGE: Input file now generated manually, so changed to manually input of filename
input_filename <- "IPACS_20230214_fix.xlsx"

# CHANGE: Create list of (1) dataframes to create, and (2) sheets to import from
# Then import each sheet and save to the relevant dataframe
# Use "here" package to create relative file path that works on all systems
# (i.e. no backslashes)
input_list <- list(c("arrivals_all", "arrivals"),
                   c("init_conds", "initial conditions"),
                   c("capacity", "capacity"),
                   c("losA", "los"),
                   c("costs", "costs"))
for (x in input_list){
  assign(x[1], readxl::read_excel(here("model_inputs", input_filename),
                                  sheet=x[2]))
}

#process data
#source("Parameters_IPACS_2.R")

#read in data
#run_time <- 28*2
sd_los <- 3 #no info on sd - estimate provided

run_time <- length(unique(arrivals_all$date))

#create single df with one row per simulation pathway/scenario
capacity$S <- paste0(capacity$node, "_", capacity$scenario)
arrivalsS <- arrivals_all[!duplicated(arrivals_all[, c(1,3)]),]
scenarioss <- merge(capacity, losA, by="node", all=TRUE)
scenarioss$S <- paste0(scenarioss$S, "_", scenarioss$scenario.y)

scenarios <-  merge(arrivalsS, scenarioss, by="node", all=TRUE)
scenarios$S <- paste0(scenarios$S, "_", scenarios$scenario)
scenarios <- merge(scenarios, init_conds, by= "node", all=TRUE) %>%
  pivot_wider(names_from = measure, values_from = value)

#arrivals dataframe for arrivals trajectories
arr_scenarios <- merge(arrivals_all, scenarioss, by="node", all=TRUE)
arr_scenarios$S <- paste0(arr_scenarios$S, "_", arr_scenarios$scenario)  
arr_scenarios <- merge(arr_scenarios, init_conds, by= "node", all=TRUE)  %>%
  pivot_wider(names_from = measure, values_from = value)

start.time<-Sys.time()
nruns_all<-5
  
source("Visit_based_submodel_script.R")
source("Bed_based_submodel_script.R")
print(difftime(Sys.time(),start.time),quote=FALSE)

suppressWarnings({rmarkdown::render('rmdscript_ipacs_V3.Rmd', output_file=("IPACS Report"))})

#suppressWarnings({rmarkdown::render('rmdscript_ipacs_V5_fewscenarios_november.Rmd', output_file=("5 Scenarios IPACS Report November"))})

