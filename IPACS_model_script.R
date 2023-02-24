rm(list=ls())

install.packages("doSNOW")
install.packages("flextable")

rm(list=ls())
require(doSNOW)
#install.packages("foreach")
require(foreach)
#install.packages("tidyverse")
require(tidyverse, warn.conflicts=FALSE)
options(dplyr.summarise.inform=FALSE)
#install.packages("parallel")
require(parallel)
#install.packages("readxl")
require(readxl)

####################################################################################################################
#set working directory. Can use getwd() in console 
wd<-setwd("~/Documents/IPACS/RoutineReport_HandoverPeter")
input_file <- read.csv("IPACS input data.csv") 

arrivals_all <- readxl::read_excel(paste0("model_inputs/IPACS_", format(Sys.time(), 
                             "%Y%m%d.xlsx")), sheet = "arrivals")
init_conds <- readxl::read_excel(paste0("model_inputs/IPACS_", format(Sys.time(), 
                             "%Y%m%d.xlsx")), sheet = "initial conditions")
capacity <- readxl::read_excel(paste0("model_inputs/IPACS_", format(Sys.time(), 
                             "%Y%m%d.xlsx")), sheet = "capacity")
losA <- readxl::read_excel(paste0("model_inputs/IPACS_", format(Sys.time(), 
                             "%Y%m%d.xlsx")), sheet = "los")
costs <- readxl::read_excel(paste0("model_inputs/IPACS_", format(Sys.time(),
                              "%Y%m%d.xlsx")), sheet = "costs")
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

