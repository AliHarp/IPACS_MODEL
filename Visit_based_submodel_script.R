

############## Model for P1 type pathways #####################
# PARAMETERS ###################################
################################################

# Select visit-based scenarios
visit_scenarios <- scenarios %>% filter(str_detect(node, "P1"))
arr_scenarios_v <- arr_scenarios %>% filter(str_detect(node, "P1"))
costs_visit <- costs %>% filter(str_detect(node, "P1"))
sd_ISR <- 0.5
sd_ESR <- 0.5
#####input variables#####
visit_cap <- as.list(visit_scenarios$capacity)
visit_init_occ <- as.list(visit_scenarios$occ)
visit_init_q <- as.list(visit_scenarios$dtoc)
visit_loss <- as.list(rep(0, nrow(visit_scenarios)))
visit_srv_dist <- as.list(visit_scenarios$los_dist)

#if lnorm dist los
visit_param_dists <- visit_scenarios %>% separate(los_params, 
                      into = c("mu", "sigma"), sep = ",")
visit_param_dists$mu <- as.double(visit_param_dists$mu)
visit_param_dists$sigma <- as.double(visit_param_dists$sigma)
mu_sig_pair<-as.data.frame(cbind(visit_param_dists$mu,visit_param_dists$sigma))
colnames(mu_sig_pair) <- NULL
visit_srv_params <- as.list(data.frame(t(mu_sig_pair)))

#if norm
visit_param_dist <- as.list(as.integer(visit_scenarios$mean_los))
visit_param_sd <- as.list(rep(sd_los, nrow(visit_scenarios)))

#arrival rates 
#arrivalsSV$scenarios <- paste0(arrivalsSV$node, '_', arrivalsSV$scenario)
arr_rates_visit_p1 <- arr_scenarios_v %>% 
  dplyr::select(arrivals, date, S) %>%
  pivot_wider(names_from=S,
      values_from=arrivals) %>%
    arrange(date)
visit_pathway_vector <- dput(colnames(arr_rates_visit_p1[-1]))
#set the minimum and maximum LOS and ISR. 
ISR_list<-as.list(visit_scenarios$IVR)
ISR <- as.integer(ISR_list)
endSR_list<-as.list(visit_scenarios$FVR)
endSR <- as.integer(endSR_list)
sd_ISR_list <- as.list(rep(sd_ISR, nrow(visit_scenarios)))
sd_ESR_list <- as.list(rep(sd_ESR, nrow(visit_scenarios)))
sd_ISR <- as.double(sd_ISR_list)
sd_ESR <- as.double(sd_ESR_list)

n_patients <- as.integer(visit_cap) #number of patients system can take
n_slots  <- n_patients * mean(c(ISR, endSR))

SEED <-1
sim_length <- as.integer(run_time)
warmup <-0 
nruns_p1<-as.integer(nruns_all)
visits_based_output<-NULL
# date_ts = data.frame(seq(arr_rates_visit_p1[1,1]), 
#   by = "day", length.out = sim_length)
    
  ####runs##################################
  #z represents the index for all scenarios through all visit-based pathways

  for(z in 1:length(visit_pathway_vector)){
    cl<-parallel::makeCluster(1)   #, setup_strategy="sequential")
    registerDoSNOW(cl)
    run<-1
    RESULTS<-foreach(run=1:nruns_all,.combine="rbind") %dopar% {
    set.seed(nruns_p1*(SEED-1)+run)  
    
      #LOS distribution
     dis_los <- function(){
       max_los <- visit_param_dist[[z]] + visit_param_sd[[z]]*3
       if (visit_srv_dist[[z]] == "lnorm"){
         x<- round(do.call(paste0("r",visit_srv_dist[[z]]),
                           c(list(1,visit_srv_params[[z]]))))
         while (x<=0 | x>= max_los){
           x<- round(do.call(paste0("r",visit_srv_dist[[z]]),
                             c(list(1,visit_srv_params[[z]]))))}
       }
       else {
         x<- round(do.call(paste0("r", visit_srv_dist[[z]]), 
                           c(list(1, visit_param_dist[[z]], visit_param_sd[[z]]))))
         while (x<=0 | x>= max_los){ 
           x<- round(do.call(paste0("r",visit_srv_dist[[z]]),
                             c(list(1, visit_param_dist[[z]], visit_param_sd[[z]]))))} 
       }
       return(as.integer(x))}
    
    # dis_los2 for patients already in service
     dis_los2 <- function(){
       sample_los <- dis_los()
       x <- round(runif(1, 1, sample_los))
       return(as.integer(x))
     } 
        
    #ISR distribution
    dis_init_slots <- function(){
      x<- round(do.call(paste0("rnorm"),c(list(1,mean=ISR[z],sd=sd_ISR[z]))))
      max_v <- ISR[z] + (sd_ISR[z]*3)
      while (x<=0 | x>max_v | x>n_slots[z]){
        x<- round(do.call(paste0("rnorm"),c(list(1,mean=ISR[z],sd=sd_ISR[z]))))}
      return(x)
    }
    
    #end SR distribution 
    dis_end_slots <- function(){
      x<- round(do.call(paste0("rnorm"),c(list(1,mean=endSR[z],sd=sd_ESR[z]))))
      max_v <- endSR[z] + (sd_ESR[z]*3)
      while (x<=0 | x>max_v){
        x<- round(do.call(paste0("rnorm"),c(list(1,mean=endSR[z],sd=sd_ESR[z]))))}
      return(x)
    }
    #####output variables#####
    ent_sys <- 0 # number of entities that entered the system
    left_sys <-0 # number of entities that left the system
    
    #output after warm up period
    output<-data.frame(RUNX = integer(sim_length), #run number x
                       node = character(sim_length), #scenario
                       day = integer(sim_length), #output per day
                       q_length = integer(sim_length), #number of patients in the queue
                       n_slots_used = numeric(sim_length),
                       patients_in_service = numeric(sim_length),
                       res_used = numeric(sim_length), #used slots
                       res_idle = numeric(sim_length), #idle slots
                       in_sys = numeric(sim_length) #number of patients in the system
    ) 
    #####creating necessary data structures#####
    patients_initial<-data.frame(id = integer(visit_init_occ[[z]]),            #patient id
                                 los = integer(visit_init_occ[[z]]),           #length of stay
                                 arrival_time = integer(visit_init_occ[[z]]), # day in the simulation the entity arrived
                                 start_service = integer(visit_init_occ[[z]]), # day actual service started
                                 end_service = integer(visit_init_occ[[z]]),   # day service ended
                                 wait_time = integer(visit_init_occ[[z]]),     # number of days spent in the queue
                                 exit = logical(visit_init_occ[[z]]),          # boolean variable, TRUE if the entity has left the system
                                 stringsAsFactors = FALSE)
    
    patients_inqueue<-data.frame(id = integer(visit_init_q[[z]]),            #patient id
                                 los = integer(visit_init_q[[z]]),           #length of stay
                                 arrival_time = integer(visit_init_q[[z]]), # day in the simulation the entity arrived
                                 start_service = integer(visit_init_q[[z]]), # day actual service started
                                 end_service = integer(visit_init_q[[z]]),   # day service ended
                                 wait_time = integer(visit_init_q[[z]]),     # number of days spent in the queue
                                 exit = logical(visit_init_q[[z]]),          # boolean variable, TRUE if the entity has left the system
                                 stringsAsFactors = FALSE)
    
    #patient list
    patients<-data.frame(id = integer((sim_length+warmup)*2),            #patient id
                         los = integer((sim_length+warmup)*2),           #length of stay
                         arrival_time = integer((sim_length+warmup)*2), # day in the simulation the entity arrived
                         start_service = integer((sim_length+warmup)*2), # day actual service started
                         end_service = integer((sim_length+warmup)*2),   # day service ended
                         wait_time = integer((sim_length+warmup)*2),     # number of days spent in the queue
                         exit = logical((sim_length+warmup)*2),          # boolean variable, TRUE if the entity has left the system
                         stringsAsFactors = FALSE)
    
    npat <- 0 #initialising counter for patients dataframe
    
    #list with required visit vectors for each patient
    req_visits <- list()
    
    #resources
    resources <- matrix(nrow=(sim_length+warmup)*10, ncol = 1) #times 2 to make the calculations for resources work at the end of the simulation
    
    resources[,] <- n_slots[z]
    
    #vector for storing waiting time, kept for each patient who left the system
    waittime_vec <- data.frame(RUNX=integer(),
                               day_=integer(),
                               scen_=character(),
                               start_service= integer(),
                               waittime = integer(),
                               stringsAsFactors=T)
    id<-0
    t<-1
    #creating set of initial condition patients that are already in the system at day 1.
    for (j in 1:visit_init_occ[[z]]) {
      id<-id+1
      npat<-npat+1
      los<- dis_los2()
      templos<-dis_los() #for determining visit seq of those already in service
      arrival_time <- t
      exit <-FALSE
      patients_initial[npat, ] <- c(id,los,arrival_time,NA, NA, 0, exit)
      
      #initial slots and creating required visits vector
      init_slots <- dis_init_slots()
      end_slots <- dis_end_slots()
      temp_visit_vector <- round(seq(init_slots,end_slots,length.out = templos)) #full visit seq
      visit_vector <- seq(rev(rev(temp_visit_vector[1])), 
                          rev(temp_visit_vector[length(los)])) #truncate to new LoS
      req_visits[[id]] <- visit_vector
      
      #planning service, checking resources
      tt<-t #temporary t for incrementing when no resources available
      
      while (is.na(patients_initial$start_service[npat])==TRUE){
        if (all((resources[((tt):((tt)+patients_initial$los[npat]-1)),]>= req_visits[[id]])==TRUE)){
          patients_initial$start_service[npat] <- tt
          patients_initial$end_service[npat] <- patients_initial$start_service[npat]+(patients_initial$los[npat]-1)
          
          #decrease capacity
          resources[((tt):((tt)+patients_initial$los[npat]-1)),] <-
            resources[((tt):((tt)+patients_initial$los[npat]-1)),] - req_visits[[id]]
        } else {
          tt<-tt+1 #if no sufficient resources, check for starting on the next day
        } 
      }          
    }
   ent_sys<-ent_sys+npat
    
    #creating set of initial condition patients that are already in the system at day 1.
    for (j in 1:visit_init_q[[z]]) {
      id<-id+1
      npat<-npat+1
      los<- dis_los()
      arrival_time <- t
      exit <-FALSE
      patients_inqueue[npat, ] <- c(id,los,arrival_time,NA, NA, 0, exit)
      
      #initial slots and creating required visits vector
      init_slots <- dis_init_slots()
      end_slots <- dis_end_slots()
      visit_vector <- round(seq(init_slots,end_slots,length.out = los)) #full visit seq
      req_visits[[id]] <- visit_vector
      
      #planning service, checking resources
      tt<-t #temporary t for incrementing when no resources available
      while (is.na(patients_inqueue$start_service[npat])==TRUE){
        if (all((resources[((tt):((tt)+patients_inqueue$los[npat]-1)),]>= req_visits[[id]])==TRUE)){
          patients_inqueue$start_service[npat] <- tt
          patients_inqueue$end_service[npat] <- patients_inqueue$start_service[npat]+(patients_inqueue$los[npat]-1)
          
          #decrease capacity
          resources[((tt):((tt)+patients_inqueue$los[npat]-1)),] <- 
            resources[((tt):((tt)+patients_inqueue$los[npat]-1)),] - req_visits[[id]]
        } else {
          tt<-tt+1 #if no sufficient resources, check for starting on the next day
        }
      }
    }
    patients<-rbind(patients_initial,patients_inqueue, patients)
    ent_sys<-ent_sys+npat

    #####simulation#####
    #  id<-0  
    # t<-2
    for (t in 1:(sim_length+warmup)) {
      #arrivals to service
      narr<-round(rpois(1,arr_rates_visit_p1[t,z+1]))
      if(narr>0){
        ent_sys <- ent_sys + narr
        
        #for each arrived patient
        for (j in 1:narr) {
          id<-id+1
          npat<-npat+1
          los<- dis_los()
          arrival_time <- t
          exit <-FALSE
          patients[npat, ] <- c(id,los,arrival_time,NA, NA, 0, exit)
          
          #initial slots and creating required visits vector
          init_slots <- dis_init_slots()
          end_slots <- dis_end_slots()
          visit_vector <- round(seq(init_slots,end_slots,length.out = los))
          
          req_visits[[id]] <- visit_vector
          
          #planning service, checking resources
          tt<-t #temporary t for incrementing when no resources available
          
          while (is.na(patients$start_service[npat])==TRUE){

            print((tt):((tt)+patients$los[npat]-1))
            if (all((resources[((tt):((tt)+patients$los[npat]-1)),]>= req_visits[[id]])==TRUE)){
              patients$start_service[npat] <- tt
              patients$end_service[npat] <- patients$start_service[npat]+(patients$los[npat]-1)
              
              #decrease capacity
              resources[((tt):((tt)+patients$los[npat]-1)),] <- resources[((tt):((tt)+patients$los[npat]-1)),] - req_visits[[id]]
            } else {
              tt<-tt+1 #if no sufficient resources, check for starting on the next day
            } 
          }          
        }
      }
      
      in_q<-which((patients$start_service>t)&(patients$id>0))
      if (length(in_q)>0){
        patients[in_q,6]<- patients[in_q,6]+1
      }
      #recording output from the day warm up period has finished
      if (t>warmup){ #only start recording after the warm up period
        if (npat>0 & nrow(waittime_vec)>0) {
          output[t-warmup, ]<- c(RUNX=run, 
                                 node=visit_pathway_vector[z],
                                 day= t,
                                 q_length = length(in_q),
                                 n_slots_used = n_slots[z]-(resources[t,]),
                                 patients_in_service=(n_slots[z]-(resources[t,]))/(mean(c(ISR[z], endSR[z]))),
                                 res_used= 1- (resources[t,]/n_slots[z]),
                                 res_idle= resources[t,]/n_slots[z],
                                 in_sys = (ent_sys - left_sys))
          
        } else if (npat>0 & nrow(waittime_vec)==0) {
          output[t-warmup, ]<- c(RUNX=run,
                                 node=visit_pathway_vector[z],
                                 day= t,
                                 q_length = length(in_q),
                                 n_slots_used = n_slots[z]-(resources[t,]),
                                 patients_in_service=(n_slots[z]-(resources[t,]))/(mean(c(ISR[z], endSR[z]))),
                                 res_used= 1- (resources[t,]/n_slots[z]),
                                 res_idle= resources[t,]/n_slots[z],
                                 in_sys = (ent_sys - left_sys))
        } else {
          output[t-warmup, ]<- c(RUNX=run,
                                 node=visit_pathway_vector[z],
                                 day= t,
                                 q_length = length(in_q),
                                 n_slots_used = n_slots[z]-(resources[t,]),
                                 patients_in_service=(n_slots[z]-(resources[t,]))/(mean(c(ISR[z], endSR[z]))),
                                 res_used= 1-(resources[t,]/n_slots[z]),
                                 res_idle= resources[t,]/n_slots[z],
                                 in_sys = (ent_sys - left_sys))
        }
      
      #remove patients whose service has ended from the patients table
      remove <- which(patients$end_service==t)
      if(length(remove)>0){
        if(t>=warmup){
          df<-data.frame(RUNX = run, day_=t, scen_= visit_pathway_vector[z], start_service= patients$start_service[remove], 
                         waittime= patients[remove,6])
          waittime_vec <- rbind(waittime_vec,df) #keeping waiting time
        }
        patients <- patients[-remove,] #remove from patient list
        npat<- npat - length(remove)
        left_sys <- left_sys + length(remove)
      }
      }
    }    
    list<-list(output, resources, waittime_vec)
    
    return(list)
  }
  stopCluster(cl)
  ###############################################
  
  #creating dataframe for summary info
  summary <- data.frame(LOS = integer(nruns_p1),
                        ISR = integer(nruns_p1),
                        nruns = integer(nruns_p1),
                        sim_length = integer(nruns_p1),
                        warm_up=integer(nruns_p1),
                        capacity = integer(nruns_p1),
                        mean_wait= numeric(nruns_p1),
                        q_length = numeric(nruns_p1),
                        res_used= numeric(nruns_p1),
                        res_idle= numeric(nruns_p1),
                        in_sys = numeric(nruns_p1))
  
  #splitting up RESULTS list in 3
  output<-RESULTS[,1]
  out<-do.call(rbind, output)
  out[, c(5:9)] <- sapply(out[, c(5:9)], as.numeric)
  out[, c(1,3:4)] <- sapply(out[, c(1,3:4)], as.integer)
  #combining in one dataframe
  
  resources<-RESULTS[,2]
  res<-do.call(cbind, resources) 
  colnames(res)<- c(1:nruns_p1)
  
  waittimes <- RESULTS[,3]
  wait<-do.call(rbind, waittimes)
  
  #summary of all runs
  for (k in 1:nruns_p1){ 
    r.out <- which(out[,1]==k)
    k.wait <- which(wait[,1]==k)
    summary[k,]<- c(LOS = 1/visit_param_dist[[z]],
                    ISR = ISR[z],
                    nruns = nruns_p1,
                    sim_length = sim_length,
                    warm_up=warmup,
                    capacity = n_slots[z],
                    mean_wait= round(mean(wait$waittime[k.wait]),2),
                    q_length = round(mean(out$q_length[r.out]),2),
                    res_used= round(mean(out$res_used[r.out]),2),
                    res_idle= round(mean(out$res_idle[r.out]),2),
                    in_sys= round(mean(out$in_sys[r.out]),2) ) 

#niq and occ by day (plus other measures if needed)
ts_output <- out %>% group_by(day, node) %>%
    summarise(niq=mean(q_length), in_sys=mean(in_sys),
              n_slots_used=mean(n_slots_used),
              occ=mean(patients_in_service),
              mean_res_idle=mean(res_idle), mean_res_used=mean(res_used)) %>%
              ungroup()

#create cost columns
loc = sapply(ts_output$node, function(x) paste(strsplit(x, "_")[[1]][1:2], collapse = '_'))
ts_output$node <- loc
ts_output <- left_join(ts_output,costs_visit, by="node")
ts_output <- ts_output %>% mutate(cost=(niq*acute_dtoc)+(n_slots_used*community_cost))
ts_output <- cbind(ts_output)

#waits by day
ts_waits <- wait %>% group_by(day_, scen_) %>% 
  summarise(wait=mean(waittime)) %>%
  ungroup()

#for each scenario:
ts_output <- cbind(ts_output, ts_waits)
  } 
#rowbind each scenario
visits_based_output <- rbind(visits_based_output, ts_output)

  }

ptvisits_niq<-visits_based_output[,c(1,13,3)] %>% pivot_wider(names_from=scen_, values_from=niq)
ptvisits_niq<-subset(ptvisits_niq, select= -c(day))
ptvisits_occ<-visits_based_output[,c(1,13,6)] %>% pivot_wider(names_from=scen_, values_from=occ) 
ptvisits_occ<-subset(ptvisits_occ, select= -c(day))
ptvisits_wait<-visits_based_output[,c(1,13,14)] %>% pivot_wider(names_from=scen_, values_from=wait) 
ptvisits_wait<-subset(ptvisits_wait, select= -c(day))
ptvisits_costs<-visits_based_output[,c(1,13,11)] %>% pivot_wider(names_from=scen_, values_from=cost)
ptvisits_costs<-subset(ptvisits_costs, select= -c(day))

colnames_v <- cbind(c('date', (paste0(visit_pathway_vector,"__niq")), 
                      (paste0(visit_pathway_vector,"__occ")),
                      (paste0(visit_pathway_vector, "__wait")),
                      (paste0(visit_pathway_vector, "__cost"))))
MeansOutput_v<-cbind(data.frame(arr_rates_visit_p1$date[1:length(arr_rates_visit_p1$date)]),
                     data.frame(data.frame(ptvisits_niq)),
                     data.frame(data.frame(ptvisits_occ)),
                     data.frame(data.frame(ptvisits_wait)),
                     data.frame(data.frame(ptvisits_costs)))
colnames(MeansOutput_v)<-colnames_v

# CHANGE: Save to csv, with filename based on the input file used rather than today's date
write.csv(MeansOutput_v,
          paste0("outputs/visit_output_using_",
                 gsub(".xlsx", "", input_filename), ".csv"),
          row.names = FALSE)
 