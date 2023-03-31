# IPACS
Model for IPACS (Improving Patient flow between Acute, Community and Social care) project. Model used for D2A resource planning.  
  
This repository contains the models, with re-coding and bug fixes from [Amy Heather](https://github.com/amyheather).  
  

## Binder  
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/AliHarp/IPACS_MODEL/HEAD?urlpath=rstudio)  
 
## Bug fixes  
Most modification has been about simplification, but there have been some bug fixes that have modified the results of the simulation. These are noted below:  
* Visit simulation: `dislos2()` - each patient was leaving after 1 day with max visits on that day - modified so they have a shortened length of stay (but not 1), and the number of visits tapers towards the minimum  
* Visit simulation: `patients_initial`, `patients_inq` and `patients` were using `[npat]` as index to add patients, but npat increments, and as they were seperate dataframes, this meant there were many empty rows when rbound them together. Hence, when then add patients to that `rbind()` dataframe, you started replacing some, due to the npat indice. Modified this so that they all just add to a single patients dataframe using npat from the start, as that is ultimately what was trying to be achieved anyway  
* Visit simulation: `ent_sys = ent_sys + npat` is making ent_sys too large, as npat did not reset, but this didn't impact outcomes of the visit simulation, as in_sys (calculated from ent_sys) is not included in the output csv  
* Visit simulation: `as.integer()` in `visit_param_dist <- as.list(as.integer(visit_scenarios$mean_los))` rounded down this value (the mean of a normally distribution LOS) - so 12.3 becomes 12, but 8.9 becomes 8, 3.88 becomes 3, etc. Even in a simulation just using lnorm LOS, this impacted results, as maximum LOS used in `dis_los()` was calculated from the mean and SD of the LOS normal distribution, not lnorm.  
* Mu and sigma estimation: this is not a bug, but is a change that was made that inadvertenly has a large impact on results. The estimation of `mu` and `sigma` for use in `rlnorm()` for length of stay has been done within the project using two different methods - (1) based on median and mean, or (2) based on mean and an estimated standard deviation. With use of each method, you see large difference in simulation results, and further discussion is required within the team on the most suitable method to use, and hence the script currently provides the option of using either method  

## Linux flextable dependencies  
If on linux, may get error when installing flextable, as require the following linux Ubuntu dependencies for the following flextable dependencies:  
* `systemfonts - libfontconfig1-dev`  
* `textshaping - libfribidi-dev libharfbuzz-dev`  
* `ragg - libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev`  
* `gdtools - libcairo2-dev`  
  
For each of these, on terminal, run command: `sudo apt -y install dependencyname` (e.g. `sudo apt -y install libfontconfig1-dev`).  

## License  
[MIT](https://choosealicense.com/licenses/mit/)