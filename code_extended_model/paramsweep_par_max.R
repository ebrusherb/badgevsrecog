 ## --- cluster set up
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)
start.time <- Sys.time()

source('model.R')
source('ind2sub.R')
source('sub2ind.R')
source('glue.R')


## ---- parameters -------------------------
Tfights = 1001 #total number of fights 
down_sample = 10
sim_runs = 5
confus_prob_cat = Inf #maximum probability of misidentifying categories, which decreases with dissimilarity
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
learn_noise = 0.5 #how noisy an updated assessment is
obs_noise = 0.5 # how noisy observational learning is
p_obs = 0 #probability of observing a fight you're not engaged in
observation_happens = TRUE

fixed_parameters = data.frame(Tfights=Tfights,down_sample=down_sample,sim_runs=sim_runs,learn_noise=learn_noise,obs_noise=obs_noise,p_obs=p_obs,observation_happens=observation_happens)

##---- parameter_sweep -----------------------
source('parameters_max.R')
to_work_with  = 1
parameters = parameters[chunk[[to_work_with]],]
xparam = dim(parameters)[1]
w_chunk = parameters$w[1]

error_cat <- foreach(i = 1:xparam, .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{	
	N = parameters$N[i]
	delta  = parameters$delta[i]
	memory_window = parameters$w[i]
	rho = parameters$rho[i]
	learn_rate = parameters$l[i]
	obs_learn_rate = learn_rate - 0.1
	L_temp = dynamics_cat() }

# if(w_chunk<Inf){	
error_rule <- foreach(i = 1:xwind, .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{	
	N = parameters$N[i]
	delta  = parameters$delta[i]
	memory_window = parameters$w[i]
	rho = parameters$rho[i]
	learn_rate = parameters$l[i]
	obs_learn_rate = learn_rate - 0.1
	L_temp = dynamics_rule() }	
	# }else{
	# error_rule = list(NA)
	# }
	
error_cat = error_cat[[1]]
error_rule = error_rule[[1]]

# if(to_work_with %in% (1-xwind+(1:xrho)*xwind)){
	error_genetic <- foreach(i = 1, .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{	
	N = parameters$N[i]
	delta  = parameters$delta[i]
	memory_window = parameters$w[i]
	rho = parameters$rho[i]
	learn_rate = parameters$l[i]
	obs_learn_rate = learn_rate - 0.1
	L_temp = dynamics_genetic() }	
	error_genetic = error_genetic[[1]]
# } else{
	# error_genetic = NA
	# }


## --- find average / sd of error and median of learning time across all inds / sims for each combination of parameters

error_cat_mean<- foreach(p=1:xparam,.combine='cbind') %do% {
		error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_cat[[p]][[k]]}
		colMeans(error_cat_tmp,na.rm=TRUE)
	}
	
error_cat_sd<- foreach(p=1:xparam,.combine='cbind') %do% {
		error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_cat[[p]][[k]]}
		colSds(error_cat_tmp,na.rm=TRUE)
	}	

# if(w_chunk<Inf){	
error_rule_mean<- foreach(p=1:xwind,.combine='cbind') %do% {
		error_rule_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_rule[[p]][[k]]}
		colMeans(error_rule_tmp,na.rm=TRUE)
	}	
# }else{
	# error_rule_mean = NA
	# }
	
# if(to_work_with %in% (1-xwind+(1:xrho)*xwind)){
	error_genetic_mean<- foreach(p=1,.combine='cbind') %do% {
		error_genetic_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_genetic[[p]][[k]]}
		colMeans(error_genetic_tmp,na.rm=TRUE)
	}
# }else{
	# error_genetic_mean = NA
	# }	
	

save(fixed_parameters,parameters,error_cat,error_rule,error_genetic,error_cat_mean,error_cat_sd,error_rule_mean,error_genetic_mean,file=paste('/Users/eleanorbrush/Desktop/summary_stats_max','.Rdata',sep=''))

run.time = Sys.time() - start.time
