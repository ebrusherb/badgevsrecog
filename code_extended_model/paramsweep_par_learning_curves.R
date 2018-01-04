 ## --- cluster set up
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)
Date <- Sys.Date()

# num_cores <- detectCores()-1
num_cores <-10
cl <-makeCluster(num_cores)
registerDoParallel(cl)

source('model.R')
source('ind2sub.R')
source('sub2ind.R')
source('glue.R')


## ---- parameters -------------------------
Tfights = 20001 #total number of fights 
down_sample = 50
sim_runs = 25
confus_prob_cat = Inf #maximum probability of misidentifying categories, which decreases with dissimilarity
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
learn_noise = 0.1 #how noisy an updated assessment is
obs_noise = 0.1 # how noisy observational learning is
p_obs = 0 #probability of observing a fight you're not engaged in
observation_happens = TRUE

fixed_parameters = data.frame(Tfights=Tfights,down_sample=down_sample,sim_runs=sim_runs,learn_noise=learn_noise,obs_noise=obs_noise,p_obs=p_obs,observation_happens=observation_happens)

##---- parameter_sweep -----------------------
source('parameters_learning_curves.R')
xparam = dim(parameters)[1]

error_cat <- foreach(i = 1:xparam, .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %dopar%{	
	N = parameters$N[i]
	delta  = parameters$delta[i]
	memory_window = parameters$w[i]
	rho = parameters$rho[i]
	learn_rate = parameters$l[i]
	obs_learn_rate = learn_rate - 0.1
	L_temp = dynamics_cat() }
	
error_rule <- foreach(i = 1:(xparam/2), .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %dopar%{	
	N = parameters$N[i]
	delta  = parameters$delta[i]
	memory_window = parameters$w[i]
	rho = parameters$rho[i]
	learn_rate = parameters$l[i]
	obs_learn_rate = learn_rate - 0.1
	L_temp = dynamics_rule() }		

error_genetic <- foreach(i = 1:(xparam/2), .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %dopar%{	
	N = parameters$N[i]
	delta  = parameters$delta[i]
	memory_window = parameters$w[i]
	rho = parameters$rho[i]
	learn_rate = parameters$l[i]
	obs_learn_rate = learn_rate - 0.1
	L_temp = dynamics_genetic() }	

error_cat = error_cat[[1]]
error_rule = error_rule[[1]]
error_genetic = error_genetic[[1]]

stopCluster(cl)


## --- find average / sd of error and median of learning time across all inds / sims for each combination of parameters

error_cat_mean<- foreach(p=1:xparam,.combine='cbind') %do% {
		error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_cat[[p]][[k]]}
		colMeans(error_cat_tmp,na.rm=TRUE)
	}
	
error_rule_mean<- foreach(p=1:(xparam/2),.combine='cbind') %do% {
		error_rule_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_rule[[p]][[k]]}
		colMeans(error_rule_tmp,na.rm=TRUE)
	}	
	
error_genetic_mean<- foreach(p=1:(xparam/2),.combine='cbind') %do% {
	error_genetic_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
	error_genetic[[p]][[k]]}
	colMeans(error_genetic_tmp,na.rm=TRUE)
}

error_cat_sd<- foreach(p=1:xparam,.combine='cbind') %do% {
		error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_cat[[p]][[k]]}
		colSds(error_cat_tmp,na.rm=TRUE)
	}
	
error_rule_sd<- foreach(p=1:(xparam/2),.combine='cbind') %do% {
		error_rule_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		error_rule[[p]][[k]]}
		colSds(error_rule_tmp,na.rm=TRUE)
	}	
	
error_genetic_sd<- foreach(p=1:(xparam/2),.combine='cbind') %do% {
	error_genetic_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
	error_genetic[[p]][[k]]}
	colSds(error_genetic_tmp,na.rm=TRUE)
}
	

save(fixed_parameters,parameters,error_cat_mean,error_rule_mean,error_genetic_mean,error_cat_sderror_rule_sd,error_genetic_sd,file='/homes/ebrush/priv/badgevsrecog/summary_stats_learning_curves.Rdata')

quit()
