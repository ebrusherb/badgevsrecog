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
source('glue.R')


## ---- parameters -------------------------
Tfights = 20000 #total number of fights 
Tfights_min = 5000
sim_runs = 25
# N = 20 # individuals
# perc_wind = 0.1 # difference that animals can perceive
# memory_window = Inf #how many fights ago they can remember
# confus_prob_cat = 1000 #maximum probability of misidentifying categories, which decreases with dissimilarity
# confus_prob_ind = 0 #probability of misidentifying individuals
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
# sig_qual_corr = 0.5 #correlation between quality and signal
learn_rate = 0.2 #how much the quality of the opponent affects the new assessment
learn_noise = 0.01 #how noisy an updated assessment is
obs_learn_rate = 0.1 #how much true difference affects assessment of difference
obs_noise = 0.02 # how noisy observational learning is
# p_obs = 0.2 #probability of observing a fight you're not engaged in
dominance = 2 #how quickly the probability switches from A winning to A losing
error_threshold = 0.2
observation_happens = TRUE

##---- parameter_sweep -----------------------
source('parameters.R')
args = commandArgs(TRUE)
to_work_with  = strtoi(args[1], base=10L)
parameters = parameters[chunk[[to_work_with]],]
xparam = dim(parameters)[1]

L <- foreach(i = 1:xparam, .combine='glue',.multicombine=TRUE, .init=list(list(),list(),list(),list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list(),list(),list(),list())) %dopar%{	
	N = parameters$N[i]
	perc_wind = parameters$c1[i]
	memory_window = parameters$w[i]
	confus_prob_cat = parameters$pcat[i]
	confus_prob_ind = parameters$pind[i]
	sig_qual_corr = parameters$c2[i]
	p_obs = parameters$pobs[i]
	L_temp = dynamics_full() }

error_cat = as.list(1:xparam)
error_ind = as.list(1:xparam)
time_cat = as.list(1:xparam)
time_ind = as.list(1:xparam)

for(i in 1:xparam){
	error_cat[[i]] = L[[1]][[i]]
	error_ind[[i]] = L[[2]][[i]]
	time_cat[[i]] = L[[3]][[i]]
	time_ind[[i]] = L[[4]][[i]]
}

stopCluster(cl)


## --- find average / sd of error and median of learning time across all inds / sims for each combination of parameters

error_cat_mean<- foreach(p=1:xparam,.combine='cbind') %do% {
		error_cat_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {		
		error_cat[[p]][[k]][,Tfights]	}
		mean(error_cat_tmp,na.rm=TRUE)
	}

error_ind_mean<- foreach(p=1:xparam,.combine='cbind') %do% {
		error_ind_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {		
		error_ind[[p]][[k]][,Tfights]	}
		mean(error_ind_tmp,na.rm=TRUE)
	}

time_cat_mean<- foreach(p=1:xparam,.combine='c') %do% {
		time_cat_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {		
		time_cat[[p]][[k]] }
		mean(time_cat_tmp)
	} 	
	
time_ind_mean<- foreach(p=1:xparam,.combine='c') %do% {
		time_ind_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {	
		time_ind[[p]][[k]] }
		mean(time_ind_tmp)
	} 	
	
error_time = list()

save(parameters,error_cat_mean,error_ind_mean,time_cat_mean,time_ind_mean,error_time,Tfights,Tfights_min,sim_runs,learn_rate,learn_noise,obs_learn_rate,obs_noise,file=paste('/homes/ebrush/priv/badgevsrecog/summary_stats_',substr(Date,1,4),'_',substr(Date,6,7),'_',substr(Date,9,10),'_',to_work_with,'.Rdata',sep=''))


if(to_work_with == breaks){
	collapsed_indices = matrix(1:xparam,ncol=length(c2vals))
	for(p in 1:(xparam/length(c2vals))){
		
		error_cat_stats_time<- foreach(c2=c2vals,.combine='cbind') %do% {
			error_cat_stats_time_tmp <-foreach(k = 1:sim_runs,.combine='rbind') %do%{
				error_cat[[collapsed_indices[p,c2]]][[k]][,1:Tfights_min]
			} 
			rbind(colMeans(error_cat_stats_time_tmp,na.rm=TRUE),colSds(error_cat_stats_time_tmp,na.rm=TRUE))
		}
		c2 = which.max(corr_vals[c2vals])
		error_ind_stats_time_tmp <-foreach(k = 1:sim_runs,.combine='rbind') %do%{
				error_ind[[collapsed_indices[p,c2]]][[k]][,1:Tfights_min]
			} 
		error_ind_stats_time<-rbind(colMeans(error_ind_stats_time_tmp,na.rm=TRUE),colSds(error_ind_stats_time_tmp,na.rm=TRUE))
		
		error = data.frame(error = c(error_cat_stats_time[1,],error_ind_stats_time[1,]),sd = 0.5*c(error_cat_stats_time[2,],error_ind_stats_time[2,]),categ = as.factor(c(rep('Badge',length(c2vals)*Tfights_min),rep('Indiv',Tfights_min))),fights=rep(1:Tfights_min,times=length(c2vals)+1),sigcorr=as.factor(c(rep(corr_vals[c2vals],each=Tfights_min),rep(corr_vals[c2],Tfights_min))))
		error_time[[p]] = error
		
	}
}

save(parameters,error_cat_mean,error_ind_mean,time_cat_mean,time_ind_mean,error_time,Tfights,Tfights_min,sim_runs,learn_rate,learn_noise,obs_learn_rate,obs_noise,file=paste('/homes/ebrush/priv/badgevsrecog/summary_stats_',substr(Date,1,4),'_',substr(Date,6,7),'_',substr(Date,9,10),'_',to_work_with,'.Rdata',sep=''))

quit()
