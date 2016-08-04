 ## --- cluster set up
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)

# num_cores <- detectCores()-1
num_cores <-10
cl <-makeCluster(num_cores)
registerDoParallel(cl)

source('model.R')
source('ind2sub.R')
source('glue.R')



## ---- parameters -------------------------
Tfights = 10000 #total number of fights 
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
dominance = 2 #how quickly the probability switches from A winning to A losing
error_threshold = 0.2
observation_happens = FALSE

##---- parameter_sweep -----------------------
sim_runs = 25
N_vals = seq(20,100,by=10)
xN = length(N_vals)
perc_vals = c(0.4)
xperc = length(perc_vals)
wind_vals = c(Inf,seq(2000,0,by=-250))
xwind = length(wind_vals)
confus_cat_vals = c(Inf)
xconfus_cat = length(confus_cat_vals)
confus_ind_vals = c(0)
xconfus_ind = length(confus_ind_vals)
corr_vals = c(0.5,0.9)
xcorr = length(corr_vals)
d = c(xN,xperc,xwind,xconfus_cat,xconfus_ind,xcorr)
P = prod(d)
Tfights_min = 5000
c2vals = c(1,2)
xcorr2 = length(c2vals)
toplot = data.frame(n = rep(c(1,4),each=3), c1 = rep(c(1,1,1),2), w = rep(c(1,1,3),2), pcat = rep(1,6), pind = rep(1,6))

L <- foreach(ind = 1:P, .combine='glue',.multicombine=TRUE, .init=list(list(),list(),list(),list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list(),list(),list(),list())) %dopar%{
	v = ind2sub(d,ind)
	N = N_vals[v[1]]
	perc_wind = perc_vals[v[2]]
	memory_window = wind_vals[v[3]]
	confus_prob_cat = confus_cat_vals[v[4]]
	confus_prob_ind = confus_ind_vals[v[5]]
	sig_qual_corr = corr_vals[v[6]]
	L_temp = dynamics_full() }

error_cat = as.list(1:P)
dim(error_cat) = d
error_ind = as.list(1:P)
dim(error_ind) = d
time_cat = as.list(1:P)
dim(time_cat) = d
time_ind = as.list(1:P)
dim(time_ind) = d

for(ind in 1:P){
	error_cat[[ind]] = L[[1]][[ind]]
	error_ind[[ind]] = L[[2]][[ind]]
	time_cat[[ind]] = L[[3]][[ind]]
	time_ind[[ind]] = L[[4]][[ind]]
}

stopCluster(cl)


## --- find average / sd of error and median of learning time across all inds / sims for each combination of parameters

error_cat_stats<- foreach(p=1:P,.combine='cbind') %do% {
		error_cat_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		error_cat[[n,c1,w,pcat,pind,c2]][[k]][,Tfights]	}
		c(mean(error_cat_tmp,na.rm=TRUE),sd(error_cat_tmp,na.rm=TRUE))
	}
error_cat_stats = unname(error_cat_stats)
error_cat_mean = error_cat_stats[1,]
error_cat_sd = error_cat_stats[2,]

error_ind_stats<- foreach(p=1:P,.combine='cbind') %do% {
		error_ind_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		error_ind[[n,c1,w,pcat,pind,c2]][[k]][,Tfights]	}
		c(mean(error_ind_tmp,na.rm=TRUE),sd(error_ind_tmp,na.rm=TRUE))
	}
error_ind_stats = unname(error_ind_stats)
error_ind_mean = error_ind_stats[1,]
error_ind_sd = error_ind_stats[2,]

time_cat_mean<- foreach(p=1:P,.combine='c') %do% {
		time_cat_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		time_cat[[n,c1,w,pcat,pind,c2]][[k]] }
		mean(time_cat_tmp)
	} 	
	
time_ind_mean<- foreach(p=1:P,.combine='c') %do% {
		time_ind_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		time_ind[[n,c1,w,pcat,pind,c2]][[k]] }
		mean(time_ind_tmp)
	} 	
	
error_time = list()

for(q in 1:dim(toplot)[1]){

	n = toplot$n[q]
	c1 = toplot$c1[q]
	w = toplot$w[q]
	pcat = toplot$pcat[q]
	pind = toplot$pind[q]
	
	error_cat_stats_time<- foreach(c2=c2vals,.combine='cbind') %do% {
		error_cat_stats_time_tmp <-foreach(k = 1:sim_runs,.combine='rbind') %do%{
			error_cat[[n,c1,w,pcat,pind,c2]][[k]][,1:Tfights_min]
		} 
		rbind(colMeans(error_cat_stats_time_tmp,na.rm=TRUE),colSds(error_cat_stats_time_tmp,na.rm=TRUE))
	}
	c2 = c2vals[2]
	error_ind_stats_time_tmp <-foreach(k = 1:sim_runs,.combine='rbind') %do%{
			error_ind[[n,c1,w,pcat,pind,c2]][[k]][,1:Tfights_min]
		} 
	error_ind_stats_time<-rbind(colMeans(error_ind_stats_time_tmp,na.rm=TRUE),colSds(error_ind_stats_time_tmp,na.rm=TRUE))
	
	error = data.frame(error = c(error_cat_stats_time[1,],error_ind_stats_time[1,]),sd = 0.5*c(error_cat_stats_time[2,],error_ind_stats_time[2,]),categ = as.factor(c(rep('Categ',2*Tfights_min),rep('Indiv',Tfights_min))),fights=rep(1:Tfights_min,times=3),sigcorr=as.factor(c(rep(corr_vals[c2vals],each=Tfights_min),rep(corr_vals[c2],Tfights_min))))
	error_time[[q]] = error
	
	
}

Date <- Sys.Date()
# save(error_cat=error_cat,error_ind=error_ind,time_cat=time_cat,time_ind=time_ind,N_vals=N_vals,perc_vals=perc_vals,wind_vals=wind_vals,confus_cat_vals=confus_cat_vals,confus_ind_vals=confus_ind_vals,corr_vals=corr_vals,file=paste('/homes/ebrush/priv/badgevsrecog/badgevsrecog_paramsweep_par_',substr(Date,1,4),'_',substr(Date,6,7),'_',substr(Date,9,10),'.Rdata',sep=''))
save(confus_cat_vals,confus_ind_vals,corr_vals,d,error_cat_mean,error_ind_mean,N_vals,perc_vals,time_cat_mean,time_ind_mean,wind_vals,error_time,toplot,Tfights,sim_runs,observation_happens,file=paste('/homes/ebrush/priv/badgevsrecog/summary_stats_diffparams_',substr(Date,1,4),'_',substr(Date,6,7),'_',substr(Date,9,10),'.Rdata',sep=''))

quit()
