## --- cluster set up
library(parallel)
library(foreach)
library(doParallel)

# num_cores <- detectCores()-1
num_cores <-20
cl <-makeCluster(num_cores)
registerDoParallel(cl)

source('base_model.R')
source('ind2sub.R')
source('glue.R')



## ---- parameters -------------------------
Tfights = 10000 #total number of fights 
# N = 20 # individuals
# categor_num = 10 #number of categories to learn about
# memory_window = Inf #how many fights ago they can remember
# confus_prob_cat = 0.1 #maximum probability of misidentifying categories, which decreases with dissimilarity
# confus_prob_ind = 0 #probability of misidentifying individuals
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
# sig_qual_corr = 0.5 #correlation between quality and signal
learn_rate = 0.2 #how much the quality of the opponent affects the new assessment
learn_noise = 0.01 #how noisy an updated assessment is
dominance = 2 #how quickly the probability switches from A winning to A losing
sig_min = -1 ; sig_max = 1
error_threshold = 0.2

##---- parameter_sweep -----------------------
sim_runs = 50
N_vals = c(25,50,75,100)
xN = length(N_vals)
cat_vals = c(2,5,10,25)
xcat = length(cat_vals)
wind_vals = c(Inf,500,100,10)
xwind = length(wind_vals)
confus_cat_vals = c(0,0.05,0.1)
xconfus_cat = length(confus_cat_vals)
confus_ind_vals = c(0,0.05,0.1)
xconfus_ind = length(confus_ind_vals)
corr_vals = c(0.1,0.5,0.9)
xcorr = length(corr_vals)
d = c(xN,xcat,xwind,xconfus_cat,xconfus_ind,xcorr)
P = prod(d)

L <- foreach(ind = 1:P, .combine='glue',.multicombine=TRUE, .init=list(list(),list(),list(),list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list(),list(),list(),list())) %dopar%{
	v = ind2sub(d,ind)
	N = N_vals[v[1]]
	categor_num = cat_vals[v[2]]
	memory_window = wind_vals[v[3]]
	confus_prob_cat = confus_cat_vals[v[4]]
	confus_prob_ind = confus_ind_vals[v[5]]
	sig_qual_corr = corr_vals[v[6]]
	L_temp = dynamics() }

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

save(error_cat=error_cat,error_ind=error_ind,time_cat=time_cat,time_ind=time_ind,file='homes/ebrush/priv/badgevsrecog/paramsweep_par.Rdata')