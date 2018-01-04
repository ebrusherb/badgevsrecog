source('/Users/eleanorbrush/Desktop/model.R')

Tfights = 5001 #total number of fights 
down_sample = 50
sim_runs = 10
confus_prob_cat = Inf #maximum probability of misidentifying categories, which decreases with dissimilarity
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
learn_noise = 0.1 #how noisy an updated assessment is
obs_noise = 0.1 # how noisy observational learning is
p_obs = 0 #probability of observing a fight you're not engaged in
observation_happens = TRUE

N = 50
delta = 0
learn_rate = 0.05

memory_window = 2000
rho = 0.5

d = foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{		
	L_temp = dynamics_cat() }
	
memory_window = 2000	
rho = 0.99

d2 = foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{		
	L_temp = dynamics_cat() }
	
memory_window = Inf	
rho = 0.99

d3 = foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{		
	L_temp = dynamics_cat() }
	
error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
 		d[[1]][[k]]};
m=colMeans(error_cat_tmp)

error_cat_tmp2 <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
 		d2[[1]][[k]]};
m2=colMeans(error_cat_tmp2)

error_cat_tmp3 <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
 		d3[[1]][[k]]};
m3=colMeans(error_cat_tmp3)	
