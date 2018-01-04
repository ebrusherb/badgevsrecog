library(foreach)
source('glue.R')

# group size, category width, memory window, rho
parameters = data.frame(N=c(),rho=c(),w=c(),delta=c(),l=c())

N_vals = 100
rho_vals = 0.9
wind_vals = 2500
delta_vals = seq(0,0.25,by=0.25)
learn_rate_vals = c(0.25,0.5)

xN = length(N_vals)
xrho = length(rho_vals)
xwind = length(wind_vals)
xdelta = length(delta_vals)
xlearn = length(learn_rate_vals)

for(l in 1:xrho){
	for(k in 1:xwind){
		for(j in 1:xdelta){		
			for(m in 1:xlearn){
				for(i in 1:xN){
					ind = sub2ind(c(xN,xdelta,xwind,xrho,xlearn),c(i,j,k,l,m))
					parameters = rbind(parameters,data.frame(N=N_vals[i],rho=rho_vals[l],w=wind_vals[k],delta=delta_vals[j],l=learn_rate_vals[m]))
				}
			}
		}
	}
}

## ---- parameters -------------------------
Tfights = 5001 #total number of fights 
down_sample = 25
sim_runs = 4
confus_prob_cat = Inf #maximum probability of misidentifying categories, which decreases with dissimilarity
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
learn_noise = 0.1 #how noisy an updated assessment is
obs_noise = 0.1 # how noisy observational learning is
p_obs = 0 #probability of observing a fight you're not engaged in
observation_happens = TRUE

xparam = dim(parameters)[1]

# error_cat_full <- foreach(i = 1:xparam, .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{	
	# N = parameters$N[i]
	# delta  = parameters$delta[i]
	# memory_window = parameters$w[i]
	# rho = parameters$rho[i]
	# learn_rate = parameters$l[i]
	# obs_learn_rate = learn_rate - 0.1
	# L_temp = dynamics_cat() }
# error_cat_full = error_cat_full[[1]]
	
# error_cat_max_full <- foreach(i = 1:xparam, .combine='glue',.multicombine=TRUE, .init=list(list())) %:% foreach(t = 1:sim_runs, .combine='glue',.multicombine=TRUE, .init=list(list())) %do%{	
	# N = parameters$N[i]
	# delta  = parameters$delta[i]
	# memory_window = parameters$w[i]
	# rho = parameters$rho[i]
	# learn_rate = parameters$l[i]
	# obs_learn_rate = learn_rate - 0.1
	# L_temp = dynamics_cat_max() }
# error_cat_max_full = error_cat_max_full[[1]]

# error_cat_mean<- foreach(p=1:xparam,.combine='cbind') %do% {
		# error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		# error_cat_full[[p]][[k]]}
		# colMeans(error_cat_tmp,na.rm=TRUE)
	# }
	
# error_cat_max_mean<- foreach(p=1:xparam,.combine='cbind') %do% {
		# error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		# error_cat_max_full[[p]][[k]]}
		# colMeans(error_cat_tmp,na.rm=TRUE)
	# }	
	
# error_cat_sd<- foreach(p=1:xparam,.combine='cbind') %do% {
		# error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		# error_cat_full[[p]][[k]]}
		# colSds(error_cat_tmp,na.rm=TRUE)
	# }
	
# error_cat_max_sd<- foreach(p=1:xparam,.combine='cbind') %do% {
		# error_cat_tmp <- foreach(k=1:sim_runs,.combine='rbind') %do% {		
		# error_cat_max_full[[p]][[k]]}
		# colSds(error_cat_tmp,na.rm=TRUE)
	# }	
	
time = seq(1,Tfights,by=down_sample)	

# par(mfrow=c(2,1))
# ylim=range(c(range(error_cat_mean[length(time),],na.rm=TRUE),range(error_cat_max_mean[length(time),],na.rm=TRUE)))
# plot(parameters$delta,parameters$delta,ylim=ylim,t='n')
# icol = 0
# for(i in 1:length(unique(parameters$N))){
	# for(j in 1:length(unique(parameters$rho))){
		# for(k in 1:length(unique(parameters$w))){
			# for(q in 1:length(unique(parameters$l))){
			# N = unique(parameters$N)[i]
			# rho = unique(parameters$rho)[j]
			# w = unique(parameters$w)[k]
			# l = unique(parameters$l)[q]
			# find = intersect(intersect(which(parameters$w==w),intersect(which(parameters$N==N),which(parameters$rho==rho))),which(parameters$l==l))
			# points(parameters$delta[find],error_cat_mean[length(time),find],col=rainbow(4)[q],t='o',lwd=2*i,lty=(1:2)[j],pch=(1:3)[k])
			# icol = (icol+1)%%9
			# }
		# }
	# }
# }

# plot(parameters$delta,parameters$delta,ylim=ylim,t='n')
# icol = 0
# for(i in 1:length(unique(parameters$N))){
	# for(j in 1:length(unique(parameters$rho))){
		# for(k in 1:length(unique(parameters$w))){
			# for(q in 1:length(unique(parameters$l))){
			# N = unique(parameters$N)[i]
			# rho = unique(parameters$rho)[j]
			# w = unique(parameters$w)[k]
			# l = unique(parameters$l)[q]
			# find = intersect(intersect(which(parameters$w==w),intersect(which(parameters$N==N),which(parameters$rho==rho))),which(parameters$l==l))
			# points(parameters$delta[find],error_cat_max_qmean[length(time),find],col=rainbow(4)[q],t='o',lwd=2*i,lty=(1:2)[j],pch=(1:3)[k])
			# icol = (icol+1)%%9
			# }
		# }
	# }
# }

# # par(mfrow=c(2,1))
# ylim=range(c(range(error_cat_mean,na.rm=TRUE)))
# ylim=c(0,0.7)
# plot(time,time,ylim=ylim,t='n')

# k = 1:2

# for(k in k){
	# for(q in 1:length(unique(parameters$l))){
	# delta = unique(parameters$delta)[k]				
	# l = unique(parameters$l)[q]
	# find = intersect(which(parameters$delta==delta),which(parameters$l==l))
	# lines(time,error_cat_mean[,find],col=rainbow(3)[q],t='l',lwd=2,lty=(1:3)[k])
	# lines(time,error_cat_mean[,find]+error_cat_sd[,find],col=rainbow(3)[q],t='l',lwd=1,lty=(1:3)[k])
	# lines(time,error_cat_mean[,find]-error_cat_sd[,find],col=rainbow(3)[q],t='l',lwd=1,lty=(1:3)[k])
	# }
# }

# ylim=range(c(range(error_cat_max_mean,na.rm=TRUE)))
# plot(time,time,ylim=ylim,t='n')

# for(k in k){
	# for(q in 1:length(unique(parameters$l))){
	# delta = unique(parameters$delta)[k]				
	# l = unique(parameters$l)[q]
	# find = intersect(which(parameters$delta==delta),which(parameters$l==l))
	# lines(time,error_cat_max_mean[,find],col=rainbow(3)[q],t='o',lwd=2,lty=(1:3)[k])
	# }
# }

par(mfrow=c(2,1))
plot(time,time,ylim=range(error_cat_max_full,na.rm=TRUE),t='n')
delta =0
for(l in 2:1){
find = intersect(which(parameters$delta==delta),which(parameters$l==unique(parameters$l)[l]))
for(j in 1:sim_runs){
	for(k in 1:N){
		points(time,error_cat_max_full[[find]][[j]][k,],col=rainbow(3)[l+1],t='o')
	}
}
}	

plot(time,time,ylim=range(error_cat_max_full,na.rm=TRUE),t='n')
delta =0.25
for(l in 2:1){
find = intersect(which(parameters$delta==delta),which(parameters$l==unique(parameters$l)[l]))
for(j in 1:sim_runs){
	for(k in 1:N){
		points(time,error_cat_max_full[[find]][[j]][k,],col=rainbow(3)[l+1],t='o')
	}
}
}	