setwd('/Users/eleanorbrush/Desktop/code_extension')
load('/Users/eleanorbrush/Desktop/noise=0.1/all_parameters.Rdata')
library(changepoint)
Tfights = fixed_parameters$Tfights
down_sample = fixed_parameters$down_sample
time = seq(1,fixed_parameters$Tfights,by=fixed_parameters$down_sample)
xN_cat = length(unique(parameters_cat$N))
xdelta_cat = length(unique(parameters_cat$delta))
xl_cat = length(unique(parameters_cat$l))
xw_cat = length(unique(parameters_cat$w))
xrho_cat = length(unique(parameters_cat$rho))

find <- function(N=NA,rho=NA,w=NA,delta=NA,l=NA,p=parameters_cat){
	params = unlist(as.list(environment())[1:5])
	provided = which(!is.na(params))
	vec = 1:dim(p)[1]
	for(i in provided){
		now = names(params)[i]
		vec = intersect(vec,which(p[[now]]==params[i]))
	}
	return(vec)
}

myround <-function(x,digits){
	round(x*10^digits)/10^digits
}

moving_window <- function(v,k=5){
	v2 = array(NA,dim=c(length(v),1))
	for(i in 1:length(v)){
		v2[i] = mean(v[i+(max(-floor(k/2),0):min(floor((k-1)/2),length(v)))],na.rm=TRUE)
	}
	return(v2)
}

find_changes <- function(s){
	which(diff(s)!=0)+1
}

find_changes <- function(s){
	# intersect(which(diff(s)!=0),intersect(intersect(which(c(diff(s)[2:(length(s)-1)],0)==0),which(c(diff(s)[3:(length(s)-1)],0,0)==0)),intersect(which(c(0,diff(s)[1:(length(s)-2)])==0),which(c(0,0,diff(s)[1:(length(s)-3)])==0))))+1
	intersect(which(diff(s)!=0),intersect(which(c(diff(s)[2:(length(s)-1)],0)==0),which(c(0,diff(s)[1:(length(s)-2)])==0)))+1
}

w=0.8

cost_cat = apply(error_cat,2,function(v) 2*w*v+(1-w)*time/30001)

# start_learning = 6
# cpts_cat = as.vector(apply(cost_cat,2,function(v) cpt.mean(diff(v[start_learning:length(time)]))@cpts[1]+start_learning))
# mean_cat =  (apply(cost_cat,2,function(v) cpt.mean(diff(v[start_learning:length(time)]))@param.est$mean))

# # best_cat  = array(NA,dim(cost_cat)[2])
# for(i in 1:dim(cost_cat)[2]){
	# if(cpts_cat[i]==length(time)){
		# if(mean_cat[[i]][1]<0){
			# best_cat[i] = length(time)
		# }else{
			# best_cat[i] = start_learning
		# }
	# }else{
		# if(mean_cat[[i]][1]>0){
			# best_cat[i] = start_learning
		# }else if(mean_cat[[i]][2]>0){
			# best_cat[i] = cpts_cat[i]
		# }else{
			# best_cat[i] = length(time)
		# }		
		# }
# }

# best_cat = array(NA,dim(parameters_cat)[1])
# k_vec = rep(10,dim(parameters_cat)[1])
# start = 3
# for(i in 1:dim(parameters_cat)[1]){
	# d = c(rep(NA,start-1),diff(cost_cat[start:length(time),i]))
	# k = 40
	# d_avg = moving_window(d,k)
	# s = sign(round(d_avg,3))
	# f = find_changes(s)
	# # while(length(f)>2){
		# # k=k+5
		# # d_avg = moving_window(d,k)
		# # s =  sign(round(d_avg,3))
		# # f = find_changes(s)
	# # }
	# # k_vec[i] = k
	# if(length(f)==0 || length(f)>3){
		# if(max(s,na.rm=TRUE)>(-1)){
			# best_cat[i] = 1
		# } else{
			# best_cat[i] = length(time)
		# }
	# }else {
		# if(max(s[f])==-1){
			# best_cat[i] = length(time)
		# }else{
			# best_cat[i] = max(f[which(s[f]>(-1))])
		# }
	# }
# }

best_cat = array(NA,dim(parameters_cat)[1])
start = 3
start2 = 10
for(i in 1:dim(parameters_cat)[1]){
	d = c(rep(NA,start-1),diff(cost_cat[start:length(time),i]))
	k = 70
	d_avg = moving_window(d,k)
	if((diff(range(error_cat[start2:dim(cost_cat)[1],i],na.rm=TRUE)))>0.1){
		offset = 4
		s = sign(myround(d_avg,3.5))
		}else{
		offset = 10
		s = sign(myround(d_avg,3))	
		}
	s[s==0] = 1
	z = apply(matrix(1:length(s),ncol=1),1,function(x) identical(s[(max(0,x-offset):min(length(s),x+offset))],rep(s[x],offset*2+1)))
	if(length(s[z])>1){
		f = which(z)[find_changes(s[z])]-offset
	}else{
		f = NULL
	}	
	if(length(f)==0 || length(f)>3){
		if(sum(z)==0 || max(s[z],na.rm=TRUE)>(-1)){
			best_cat[i] = start2
		} else{
			best_cat[i] = length(time)
		}
	}else {
		if(max(s[f])==-1){
			best_cat[i] = length(time)
		}else{
			best_cat[i] = min(f[which(s[f]>(-1))])
		}
	}
}


time_optimized_cat = apply(matrix(c(best_cat,1:dim(parameters_cat)[1]),nrow=2,byrow=TRUE),2,function(x) cost_cat[x[1],x[2]])

layout(matrix(1:8,nrow=2))

var = 'N'
f = rbind(find(rho=0.5,delta=0,l=0.5,w=1000),
	find(rho=0.9,delta=0,l=0.5,w=Inf),
	find(rho=0.99,delta=0,l=0.25,w=Inf))

plot(parameters_cat[[var]],time_optimized_cat,t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time_optimized_cat[f[i,]],t='o',col=rainbow(3)[i])
}

plot(parameters_cat[[var]],time[best_cat],t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time[best_cat[f[i,]]],t='o',col=rainbow(3)[i])
}

var='delta'
f = rbind(find(rho=0.5,N=25,l=0.5,w=1000),
	find(rho=0.9,N=25,l=0.5,w=Inf),
	find(rho=0.99,N=50,l=0.25,w=Inf))

plot(parameters_cat[[var]],time_optimized_cat,t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time_optimized_cat[f[i,]],t='o',col=rainbow(3)[i])
}

plot(parameters_cat[[var]],time[best_cat],t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time[best_cat[f[i,]]],t='o',col=rainbow(3)[i])
}

var='l'
f = rbind(find(rho=0.5,N=25,delta=0,w=1000),
	find(rho=0.9,N=25,delta=0,w=Inf),
	find(rho=0.99,N=50,delta=0.25,w=Inf))

plot(parameters_cat[[var]],time_optimized_cat,t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time_optimized_cat[f[i,]],t='o',col=rainbow(3)[i])
}

plot(parameters_cat[[var]],time[best_cat],t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time[best_cat[f[i,]]],t='o',col=rainbow(3)[i])
}

var='w'
f = rbind(find(rho=0.5,N=25,delta=0,l=0.1),
	find(rho=0.9,N=25,delta=0,l=0.5),
	find(rho=0.9,N=50,delta=0.25,l=0.5))

plot(parameters_cat[[var]],time_optimized_cat,t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time_optimized_cat[f[i,]],t='o',col=rainbow(3)[i])
}

plot(parameters_cat[[var]],time[best_cat],t='n',xlab=var)
for(i in 1:dim(f)[1]){
	points(parameters_cat[[var]][f[i,]],time[best_cat[f[i,]]],t='o',col=rainbow(3)[i])
}



# for(i in 1:2){
	# for(j in 1:xdelta_cat){
		# for(k in 1:xl_cat){
			# for(q in 1:xw_cat){
				# f = find(rho=unique(parameters_cat$rho)[i],delta=unique(parameters_cat$delta)[j],l=unique(parameters_cat$l)[k],w=unique(parameters_cat$w)[q])
				# points(parameters_cat$N[f],time_optimized_cat[f])
			# }
		# }
	# }
# }

# # N = 25
# # rho = 0.99

# # find = intersect(which(parameters_cat$N==N),which(parameters_cat$rho==rho))
# # optimal_params = parameters_cat[find[which.min(time_optimized_cat[find])],]
# # print(data.frame(optimal_params,time=time[best_cat[find[which.min(time_optimized_cat[find])]]]))

# # ###
# # rho=0.9
# # good = which(parameters_cat$rho==rho)

# ####
# # par(mfrow=c(2,5))

# # tmax=2000
# # L = 2

# # facs=with(parameters_cat[good,],interaction(w,N,rho,sep=','))
# # facs = apply(matrix(facs,nrow=1),2,as.character)
# # now = which(facs==facs[L])

# # good_rule = which(parameters_rule$rho==rho)
# # facs_rule = with(parameters_rule[good_rule,],interaction(w,N,rho,sep=','))
# # facs_rule = apply(matrix(facs_rule,nrow=1),2,as.character)
# # now_rule= which(facs_rule==facs[L])

# # for(q in 1:4){
	# # j = now[c(1,5,9,13,17)+q-1]
	# # plot(time,time,t='n',ylim=range(cost_cat,na.rm=TRUE),xlim=c(0,tmax))
	# # title(paste('N=',parameters_cat$N[good[j[1]]],'rho=',parameters_cat$rho[good[j[1]]],'w=',parameters_cat$w[good[j[1]]],'l=',parameters_cat$l[good[j[1]]]))
	# # for(k in 1:length(j)){
		# # lines(time,cost_cat[,good[j[k]]],col=rainbow(5)[k],lty=1,lwd=2)
		# # abline(v=time[best_cat[j[k]]],col=rainbow(5)[k])
	# # }
	# # if(length(now_rule)>0){
	# # lines(time,error_rule[,good_rule[now_rule]],col='black',lty=1,lwd=2)
	# # }
# # }
# # legend(500,0.55,legend=unique(parameters_cat$delta),lty=1,col=rainbow(5),lwd=2,bty='n')
# # plot(time,time,t='n')


# # for(q in 1:5){
	# j = now[1:4+(q-1)*4]
	# plot(time,time,t='n',ylim=range(cost_cat,na.rm=TRUE),xlim=c(0,tmax))
	# title(paste('N=',parameters_cat$N[good[j[1]]],'rho=',parameters_cat$rho[good[j[1]]],'w=',parameters_cat$w[good[j[1]]],'delta=',parameters_cat$delta[good[j[1]]]))
	# for(k in 1:length(j)){
		# lines(time,cost_cat[,good[j[k]]],col=rainbow(5)[k],lty=1,lwd=2)
		# abline(v=time[best_cat[j[k]]],col=rainbow(5)[k])
	# }
	# if(length(now_rule)>0){
	# lines(time,error_rule[,good_rule[now_rule]],col='black',lty=1,lwd=2)
	# }
# # }
# # legend(500,0.55,legend=unique(parameters_cat$l),lty=1,col=rainbow(5),lwd=2,bty='n')


# # ###
# # par(mfrow=c(2,2))

# # tmax=2000
# # L = 361

# # facs=with(parameters_cat[good,],interaction(w,N,rho,sep=','))
# # facs = apply(matrix(facs,nrow=1),2,as.character)
# # now = which(facs==facs[L])

# # good_rule = which(parameters_rule$rho==rho)
# # facs_rule = with(parameters_rule[good_rule,],interaction(w,N,rho,sep=','))
# # facs_rule = apply(matrix(facs_rule,nrow=1),2,as.character)
# # now_rule= which(facs_rule==facs[L])

# # for(q in 1:4){
	# # j = now[c(1,5,9,13,17)+q-1]
	# # plot(time,time,t='n',ylim=range(cost_cat,na.rm=TRUE),xlim=c(0,tmax))
	# # title(paste('N=',parameters_cat$N[good[j[1]]],'rho=',parameters_cat$rho[good[j[1]]],'w=',parameters_cat$w[good[j[1]]],'l=',parameters_cat$l[good[j[1]]]))
	# # for(k in 1:length(j)){
		# # lines(time,cost_cat[,good[j[k]]],col=rainbow(5)[k],lty=1,lwd=2)
		# # abline(v=time[best_cat[j[k]]],col=rainbow(5)[k])
	# # }
	# # if(length(now_rule)>0){
	# # lines(time,error_rule[,good_rule[now_rule]],col='black',lty=1,lwd=2)
	# # }
# # }
# # legend(700,1.2,legend=unique(parameters_cat$delta),lty=1,col=rainbow(5),lwd=2,bty='n')
