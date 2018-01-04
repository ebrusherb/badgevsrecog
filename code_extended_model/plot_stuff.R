############
# par(mfrow=c(1,3))
# I=I+12;
# i=I;
# plot(cost_cat[,i],ylim=range(cost_cat,na.rm=TRUE));abline(v=best_cat[i]);
# points(cost_cat[,i+3],col='red');abline(v=best_cat[i+3],col='red');
# points(cost_cat[,i+6],col='blue');abline(v=best_cat[i+6],col='blue');
# points(cost_cat[,i+9],col='green');abline(v=best_cat[i+9],col='green');
# legend(200,0.2,parameters_cat[c(i,i+3,i+6,i+9),]$l,lty=1,col=c('black','red','blue','green'));
# i=I+1;
# plot(cost_cat[,i],ylim=range(cost_cat,na.rm=TRUE));abline(v=best_cat[i],col='black');
# points(cost_cat[,i+3],col='red');abline(v=best_cat[i+3],col='red');
# points(cost_cat[,i+6],col='blue');abline(v=best_cat[i+6],col='blue');
# points(cost_cat[,i+9],col='green');abline(v=best_cat[i+9],col='green');
# i=I+2;
# plot(cost_cat[,i],ylim=range(cost_cat,na.rm=TRUE));abline(v=best_cat[i],col='black');
# points(cost_cat[,i+3],col='red');abline(v=best_cat[i+3],col='red');
# points(cost_cat[,i+6],col='blue');abline(v=best_cat[i+6],col='blue');
# points(cost_cat[,i+9],col='green');abline(v=best_cat[i+9],col='green');
# parameters_cat[I,]

#######
# par(mfrow=c(1,3))
# I=1;
# i=I;
# plot(cost_cat[,i],ylim=range(cost_cat,na.rm=TRUE),xlim=c(0,200));abline(v=best_cat[i]);
# points(cost_cat[,i+12],col=rainbow(9)[1]);abline(v=best_cat[i+12],col=rainbow(9)[1]);
# points(cost_cat[,i+24],col=rainbow(9)[2]);abline(v=best_cat[i+24],col=rainbow(9)[2]);
# points(cost_cat[,i+36],col=rainbow(9)[3]);abline(v=best_cat[i+36],col=rainbow(9)[3]);
# points(cost_cat[,i+48],col=rainbow(9)[4]);abline(v=best_cat[i+48],col=rainbow(9)[4]);
# legend(100,0.3,parameters_cat[c(I,I+12,I+24,I+36,I+48),]$delta,lty=1,col=c('black',rainbow(9)[1:4]));
# i=I+1;
# plot(cost_cat[,i],ylim=range(cost_cat,na.rm=TRUE),xlim=c(0,200));abline(v=best_cat[i],col='black');
# points(cost_cat[,i+12],col=rainbow(9)[1]);abline(v=best_cat[i+12],col=rainbow(9)[1]);
# points(cost_cat[,i+24],col=rainbow(9)[2]);abline(v=best_cat[i+24],col=rainbow(9)[2]);
# points(cost_cat[,i+36],col=rainbow(9)[3]);abline(v=best_cat[i+36],col=rainbow(9)[3]);
# points(cost_cat[,i+48],col=rainbow(9)[4]);abline(v=best_cat[i+48],col=rainbow(9)[4]);
# i=I+2;
# plot(cost_cat[,i],ylim=range(cost_cat,na.rm=TRUE),xlim=c(0,200));abline(v=best_cat[i],col='black');
# points(cost_cat[,i+12],col=rainbow(9)[1]);abline(v=best_cat[i+12],col=rainbow(9)[1]);
# points(cost_cat[,i+24],col=rainbow(9)[2]);abline(v=best_cat[i+24],col=rainbow(9)[2]);
# points(cost_cat[,i+36],col=rainbow(9)[3]);abline(v=best_cat[i+36],col=rainbow(9)[3]);
# points(cost_cat[,i+48],col=rainbow(9)[4]);abline(v=best_cat[i+48],col=rainbow(9)[4]);
# parameters_cat[I,]

#####
# N = 25
# rho = 0.5
# w = 250

# find = intersect(which(parameters_cat$w==w),intersect(which(parameters_cat$N==N),which(parameters_cat$rho==rho)))
# image(matrix(time_optimized_cat[find],nrow=xlearn),breaks=seq(0,1,length.out=10),col=brewer.pal(9,'YlOrRd'))

# ######
# # par(mfrow=c(1,3))
# t = 400
# ylim=range(error_cat,na.rm=TRUE)
# plot(parameters_cat$delta,parameters_cat$delta,ylim=ylim,t='n')
# icol = 0
# for(i in 1:length(unique(parameters_cat$N))){
	# for(j in 2){
		# for(k in 1:length(unique(parameters_cat$w))){
			# for(q in 1:length(unique(parameters_cat$l))){
			# N = unique(parameters_cat$N)[i]
			# rho = unique(parameters_cat$rho)[j]
			# w = unique(parameters_cat$w)[k]
			# l = unique(parameters_cat$l)[q]
			# find = intersect(intersect(which(parameters_cat$w==w),intersect(which(parameters_cat$N==N),which(parameters_cat$rho==rho))),which(parameters_cat$l==l))
			# points(parameters_cat$delta[find],error_cat[t,find],col=rainbow(4)[q],t='o',lwd=i,lty=(2:1)[j],pch=(1:3)[k])
			# icol = (icol+1)%%9
			# }
		# }
	# }
# }
# legend(0.1,ylim[1]+0.15*diff(ylim),unique(parameters_cat$N),lty=1,lwd=(1:3),bty='n')
# legend(0.3,ylim[1]+0.15*diff(ylim),unique(parameters_cat$rho),lty=2:1,bty='n')
# legend(0.5,ylim[1]+0.25*diff(ylim),unique(parameters_cat$w),lty=1,pch=1:3,bty='n')
# legend(0.7,ylim[1]+0.15*diff(ylim),unique(parameters_cat$l),lty=1,col=rainbow(4),bty='n')


# # # ylim=range(best_cat,na.rm=TRUE)
# # plot(parameters_cat$delta,parameters_cat$delta,ylim=ylim,t='n')
# # icol = 0
# # for(i in 1:length(unique(parameters_cat$N))){
	# # for(j in 2){
		# # for(k in 1:length(unique(parameters_cat$w))){
			# # for(q in 1:length(unique(parameters_cat$l))){
			# # N = unique(parameters_cat$N)[i]
			# # rho = unique(parameters_cat$rho)[j]
			# # w = unique(parameters_cat$w)[k]
			# # l = unique(parameters_cat$l)[q]
			# # find = intersect(intersect(which(parameters_cat$w==w),intersect(which(parameters_cat$N==N),which(parameters_cat$rho==rho))),which(parameters_cat$l==l))
			# # points(parameters_cat$delta[find],best_cat[find],col=rainbow(4)[q],t='o',lwd=i,lty=(2:1)[j],pch=(1:3)[k])
			# # icol = (icol+1)%%9
			# # }
		# # }
	# # }
# # }

# # ylim=range(time_optimized_cat,na.rm=TRUE)
# # plot(parameters_cat$delta,parameters_cat$delta,ylim=ylim,t='n')
# # icol = 0
# # for(i in 1:length(unique(parameters_cat$N))){
	# # for(j in 2){
		# # for(k in 1:length(unique(parameters_cat$w))){
			# # for(q in 1:length(unique(parameters_cat$l))){
			# # N = unique(parameters_cat$N)[i]
			# # rho = unique(parameters_cat$rho)[j]
			# # w = unique(parameters_cat$w)[k]
			# # l = unique(parameters_cat$l)[q]
			# # find = intersect(intersect(which(parameters_cat$w==w),intersect(which(parameters_cat$N==N),which(parameters_cat$rho==rho))),which(parameters_cat$l==l))
			# # points(parameters_cat$delta[find],time_optimized_cat[find],col=rainbow(4)[q],t='o',lwd=i,lty=(2:1)[j],pch=(1:3)[k])
			# # icol = (icol+1)%%9
			# # }
		# # }
	# # }
# # }


# ##################
# facs=with(parameters_cat,interaction(delta,w,N,rho,sep=','))
# facs = apply(matrix(facs,nrow=1),2,as.character)
# l = l+3
# j = which(facs==unique(facs)[l])

# time = seq(1,fixed_parameters$Tfights,by=fixed_parameters$down_sample)

# plot(time,time,t='n',ylim=range(c(range(error_cat_short),range(error_cat))))
# title(paste('N=',parameters_cat$N[j[1]],'rho=',parameters_cat$rho[j[1]],'w=',parameters_cat$w[j[1]],'delta=',parameters_cat$delta[j[1]]))
# for(k in 1:length(j)){
	# lines(time,error_cat[,j[k]],col=rainbow(4)[k],lty=1,lwd=2)
# }

# legend(20000,0.5,legend=unique(parameters_cat$l),lty=1,col=rainbow(4),lwd=2,bty='n')

#################
rho=0.99
good = which(parameters_cat$rho==rho)
time = seq(1,fixed_parameters$Tfights,by=fixed_parameters$down_sample)

par(mfrow=c(2,5))

tmax=5000
L = 3

facs=with(parameters_cat[good,],interaction(w,N,rho,sep=','))
facs = apply(matrix(facs,nrow=1),2,as.character)
now = which(facs==facs[L])

good_rule = which(parameters_rule$rho==rho)
facs_rule = with(parameters_rule[good_rule,],interaction(w,N,rho,sep=','))
facs_rule = apply(matrix(facs_rule,nrow=1),2,as.character)
now_rule= which(facs_rule==facs[L])

for(q in 1:4){
	j = now[c(1,5,9,13,17)+q-1]
	plot(time,time,t='n',ylim=range(c(range(error_cat),range(error_cat))),xlim=c(0,tmax))
	title(paste('N=',parameters_cat$N[good[j[1]]],'rho=',parameters_cat$rho[good[j[1]]],'w=',parameters_cat$w[good[j[1]]],'l=',parameters_cat$l[good[j[1]]]))
	for(k in 1:length(j)){
		lines(time,error_cat[,good[j[k]]],col=rainbow(5)[k],lty=1,lwd=2)
	}
	if(length(now_rule)>0){
	lines(time,error_rule[,good_rule[now_rule]],col='black',lty=1,lwd=2)
	}
}
legend(500,0.55,legend=unique(parameters_cat$delta),lty=1,col=rainbow(5),lwd=2,bty='n')
plot(time,time,t='n')


for(q in 1:5){
	j = now[1:4+(q-1)*4]
	plot(time,time,t='n',ylim=range(c(range(error_cat),range(error_cat))),xlim=c(0,tmax))
	title(paste('N=',parameters_cat$N[good[j[1]]],'rho=',parameters_cat$rho[good[j[1]]],'w=',parameters_cat$w[good[j[1]]],'delta=',parameters_cat$delta[good[j[1]]]))
	for(k in 1:length(j)){
		lines(time,error_cat[,good[j[k]]],col=rainbow(5)[k],lty=1,lwd=2)
	}
	if(length(now_rule)>0){
	lines(time,error_rule[,good_rule[now_rule]],col='black',lty=1,lwd=2)
	}
}
legend(500,0.55,legend=unique(parameters_cat$l),lty=1,col=rainbow(5),lwd=2,bty='n')

# # good = which(parameters_cat$rho==0.9)
# time = seq(1,fixed_parameters$Tfights,by=fixed_parameters$down_sample)

# par(mfrow=c(2,4))

# for(L in seq(1,420,by=60)){

# facs=with(parameters_cat[good,],interaction(w,N,rho,sep=','))
# facs = apply(matrix(facs,nrow=1),2,as.character)
# now = which(facs==facs[L])
# levelplot(matrix(error_cat[t,now],nrow=4),at=my.at,col.regions=heat.colors(k),colorkey=list(at=my.at,col=(heat.colors(k))))
# }

