load("/Users/eleanorbrush/Desktop/noise=0.1_hold/all_parameters.Rdata")
error_cat_short = error_cat
error_genetic_short = error_genetic
error_rule_short = error_rule
fixed_parameters_short = fixed_parameters
parameters_cat_short = parameters_cat
parameters_genetic_short = parameters_genetic
parameters_rule = parameters_rule

load('/Users/eleanorbrush/Desktop/noise=0.1/all_parameters.Rdata')

facs=with(parameters_cat,interaction(delta,w,N,rho,sep=','))
facs_short=with(parameters_cat_short,interaction(delta,w,N,rho,sep=','))
facs = apply(matrix(facs,nrow=1),2,as.character)
facs_short = apply(matrix(facs_short,nrow=1),2,as.character)
l = l+3
i = which(facs_short==unique(facs_short)[l])
j = which(facs==unique(facs)[l])

time = seq(1,fixed_parameters$Tfights,by=fixed_parameters$down_sample)
time_short =seq(1,fixed_parameters_short$Tfights,by=fixed_parameters_short$down_sample)

plot(time,time,t='n',ylim=range(c(range(error_cat_short),range(error_cat))))
title(paste('N=',parameters_cat$N[j[1]],'rho=',parameters_cat$rho[j[1]],'w=',parameters_cat$w[j[1]],'delta=',parameters_cat$delta[j[1]]))
for(k in 1:length(j)){
	lines(time_short,error_cat_short[,i[k]],col=rainbow(4)[k],lwd=2,lty=2)
	lines(time,error_cat[,j[k]],col=rainbow(4)[k],lty=1,lwd=2)
}

legend(20000,0.5,legend=unique(parameters_cat$l),lty=1,col=rainbow(4),lwd=2,bty='n')