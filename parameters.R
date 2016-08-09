# group size, perc window, window, confus cat, confus ind, corr, p obs
parameters = data.frame(N=c(),c1=c(),w=c(),pcat=c(),pind=c(),c2=c(),pobs=c())


N_vals = seq(20,100,by=10)
perc_vals = seq(0,2,by=0.2)
wind_vals = seq(250,2000,by=250)
confus_cat_vals = rev(c(100,500,1000,Inf))
confus_ind_vals = c(0,0.05,0.1,0.4)
corr_vals = c(0.5,0.9)

# N_vals = c(20,30)

xN = length(N_vals)
xperc = length(perc_vals)
xwind = length(wind_vals)
xconfus_cat = length(confus_cat_vals)
xconfus_ind = length(confus_ind_vals)
xcorr = length(corr_vals)

N = 50
c1 = perc_vals[3]
w = wind_vals[4]
pcat = confus_cat_vals[1]
pind = confus_ind_vals[1]
pobs = 0

#vary group size
parameters = rbind(parameters,data.frame(N=rep(N_vals,times=xcorr),c1=rep(c1,xN*xcorr),w=rep(w,xN*xcorr),pcat=rep(pcat,xN*xcorr),pind=rep(pind,xN*xcorr),c2=rep(corr_vals,each=xN),pobs = rep(pobs,xN*xcorr)))

# #vary memory window
parameters = rbind(parameters,data.frame(N=rep(N,xwind*xcorr),c1=rep(c1,xwind*xcorr),w=rep(wind_vals,times=xcorr),pcat=rep(pcat,xwind*xcorr),pind=rep(pind,xwind*xcorr),c2=rep(corr_vals,each=xwind),pobs=rep(pobs,xwind*xcorr)))

#vary perception window
parameters = rbind(parameters,data.frame(N=rep(N,xperc*xcorr),c1=rep(perc_vals,times=xcorr),w=rep(w,xperc*xcorr),pcat=rep(pcat,xperc*xcorr),pind=rep(pind,xperc*xcorr),c2=rep(corr_vals,each=xperc),pobs=rep(pobs,xperc*xcorr)))

#vary confusion probabilities
parameters = rbind(parameters,data.frame(N=rep(N,xconfus_cat*xcorr),c1=rep(c1,xconfus_cat*xcorr),w=rep(w,xconfus_cat*xcorr),pcat=rep(confus_cat_vals,times=xcorr),pind=rep(pind,xconfus_cat*xcorr),c2=rep(corr_vals,each=xconfus_cat),pobs=rep(pobs,xconfus_cat*xcorr)))

parameters = rbind(parameters,data.frame(N=rep(N,xconfus_ind*xcorr),c1=rep(c1,xconfus_ind*xcorr),w=rep(w,xconfus_ind*xcorr),pcat=rep(pcat,xconfus_ind*xcorr),pind=rep(confus_ind_vals,times=xcorr),c2=rep(corr_vals,each=xconfus_ind),pobs=rep(pobs,xconfus_ind*xcorr)))

#vary group size and memory window

parameters = rbind(parameters,data.frame(N=rep(N_vals,times=xwind*xcorr),c1=rep(c1,xN*xwind*xcorr),w=rep(wind_vals,each=xN,times=xcorr),pcat=rep(pcat,xN*xwind*xcorr),pind=rep(pind,xN*xwind*xcorr),c2=rep(corr_vals,each=xN*xwind),pobs=rep(pobs,xN*xwind*xcorr)))

#vary perception window and memory window 
parameters = rbind(parameters,data.frame(N=rep(N,xperc*xwind*xcorr),c1=rep(perc_vals,times=xwind*xcorr),w=rep(wind_vals,each=xperc,times=xcorr),pcat=rep(pcat,xperc*xwind*xcorr),pind=rep(pind,xperc*xwind*xcorr),c2=rep(corr_vals,each=xperc*xwind),pobs=rep(pobs,xperc*xwind*xcorr)))

parameters = rbind(parameters,c(20,1.4,1500,Inf,0,0.5,0))

#remove redundancies
facs=with(parameters,interaction(N,c1,w,pcat,pind,c2,pobs,sep=','))
parameters=aggregate(parameters,by=list(facs),FUN=mean)
facs = unique(facs)