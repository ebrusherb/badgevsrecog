library(ggplot2)
library(RColorBrewer)
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)
source('multiplot.R')
source('ind2sub.R')

mypal=brewer.pal(9,'Set1')
divpal = brewer.pal(5,'RdBu')

xN = length(N_vals)

xperc = length(perc_vals)

xwind = length(wind_vals)

xconfus_cat = length(confus_cat_vals)

xconfus_ind = length(confus_ind_vals)

xcorr = length(corr_vals)

d = c(xN,xperc,xwind,xconfus_cat,xconfus_ind,xcorr)
P = prod(d)

sim_runs = length(error_cat[[1,1,1,1,1,1]])
Tfights = dim(error_cat[[1,1,1,1,1,1]][[1]])
Tfights = Tfights[2]
Tfights_min = 1000

##---- error as a function of group size and perc vals
plots=list()
	#fix perception window and vary the other parameters 
	c1 = 2
	error_cat_now<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='cbind') %do% {
		error_cat_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xN,xwind,xcorr),p)
		n = v[1]
		w = v[2]
		c2 = v[3]
		error_cat[[n,c1,w,1,1,c2]][[k]][,Tfights]	}
		c(mean(error_cat_temp,na.rm=TRUE),sd(error_cat_temp,na.rm=TRUE))
	}
	error_cat_now = unname(error_cat_now)
	error_cat_mean = error_cat_now[1,]
	error_cat_sd = error_cat_now[2,]
	error_ind_now<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='cbind') %do% {
		error_ind_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xN,xwind,xcorr),p)
		n = v[1]
		w = v[2]
		c2 = v[3]
		error_ind[[n,c1,w,1,1,c2]][[k]][,Tfights]	}
		c(mean(error_ind_temp,na.rm=TRUE),sd(error_ind_temp,na.rm=TRUE))
	}
	error_ind_now = unname(error_ind_now)
	error_ind_mean = error_ind_now[1,]
	error_ind_sd = error_ind_now[2,]
	
	error = data.frame(categ = error_cat_mean, indiv=error_ind_mean, groupsize=rep(N_vals,times=xwind*xcorr), window = as.factor(rep(rep(wind_vals,each=xN),times=xcorr)),  sigcorr = rep((0:(xcorr-1))*xwind,each=xN*xcorr))
	corrandtype = c(error$sigcorr/2,error$sigcorr/2+2)
	corrandtype[corrandtype==0] = paste('Categ, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==1] = paste('Categ, ','Corr = ',corr_vals[2],sep='')
	corrandtype[corrandtype==2] = paste('Indiv, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==3] = paste('Indiv, ','Corr = ',corr_vals[2],sep='')
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),groupsize=rep(groupsize,2),corrandtype = as.factor(corrandtype),window = rep(window,2)))
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =error, colour = corrandtype, linetype = window)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		scale_color_manual(values=divpal[c(1,2,5,4)])+
		labs(linetype="Window", colour="") + 
		xlab("Group size")+ylab("Error") +
		ggtitle(paste( 'Perception window = ',perc_vals[c1],sep=""))
		
		#fix group size and vary the other parameters
		n=2
		
	error_cat_now<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='cbind') %do% {
		error_cat_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xperc,xwind,xcorr),p)
		c1 = v[1]
		w = v[2]
		c2 = v[3]
		error_cat[[n,c1,w,1,1,c2]][[k]][,Tfights]	}
		c(mean(error_cat_temp,na.rm=TRUE),sd(error_cat_temp,na.rm=TRUE))
	}
	error_cat_now = unname(error_cat_now)
	error_cat_mean = error_cat_now[1,]
	error_cat_sd = error_cat_now[2,]
	error_ind_now<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='cbind') %do% {
		error_ind_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xperc,xwind,xcorr),p)
		c1 = v[1]
		w = v[2]
		c2 = v[3]
		error_ind[[n,c1,w,1,1,c2]][[k]][,Tfights]	}
		c(mean(error_ind_temp,na.rm=TRUE),sd(error_ind_temp,na.rm=TRUE))
	}
	error_ind_now = unname(error_ind_now)
	error_ind_mean = error_ind_now[1,]
	error_ind_sd = error_ind_now[2,]	
	error = data.frame(categ = error_cat_mean, indiv=error_ind_mean, percwind=rep(perc_vals,times=xwind*xcorr), window = as.factor(rep(rep(wind_vals,each=xperc),times=xcorr)),  sigcorr = rep((0:(xcorr-1))*xwind,each=xperc*xcorr))
	corrandtype = c(error$sigcorr/2,error$sigcorr/2+2)
	corrandtype[corrandtype==0] = paste('Categ, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==1] = paste('Categ, ','Corr = ',corr_vals[2],sep='')
	corrandtype[corrandtype==2] = paste('Indiv, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==3] = paste('Indiv, ','Corr = ',corr_vals[2],sep='')
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),percwind=rep(percwind,2),corrandtype = as.factor(corrandtype),window = rep(window,2)))
	
	plots[[2]] = ggplot(stacked, aes(x=percwind, y =error, colour = corrandtype, linetype = window)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		scale_color_manual(values=divpal[c(1,2,5,4)])+	
		labs(linetype="Window", colour="") + 
		xlab("Perception window")+ylab("Error") +
		ggtitle(paste( "Group size = ", N_vals[n],sep=""))
		
multiplot(plotlist=plots,cols=2)

##---- learning time as a function of group size and perc vals
plots=list()
	#fix perception window and vary the other parameters 
	c1 = 2
	time_cat_med<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='c') %do% {
		time_cat_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xN,xwind,xcorr),p)
		n = v[1]
		w = v[2]
		c2 = v[3]
		time_cat[[n,c1,w,1,1,c2]][[k]] }
		median(time_cat_temp)
	} 	
	time_ind_med<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='c') %do% {
		time_ind_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xN,xwind,xcorr),p)
		n = v[1]
		w = v[2]
		c2 = v[3]
		time_ind[[n,c1,w,1,1,c2]][[k]] }
		median(time_ind_temp)
	} 	
	
	time = data.frame(categ = log(time_cat_med), indiv=log(time_ind_med), groupsize=rep(N_vals,times=xwind*xcorr), window = as.factor(rep(rep(wind_vals,each=xN),times=xcorr)),  sigcorr = rep((0:(xcorr-1))*xwind,each=xN*xcorr))
	corrandtype = c(error$sigcorr/2,error$sigcorr/2+2)
	corrandtype[corrandtype==0] = paste('Categ, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==1] = paste('Categ, ','Corr = ',corr_vals[2],sep='')
	corrandtype[corrandtype==2] = paste('Indiv, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==3] = paste('Indiv, ','Corr = ',corr_vals[2],sep='')
	stacked = with(time,data.frame(time=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),groupsize=rep(groupsize,2),corrandtype = as.factor(corrandtype),window = rep(window,2)))
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =time, colour = corrandtype, linetype = window)) + 
		geom_line() + geom_point() +
		# scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(1,2,5,4)])+
		labs(linetype="Window", colour="") + 
		xlab("Group size")+ylab("Log(time)") +
		ggtitle(paste( 'Perception window = ',perc_vals[c1],sep=""))
		
		#fix group size and vary the other parameters
		n = 2
	time_cat_med<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='c') %do% {
		time_cat_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xperc,xwind,xcorr),p)
		c1 = v[1]
		w = v[2]
		c2 = v[3]
		time_cat[[n,c1,w,1,1,c2]][[k]] }
		median(time_cat_temp)
	} 	
	time_ind_med<- foreach(p=1:prod(c(xN,xwind,xcorr)),.combine='c') %do% {
		time_ind_temp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(c(xperc,xwind,xcorr),p)
		c1 = v[1]
		w = v[2]
		c2 = v[3]
		time_ind[[n,c1,w,1,1,c2]][[k]] }
		median(time_ind_temp)
	} 	
	
	time = data.frame(categ = log(time_cat_med), indiv=log(time_ind_med), percwind=rep(perc_vals,times=xwind*xcorr), window = as.factor(rep(rep(wind_vals,each=xperc),times=xcorr)),  sigcorr = rep((0:(xcorr-1))*xwind,each=xperc*xcorr))
	corrandtype = c(error$sigcorr/2,error$sigcorr/2+2)
	corrandtype[corrandtype==0] = paste('Categ, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==1] = paste('Categ, ','Corr = ',corr_vals[2],sep='')
	corrandtype[corrandtype==2] = paste('Indiv, ','Corr = ',corr_vals[1],sep='')
	corrandtype[corrandtype==3] = paste('Indiv, ','Corr = ',corr_vals[2],sep='')
	stacked = with(time,data.frame(time=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),percwind=rep(percwind,2),corrandtype = as.factor(corrandtype),window = rep(window,2)))
	
	plots[[2]] = ggplot(stacked, aes(x=percwind, y =time, colour = corrandtype, linetype = window)) + 
		geom_line() + geom_point() +
		# scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(1,2,5,4)])+	
		labs(linetype="Window", colour="") + 
		xlab("Perception window")+ylab("Log(time)") +
		ggtitle(paste( "Group size = ", N_vals[n],sep=""))

pdf(file="/Users/eleanorbrush/Desktop/Fig1.pdf",width=6.83,height=3)		
multiplot(plotlist=plots,cols=2)
dev.off()

## ---- plot means of error for all individuals for each sim
layout(matrix(1:18,nrow=3,byrow=FALSE))

for(i in 1:18){
	
	ind = ind2sub(c(xN,xperc,xwind),i)
	n = ind[1]
	c1 = ind[2]
	w = ind[3]
	c2 = 2
		
	plot(1:Tfights_min,matrix(0,Tfights_min),type='n',ylim=c(0,1),xlab='Fights',ylab='Error',main=paste( "N", N_vals[n],',#',perc_vals[c1],',wind',wind_vals[w],sep=""))
	
	for(sim in 1:sim_runs){
		error_cat_now = error_cat[[n,c1,w,1,1,c2]][[sim]][,1:Tfights_min]
		error_ind_now = error_ind[[n,c1,w,1,1,c2]][[sim]][,1:Tfights_min]
		lines(colMeans(error_cat_now,na.rm=TRUE),col=mypal[1])
		lines(colMeans(error_ind_now,na.rm=TRUE),col=mypal[2])
	}
}

## ---- mean and sd of error for all individuals for all sims

plots=list()
toplot = lapply(1:9,ind2sub,v=c(xN,xperc))
w = 1
c2 = 1
for(i in 1:length(toplot)){
	toplot[[i]] = c(toplot[[i]],w,1,1,c2)
}

for(j in 1:length(toplot)){
	sub = toplot[[j]]
	n = sub[1]
	c1 = sub[2]
	w = sub[3]
	c2 = sub[6]
		
	error_cat_now<- foreach(k=1:sim_runs,.combine='rbind') %do% {
		error_cat[[n,c1,w,1,1,c2]][[k]][,1:Tfights_min]	
	}
	error_ind_now<- foreach(k=1:sim_runs,.combine='rbind') %do% {
		error_ind[[n,c1,w,1,1,c2]][[k]][,1:Tfights_min]
	}
	error_cat_mean = colMeans(error_cat_now,na.rm=TRUE)
	error_cat_sd = colSds(error_cat_now,na.rm=TRUE)
	error_ind_mean = colMeans(error_ind_now,na.rm=TRUE)
	error_ind_sd = colSds(error_ind_now,na.rm=TRUE)
	error = data.frame(categ = error_cat_mean, indiv=error_ind_mean, fights=1:Tfights_min)
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=Tfights_min)),fights=rep(fights,2)))
	plots[[j]]=
		ggplot(stacked, aes(fights,error,colour=categ))+
		scale_y_continuous(limits=c(0,1))+
		theme_bw()+
		geom_line()+
		scale_color_manual(values=c(mypal[1:2]))+
		geom_ribbon(aes(ymin=error-c(error_cat_sd,error_ind_sd),ymax=error+c(error_cat_sd,error_ind_sd),fill=categ,colour=NA),alpha=0.3)+
		scale_fill_manual(values=mypal[1:2])+
		xlab("Fights")+ylab("Error")+
		# ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',wind',wind_vals[w],sep=""))+	theme(legend.position="none")
		ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',wind',wind_vals[w],',corr',corr_vals[c2],sep=""))+	theme(legend.position="none")
}
multiplot(plotlist=plots,cols=3)

## ---- mean and sd of error for all individuals for all sims : effect of category number

plots=list()
toplot = lapply(1:9,ind2sub,v=c(xperc,1))
n = 1
w = 1
c2 = 1
for(i in 1:length(toplot)){
	toplot[[i]] = c(n,toplot[[i]][1],w,1,1,c2)
}

error_cat_vec = array(0,c(length(toplot),2))
error_ind_vec = array(0,c(length(toplot),2))

for(j in 1:length(toplot)){
	sub = toplot[[j]]
	c1 = sub[2]
		
	error_cat_now<- foreach(k=1:sim_runs,.combine='rbind') %do% {
		error_cat[[n,c1,w,1,1,c2]][[k]][,1:Tfights_min]	
	}
	error_ind_now<- foreach(k=1:sim_runs,.combine='rbind') %do% {
		error_ind[[n,c1,w,1,1,c2]][[k]][,1:Tfights_min]
	}
	error_cat_mean = colMeans(error_cat_now,na.rm=TRUE)
	error_cat_sd = colSds(error_cat_now,na.rm=TRUE)
	error_cat_vec[j,]= c(error_cat_mean[Tfights_min],error_cat_sd[Tfights_min])
	error_ind_mean = colMeans(error_ind_now,na.rm=TRUE)
	error_ind_sd = colSds(error_ind_now,na.rm=TRUE)
	error_ind_vec[j,] = c(error_ind_mean[Tfights_min],error_cat_sd[Tfights_min])
	error = data.frame(categ = error_cat_mean, indiv=error_ind_mean, fights=1:Tfights_min)
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=Tfights_min)),fights=rep(fights,2)))
	plots[[j]]=
		ggplot(stacked, aes(fights,error,colour=categ))+
		scale_y_continuous(limits=c(0,1))+
		theme_bw()+
		geom_line()+
		scale_color_manual(values=c(mypal[1:2]))+
		geom_ribbon(aes(ymin=error-c(error_cat_sd,error_ind_sd),ymax=error+c(error_cat_sd,error_ind_sd),fill=categ,colour=NA),alpha=0.3)+
		scale_fill_manual(values=mypal[1:2])+
		xlab("Fights")+ylab("Error")+
		# ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',wind',wind_vals[w],sep=""))+	theme(legend.position="none")
		ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',wind',wind_vals[w],',corr',corr_vals[c2],sep=""))+	theme(legend.position="none")
}
multiplot(plotlist=plots,cols=3)

plot(perc_vals,error_cat_vec[,1],type='o',ylim=range(c(error_cat_vec[,1],error_cat_vec[,1]+error_cat_vec[,2],error_cat_vec[,1]-error_cat_vec[,2])),xlab='# of categories',ylab='Error',main=paste( "N", N_vals[n],',wind',wind_vals[w],',corr',corr_vals[c2],',T',Tfights_min,sep=""))
points(perc_vals,error_cat_vec[,1]-error_cat_vec[,2],type='o',col='red')
points(perc_vals,error_cat_vec[,1]+error_cat_vec[,2],type='o',col='red')

## --- check that individual learning doesn't depend on number of categories

plots=list()
toplot=lapply(list(c(1,1,1),c(2,1,1),c(3,1,1),c(1,1,2),c(2,1,2),c(3,1,2)),sub2ind,v=c(xN,xwind,xcorr))
toplot=lapply(list(c(3,1,2)),sub2ind,v=c(xN,xwind,xcorr))

for(j in 1:length(toplot)){
	i = toplot[[j]]
	ind = ind2sub(c(xN,xwind,xcorr),i)
	n = ind[1]
	# c1 = ind[2]
	w = ind[2]
	c2 = ind[3]

	error_ind_now1<- foreach(k=1:sim_runs,.combine='rbind') %do% {
		error_ind[[n,1,w,1,1,c2]][[k]][,1:Tfights_min]
	}
	error_ind_now2<- foreach(k=1:sim_runs,.combine='rbind') %do% {
		error_ind[[n,2,w,1,1,c2]][[k]][,1:Tfights_min]
	}
	error_ind_now3<- foreach(k=1:sim_runs,.combine='rbind') %do% {
		error_ind[[n,3,w,1,1,c2]][[k]][,1:Tfights_min]
	}
	error_ind_mean1 = colMeans(error_ind_now1,na.rm=TRUE)
	error_ind_sd1 = colSds(error_ind_now1,na.rm=TRUE)
	error_ind_mean2 = colMeans(error_ind_now2,na.rm=TRUE)
	error_ind_sd2 = colSds(error_ind_now2,na.rm=TRUE)
	error_ind_mean3 = colMeans(error_ind_now3,na.rm=TRUE)
	error_ind_sd3 = colSds(error_ind_now3,na.rm=TRUE)
	error = data.frame(first=error_ind_mean1,second=error_ind_mean2,third=error_ind_mean3, fights=1:Tfights_min)
	stacked = with(error,data.frame(error=c(first,second,third),categ=factor(rep(c('first','second','third'),each=Tfights_min)),fights=rep(fights,3)))
	plots[[j]]=
		ggplot(stacked, aes(fights,error,colour=categ))+
		scale_y_continuous(limits=c(0,1))+
		theme_bw()+
		geom_line()+
		scale_color_manual(values=c(mypal[1:3]))+
		geom_ribbon(aes(ymin=error-c(error_cat_sd,error_ind_sd),ymax=error+c(error_cat_sd,error_ind_sd),fill=categ,colour=NA),alpha=0.3)+
		scale_fill_manual(values=mypal[1:3])+
		xlab("Fights")+ylab("Error")+
		# ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',wind',wind_vals[w],sep=""))+	theme(legend.position="none")
		ggtitle(paste( "N", N_vals[n],',wind',wind_vals[w],',corr',corr_vals[c2],sep=""))+	theme(legend.position="none")
}
multiplot(plotlist=plots,cols=2)

## ---- mean and sd of error for a single sim
Tfights_min = 800
plots=list()

for(i in 1:9){
	sub = ind2sub(c(xN,xperc),i)
	n = sub[1]
	c1 = sub[2]
	w = 1
	c2 = 2
		
	error_cat_now = error_cat[[n,c1,w,1,1,c2]][[1]][,1:Tfights_min]
	error_ind_now = error_ind[[n,c1,w,1,1,c2]][[1]][,1:Tfights_min]
	error_cat_mean = colMeans(error_cat_now,na.rm=TRUE)
	error_cat_sd = colSds(error_cat_now,na.rm=TRUE)
	error_ind_mean = colMeans(error_ind_now,na.rm=TRUE)
	error_ind_sd = colSds(error_ind_now,na.rm=TRUE)
	error = data.frame(categ = error_cat_mean, indiv=error_ind_mean, fights=1:Tfights_min)
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=Tfights_min)),fights=rep(fights,2)))
	plots[[i]]=
		ggplot(stacked, aes(fights,error,colour=categ))+
		scale_y_continuous(limits=c(0,1))+
		theme_bw()+
		geom_line()+
		scale_color_manual(values=c(mypal[1:2]))+
		geom_ribbon(aes(ymin=error-c(error_cat_sd,error_ind_sd),ymax=error+c(error_cat_sd,error_ind_sd),fill=categ,colour=NA),alpha=0.3)+
		scale_fill_manual(values=mypal[1:2])+
		xlab("Fights")+ylab("Error")+
		ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',win',wind_vals[w],sep=""))+	theme(legend.position="none")
}
multiplot(plotlist=plots,cols=3)

## ---- histogram of errors at a point in time
Tfights_min = 800
plots=list()
myticks = seq(0,1,by=0.2)

for(i in 1:9){
	
	ind = ind2sub(c(xN,xperc),i)
	n = ind[1]
	c1 = ind[2]
	w = 1
	c2 = 2
	
	error_cat_now<- foreach(k=1:sim_runs,.combine='c') %do% {
		error_cat[[n,c1,w,1,1,c2]][[k]][,Tfights_min]	
	}
	error_cat_now = data.frame(error=error_cat_now,recog=rep('categ',length(error_cat_now)))
	error_ind_now<- foreach(k=1:sim_runs,.combine='c') %do% {
		error_ind[[n,c1,w,1,1,c2]][[k]][,Tfights_min]
	}
	error_ind_now = data.frame(error=error_ind_now,recog=rep('indiv',length(error_cat_now)))
		
	together = rbind(error_cat_now,error_ind_now)

	plots[[i]]=ggplot(together, aes(error, fill = recog)) + 
		geom_histogram(alpha = .7, col='black', aes(y = ..density..), position = 'identity',binwidth=0.05)+
		theme_bw()+
		scale_fill_manual(values=mypal[1:2])+
		xlab(paste("Error at ",Tfights_min,' fights',sep=""))+ylab("Density")+
		ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',win',wind_vals[w],sep=""))+
		# ggtitle(paste(N_vals[n],',',perc_vals[c1],',',wind_vals[w],sep=""))+
		scale_x_continuous(breaks=(myticks),limits=(range(myticks)),labels=myticks)+
		theme(legend.position="none")
}
multiplot(plotlist=plots,cols=3)

## ---- histogram of errors at a point in time : effect of categor num
Tfights_min = 1000
plots=list()
myticks = seq(0,1,by=0.2)

toplot = lapply(1:9,ind2sub,v=c(xperc,1))
n = 1
w = 2
c2 = 1
for(i in 1:length(toplot)){
	toplot[[i]] = c(n,toplot[[i]][1],w,1,1,c2)
}

for(j in 1:9){
	sub = toplot[[j]]
	c1 = sub[2]
	
	error_cat_now<- foreach(k=1:sim_runs,.combine='c') %do% {
		error_cat[[n,c1,w,1,1,c2]][[k]][,Tfights_min]	
	}
	error_cat_now = data.frame(error=error_cat_now,recog=rep('categ',length(error_cat_now)))
	error_ind_now<- foreach(k=1:sim_runs,.combine='c') %do% {
		error_ind[[n,c1,w,1,1,c2]][[k]][,Tfights_min]
	}
	error_ind_now = data.frame(error=error_ind_now,recog=rep('indiv',length(error_cat_now)))
		
	together = rbind(error_cat_now,error_ind_now)

	plots[[j]]=ggplot(together, aes(error, fill = recog)) + 
		geom_histogram(alpha = .7, col='black', aes(y = ..density..), position = 'identity',binwidth=0.05)+
		theme_bw()+
		scale_fill_manual(values=mypal[1:2])+
		xlab(paste("Error at ",Tfights_min,' fights',sep=""))+ylab("Density")+
		ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',win',wind_vals[w],sep=""))+
		# ggtitle(paste(N_vals[n],',',perc_vals[c1],',',wind_vals[w],sep=""))+
		scale_x_continuous(breaks=(myticks),limits=(range(myticks)),labels=myticks)+
		theme(legend.position="none")
}
multiplot(plotlist=plots,cols=3)

## ---- learning times ------------

myticks = c(1,10,100,1000)

len = length(as.vector(unlist(time_cat[[n,c1,w,1,1,c2]])))

quartz(width=12,height=7)
plots = list()
toplot = lapply(1:9,ind2sub,v=c(xN,xperc))
w = 2
c2 = 2
for(i in 1:length(hold)){
	toplot[[i]] = c(toplot[[i]],w,1,1,c2)
}

for(j in 1:length(toplot)){
	sub = toplot[[j]]
	n = sub[1]
	c1 = sub[2]
	w = sub[3]
	c2 = sub[6]
	
	time_cat_now = data.frame(logtimes=log(as.vector(unlist(time_cat[[n,c1,w,1,1,c2]]))))
	time_cat_now$recog = 'categ'
	time_ind_now = data.frame(logtimes=log(as.vector(unlist(time_ind[[n,c1,w,1,1,c2]]))))
	time_ind_now$recog = 'indiv'
	
	together = rbind(time_cat_now,time_ind_now)

	plots[[j]]=ggplot(together, aes(logtimes, fill = recog)) + 
		geom_histogram(alpha = .7, col='black', aes(y = ..density..), position = 'identity',binwidth=0.5)+
		theme_bw()+
		scale_fill_manual(values=mypal[1:2])+
		xlab("Learning time")+ylab("Density")+
		ggtitle(paste( "N", N_vals[n],',#',perc_vals[c1],',wind',wind_vals[w],',corr',corr_vals[c2],sep=""))+	
		# ggtitle(paste(N_vals[n],',',perc_vals[c1],',',wind_vals[w],sep=""))+
		scale_x_continuous(breaks=log(myticks),limits=log(range(myticks))+c(0,3),labels=myticks)+
		theme(legend.position="none")
}

multiplot(plotlist=plots,cols=3)