library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)
source('multiplot.R')
source('ind2sub.R')
source('get_legend.R')
source('sub2ind.R')

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

time_cat_med<- foreach(p=1:P,.combine='c') %do% {
		time_cat_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		time_cat[[n,c1,w,pcat,pind,c2]][[k]] }
		median(time_cat_tmp)
	} 	
	
time_ind_med<- foreach(p=1:P,.combine='c') %do% {
		time_ind_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		time_ind[[n,c1,w,pcat,pind,c2]][[k]] }
		median(time_ind_tmp)
	} 	

##---- error as a function of time
plots=list()
	#fix perception and window and group size and look at error over time
	n = 2
	c1 = 2
	w = 1
	pcat = 1
	pind = 1
	
	error_cat_stats_time<- foreach(c2=1:2,.combine='cbind') %do% {
		error_cat_stats_time_tmp <-foreach(k = 1:sim_runs,.combine='rbind') %do%{
			error_cat[[n,c1,w,pcat,pind,c2]][[k]][,1:Tfights_min]
		} 
		rbind(colMeans(error_cat_stats_time_tmp,na.rm=TRUE),colSds(error_cat_stats_time_tmp,na.rm=TRUE))
	}
	c2 = 1
	error_ind_stats_time_tmp <-foreach(k = 1:sim_runs,.combine='rbind') %do%{
			error_ind[[n,c1,w,pcat,pind,c2]][[k]][,1:Tfights_min]
		} 
	error_ind_stats_time<-rbind(colMeans(error_ind_stats_time_tmp,na.rm=TRUE),colSds(error_ind_stats_time_tmp,na.rm=TRUE))
	
	error = data.frame(error = c(error_cat_stats_time[1,],error_ind_stats_time[1,]),sd = c(error_cat_stats_time[2,],error_ind_stats_time[2,]),categ = as.factor(c(rep('categ',2*Tfights_min),rep('indiv',Tfights_min))),fights=rep(1:Tfights_min,times=3),sigcorr=as.factor(c(rep(corr_vals,each=Tfights_min),rep(corr_vals[c2],Tfights_min))))
	
	plots[[1]] = ggplot(error, aes(x=fights, y =error, colour = categ, linetype = sigcorr)) + 
		geom_line() + 
		# geom_ribbon(aes(ymin=error-c(error_cat_sd,error_ind_sd),ymax=error+c(error_cat_sd,error_ind_sd),fill=categ,colour=NA),alpha=0.3)+
		# scale_fill_manual(values=mypal[1:2])+
		scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + 
		scale_color_manual(values=divpal[c(1,5)])+
		labs(linetype="Corr", colour="") + 
		xlab("Fights")+ylab("Error") +
		ggtitle(paste( 'Perception window = ',perc_vals[c1],', Group size = ',N_vals[n],', Window = ',wind_vals[w],sep=""))

## --- error as a function of  group size and perc vals
plots = list()	
	#fix perception window and vary the other parameters 
	c1 = 2
	pcat = 1
	pind = 1
	error_cat_mean_tmp = array(NA,c(prod(xN,xwind,xcorr),1))
	error_cat_sd_tmp = array(NA,c(prod(xN,xwind,xcorr),1))
	error_ind_mean_tmp = array(NA,c(prod(xN,xwind,xcorr),1))
	error_ind_sd_tmp = array(NA,c(prod(xN,xwind,xcorr),1))
	for( p in 1:prod(xN,xwind,xcorr)){
		v = ind2sub(c(xN,xwind,xcorr),p)
		i = sub2ind(d,c(v[1],c1,v[2],pcat,pind,v[3]))
		error_cat_mean_tmp[p] = error_cat_mean[i]
		error_cat_sd_tmp[p] = error_cat_sd[i]
		error_ind_mean_tmp[p] = error_ind_mean[i]
		error_cat_sd_tmp[p] = error_ind_sd[i]
	}
	
	error = data.frame(categ = error_cat_mean_tmp, indiv=error_ind_mean_tmp, groupsize=rep(N_vals,times=xwind*xcorr), window =(rep(rep(0:(xwind-1),each=xN),times=xcorr)) ,  sigcorr = as.factor(rep(corr_vals,each=xN*xcorr)))
	windandtype = rep(error$window,times=2)+rep(c(0,2),each=xN*xwind*xcorr)
	windandtype[windandtype==0] = paste('Categ, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Window = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Window = ',wind_vals[2],sep='')
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),groupsize=rep(groupsize,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =error, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+
		labs(linetype="Corr", colour="") + 
		xlab("Group size")+ylab("Error") +
		ggtitle(paste( 'Perception window = ',perc_vals[c1],sep=""))
		
		#fix group size and vary the other parameters
		n = 2
		pcat = 1
		pind = 1
		error_cat_mean_tmp = array(NA,c(prod(xperc,xwind,xcorr),1))
		error_cat_sd_tmp = array(NA,c(prod(xperc,xwind,xcorr),1))
		error_ind_mean_tmp = array(NA,c(prod(xperc,xwind,xcorr),1))
		error_ind_sd_tmp = array(NA,c(prod(xperc,xwind,xcorr),1))
		for( p in 1:prod(xperc,xwind,xcorr)){
			v = ind2sub(c(xperc,xwind,xcorr),p)
			i = sub2ind(d,c(n,v[1],v[2],pcat,pind,v[3]))
			error_cat_mean_tmp[p] = error_cat_mean[i]
			error_cat_sd_tmp[p] = error_cat_sd[i]
			error_ind_mean_tmp[p] = error_ind_mean[i]
			error_cat_sd_tmp[p] = error_ind_sd[i]
		}
		
	error = data.frame(categ = error_cat_mean_tmp, indiv=error_ind_mean_tmp, percwind=rep(perc_vals,times=xwind*xcorr), window =(rep(rep(0:(xwind-1),each=xN),times=xcorr)) ,  sigcorr = as.factor(rep(corr_vals,each=xN*xcorr)))
	windandtype = rep(error$window,times=2)+rep(c(0,2),each=xN*xwind*xcorr)
	windandtype[windandtype==0] = paste('Categ, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Wind = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Wind = ',wind_vals[2],sep='')
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),percwind=rep(percwind,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
		
	plots[[2]] = ggplot(stacked, aes(x=percwind, y =error, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+	
		labs(linetype="Corr", colour="") + 
		xlab("Perception window")+ylab("Error") +
		ggtitle(paste( "Group size = ", N_vals[n],sep=""))
		
	 legend <- get_legend(plots[[2]])
	plots[[2]] = plots[[2]] + theme(legend.position='none')

		
pdf(file="/Users/eleanorbrush/Desktop/summary_error.pdf",width=6.83,height=3)		
# multiplot(plotlist=plots,cols=3)
grid.arrange(plots[[1]],plots[[2]],legend,ncol=3,widths=c(1,1,0.5))
dev.off()

##---- learning time as a function of group size and perc vals
plots=list()
	#fix perception window and vary the other parameters 
	c1 = 2
	pcat = 1
	pind = 1
	time_cat_med_tmp = array(NA,c(prod(xN,xwind,xcorr),1))
	time_ind_med_tmp = array(NA,c(prod(xN,xwind,xcorr),1))
	for( p in 1:prod(xN,xwind,xcorr)){
		v = ind2sub(c(xN,xwind,xcorr),p)
		i = sub2ind(d,c(v[1],c1,v[2],pcat,pind,v[3]))
		time_cat_med_tmp[p] = time_cat_med[i]
		time_ind_med_tmp[p] = time_ind_med[i]
	}	
	
	time = data.frame(categ = log(time_cat_med_tmp), indiv=log(time_ind_med_tmp), groupsize=rep(N_vals,times=xwind*xcorr), window =(rep(rep(0:(xwind-1),each=xN),times=xcorr)) ,  sigcorr = as.factor(rep(corr_vals,each=xN*xcorr)))
	windandtype = rep(time$window,times=2)+rep(c(0,2),each=xN*xwind*xcorr)
	windandtype[windandtype==0] = paste('Categ, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Window = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Window = ',wind_vals[2],sep='')
	stacked = with(time,data.frame(time=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),groupsize=rep(groupsize,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =time, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		# scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+
		labs(linetype="Corr", colour="") + 
		xlab("Group size")+ylab("Log(time)") +
		ggtitle(paste( 'Perception window = ',perc_vals[c1],sep=""))
		
		#fix group size and vary the other parameters
		n = 2
		pcat = 1
		pind = 1
		time_cat_med_tmp = array(NA,c(prod(xperc,xwind,xcorr),1))
		time_ind_med_tmp = array(NA,c(prod(xperc,xwind,xcorr),1))
		for( p in 1:prod(xperc,xwind,xcorr)){
			v = ind2sub(c(xperc,xwind,xcorr),p)
			i = sub2ind(d,c(n,v[1],v[2],pcat,pind,v[3]))
			time_cat_med_tmp[p] = time_cat_med[i]
			time_ind_med_tmp[p] = time_ind_med[i]
		}	
	
	time = data.frame(categ = log(time_cat_med_tmp), indiv=log(time_ind_med_tmp), percwind=rep(perc_vals,times=xwind*xcorr),window =(rep(rep(0:(xwind-1),each=xN),times=xcorr)) ,  sigcorr = as.factor(rep(corr_vals,each=xN*xcorr)))
	windandtype = rep(time$window,times=2)+rep(c(0,2),each=xN*xwind*xcorr)
	windandtype[windandtype==0] = paste('Categ, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Wind = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Wind = ',wind_vals[2],sep='')
	stacked = with(time,data.frame(time=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr)))),percwind=rep(percwind,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	
	plots[[2]] = ggplot(stacked, aes(x=percwind, y =time, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		# scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+	
		labs(linetype="Corr", colour="") + 
		xlab("Perception window")+ylab("Log(time)") +
		ggtitle(paste( "Group size = ", N_vals[n],sep=""))
		
		legend <- get_legend(plots[[2]])
	plots[[2]] = plots[[2]] + theme(legend.position='none')
	
	blankPlot <- ggplot()+geom_blank(aes(1,1))

pdf(file="/Users/eleanorbrush/Desktop/summary_time.pdf",width=6.83,height=3)		
# multiplot(plotlist=plots,cols=2)
grid.arrange(plots[[1]],plots[[2]],legend,ncol=3,widths=c(1,1,0.5))
dev.off()
