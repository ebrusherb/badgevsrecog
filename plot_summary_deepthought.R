

##---- error as a function of time
plots=list()
error_time = list()

toplot = data.frame(n = rep(c(1,4),each=3), c1 = rep(c(2,7,7),2), w = rep(c(1,1,2),2), pcat = rep(1,6), pind = rep(1,6)) 

q=1
#fix perception and window and group size and look at error over time
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
	error_time[[1]] = error
	
	plots[[1]] = ggplot(error, aes(x=fights, y =error, colour = categ, linetype = sigcorr)) + 
		geom_line() + 
		scale_y_continuous(limits=c(0,0.5)) + scale_x_continuous(breaks=seq(0,Tfights_min,by=2500)) + 
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10),plot.margin=unit(c(0,0.25,0,0),"cm")) +  
		scale_linetype_manual(values=c(3,1)) +
		scale_color_manual(values=divpal[c(1,5)])+
		labs(linetype="Corr", colour="") + 
		xlab("Fights")+ylab("Error") +
		ggtitle(substitute(paste( delta,'=',perc,', N=',N,', w=',wind,sep=""),list(perc = perc_vals[c1], N = N_vals[n], wind = wind_vals[w]))) 
				
	legend <- get_legend(plots[[1]])

	plots[[1]] = plots[[1]] + geom_ribbon(aes(ymin=error-sd,ymax=error+sd,fill=categ,colour=NA),alpha=0.3)+
		scale_fill_manual(values=divpal[c(1,5)])+theme(legend.position='none')

for(q in 2:dim(toplot)[1]){

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
	
	plots[[q]] = ggplot(error, aes(x=fights, y =error, colour = categ, linetype = sigcorr)) + 
		geom_line() + 
		scale_y_continuous(limits=c(0,0.5)) + scale_x_continuous(breaks=seq(0,Tfights_min,by=2500)) + 
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10),plot.title=element_text(size=10) , plot.margin=unit(c(0,0.25,0,0),"cm"),legend.position='none') + 
		scale_linetype_manual(values=c(3,1)) +
		scale_color_manual(values=divpal[c(1,5)])+
		labs(linetype="Corr", colour="") + 
		xlab("Fights")+ylab("Error") +
		ggtitle(substitute(paste( delta,'=',perc,', N=',N,', w=',wind,sep=""),list(perc = perc_vals[c1], N = N_vals[n], wind = wind_vals[w]))) +
		geom_ribbon(aes(ymin=error-sd,ymax=error+sd,fill=categ,colour=NA),alpha=0.3)+
		scale_fill_manual(values=divpal[c(1,5)])+theme(legend.position='none')
	
}

pdf(file="/homes/ebrush/priv/badgevsrecog/learning_curves.pdf",width=6.83,height=3)				
grid.arrange(plots[[1]],plots[[2]],plots[[3]],legend,plots[[4]],plots[[5]],plots[[6]],ncol=4,widths=c(1,1,1,.5))
dev.off()

## --- error as a function of  group size and perc vals
plots = list()	
	#fix perception window and vary the other parameters 
	c1 = 7
	pcat = 1
	pind = 1
	error_cat_mean_tmp = array(NA,c(prod(xN,xwind,xcorr2),1))
	# error_cat_sd_tmp = array(NA,c(prod(xN,xwind,xcorr2),1))
	error_ind_mean_tmp = array(NA,c(prod(xN,xwind,xcorr2),1))
	# error_ind_sd_tmp = array(NA,c(prod(xN,xwind,xcorr2),1))
	for( p in 1:prod(xN,xwind,xcorr2)){
		v = ind2sub(c(xN,xwind,xcorr2),p)
		i = sub2ind(d,c(v[1],c1,v[2],pcat,pind,c2vals[v[3]]))
		error_cat_mean_tmp[p] = error_cat_mean[i]
		# error_cat_sd_tmp[p] = error_cat_sd[i]
		error_ind_mean_tmp[p] = error_ind_mean[i]
		# error_cat_sd_tmp[p] = error_ind_sd[i]
	}
	
	error = data.frame(categ = error_cat_mean_tmp, indiv=error_ind_mean_tmp, groupsize=rep(N_vals,times=xwind*xcorr2), window =(rep(rep(0:(xwind-1),each=xN),times=xcorr2)) ,  sigcorr = as.factor(rep(corr_vals[c2vals],each=xN*xwind)))
	windandtype = rep(error$window,times=2)+rep(c(0,2),each=xN*xwind*xcorr2)
	windandtype[windandtype==0] = paste('Categ, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Window = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Window = ',wind_vals[2],sep='')
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr2)))),groupsize=rep(groupsize,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	stacked=stacked[-intersect(which(stacked$categ=='indiv'),which(stacked$sigcorr==corr_vals[c2vals[1]])),]
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =error, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) + scale_x_continuous(breaks=N_vals) + 
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab("Group Size, N")+ylab("Error") +
		ggtitle(substitute(paste(delta," = ",perc,sep=""),list(perc=perc_vals[c1])))
		
		#fix group size and vary the other parameters
		n = 4
		pcat = 1
		pind = 1
		error_cat_mean_tmp = array(NA,c(prod(xperc,xwind,xcorr2),1))
		# error_cat_sd_tmp = array(NA,c(prod(xperc,xwind,xcorr2),1))
		error_ind_mean_tmp = array(NA,c(prod(xperc,xwind,xcorr2),1))
		# error_ind_sd_tmp = array(NA,c(prod(xperc,xwind,xcorr2),1))
		for( p in 1:prod(xperc,xwind,xcorr2)){
			v = ind2sub(c(xperc,xwind,xcorr2),p)
			i = sub2ind(d,c(n,v[1],v[2],pcat,pind,c2vals[v[3]]))
			error_cat_mean_tmp[p] = error_cat_mean[i]
			# error_cat_sd_tmp[p] = error_cat_sd[i]
			error_ind_mean_tmp[p] = error_ind_mean[i]
			# error_cat_sd_tmp[p] = error_ind_sd[i]
		}
		
	error = data.frame(categ = error_cat_mean_tmp, indiv=error_ind_mean_tmp, percwind=rep(perc_vals,times=xwind*xcorr2), window =(rep(rep(0:(xwind-1),each=xperc),times=xcorr2)) ,  sigcorr = as.factor(rep(corr_vals[c2vals],each=xperc*xwind)))
	windandtype = rep(error$window,times=2)+rep(c(0,2),each=xperc*xwind*xcorr2)
	windandtype[windandtype==0] = paste('Categ, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Wind = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Wind = ',wind_vals[2],sep='')
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xperc,xwind,xcorr2)))),percwind=rep(percwind,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	stacked=stacked[-intersect(which(stacked$categ=='indiv'),which(stacked$sigcorr==corr_vals[c2vals[1]])),]
		
	plots[[2]] = ggplot(stacked, aes(x=percwind, y =error, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.title=element_text(size=10),plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+	 scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab(expression(paste("Perception window, ",delta,sep='')))+ylab("Error") +
		ggtitle(paste( "N = ", N_vals[n],sep=""))
		
	 legend <- get_legend(plots[[2]])
	plots[[2]] = plots[[2]] + theme(legend.position='none')

		
pdf(file="/homes/ebrush/priv/badgevsrecog/summary_error.pdf",width=6.83,height=3)		
# multiplot(plotlist=plots,cols=3)
grid.arrange(plots[[1]],plots[[2]],legend,ncol=3,widths=c(1,1,0.5))
dev.off()

##---- learning time as a function of group size and perc vals
plots=list()
	#fix perception window and vary the other parameters 
	c1 = 7
	pcat = 1
	pind = 1
	time_cat_mean_tmp = array(NA,c(prod(xN,xwind,xcorr2),1))
	time_ind_mean_tmp = array(NA,c(prod(xN,xwind,xcorr2),1))
	for( p in 1:prod(xN,xwind,xcorr2)){
		v = ind2sub(c(xN,xwind,xcorr2),p)
		i = sub2ind(d,c(v[1],c1,v[2],pcat,pind,c2vals[v[3]]))
		time_cat_mean_tmp[p] = time_cat_mean[i]
		time_ind_mean_tmp[p] = time_ind_mean[i]
	}	
	
	time = data.frame(categ = log(time_cat_mean_tmp), indiv=log(time_ind_mean_tmp), groupsize=rep(N_vals,times=xwind*xcorr2), window =(rep(rep(0:(xwind-1),each=xN),times=xcorr2)) ,  sigcorr = as.factor(rep(corr_vals[c2vals],each=xN*xwind)))
	windandtype = rep(time$window,times=2)+rep(c(0,2),each=xN*xwind*xcorr2)
	windandtype[windandtype==0] = paste('Categ, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Window = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Window = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Window = ',wind_vals[2],sep='')
	stacked = with(time,data.frame(time=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xwind,xcorr2)))),groupsize=rep(groupsize,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	stacked=stacked[-intersect(which(stacked$categ=='indiv'),which(stacked$sigcorr==corr_vals[c2vals[1]])),]
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =time, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=log(c(900,10000)),breaks=log(c(1000,2000,4000,8000)),labels=c(1000,2000,4000,8000)) + scale_x_continuous(breaks=N_vals) + 
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab("Group size, N")+ylab("Time") +
		ggtitle(substitute(paste(delta," = ",perc,sep=""),list(perc=perc_vals[c1])))
		
		#fix group size and vary the other parameters
		n = 4
		pcat = 1
		pind = 1
		time_cat_mean_tmp = array(NA,c(prod(xperc,xwind,xcorr2),1))
		time_ind_mean_tmp = array(NA,c(prod(xperc,xwind,xcorr2),1))
		for( p in 1:prod(xperc,xwind,xcorr2)){
			v = ind2sub(c(xperc,xwind,xcorr2),p)
			i = sub2ind(d,c(n,v[1],v[2],pcat,pind,c2vals[v[3]]))
			time_cat_mean_tmp[p] = time_cat_mean[i]
			time_ind_mean_tmp[p] = time_ind_mean[i]
		}	
	
	time = data.frame(categ = log(time_cat_mean_tmp), indiv=log(time_ind_mean_tmp), percwind=rep(perc_vals,times=xwind*xcorr2),window =(rep(rep(0:(xwind-1),each=xperc),times=xcorr2)) ,  sigcorr = as.factor(rep(corr_vals[c2vals],each=xperc*xwind)))
	windandtype = rep(time$window,times=2)+rep(c(0,2),each=xperc*xwind*xcorr2)
	windandtype[windandtype==0] = paste('Categ, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Wind = ',wind_vals[2],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Wind = ',wind_vals[1],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Wind = ',wind_vals[2],sep='')
	stacked = with(time,data.frame(time=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xperc,xwind,xcorr2)))),percwind=rep(percwind,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	stacked=stacked[-intersect(which(stacked$categ=='indiv'),which(stacked$sigcorr==corr_vals[c2vals[1]])),]
	
	plots[[2]] = ggplot(stacked, aes(x=percwind, y =time, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=log(c(900,10000)),breaks=log(c(1000,2000,4000,8000)),labels=c(1000,2000,4000,8000)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10),plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+	scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab(expression(paste("Perception window, ",delta,sep="")))+ylab("Time") +
		ggtitle(paste( "N = ", N_vals[n],sep=""))
		
		legend <- get_legend(plots[[2]])
	plots[[2]] = plots[[2]] + theme(legend.position='none')
	

pdf(file="/homes/ebrush/priv/badgevsrecog/summary_time.pdf",width=6.83,height=3)		
# multiplot(plotlist=plots,cols=2)
grid.arrange(plots[[1]],plots[[2]],legend,ncol=3,widths=c(1,1,0.5))
dev.off()

save(confus_cat_vals,confus_ind_vals,corr_vals,d,error_cat_mean,error_ind_mean,N_vals,perc_vals,time_cat_mean,time_ind_mean,wind_vals,error_time,file='/homes/ebrush/priv/badgevsrecog/summary_stats.Rdata')
