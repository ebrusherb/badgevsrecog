library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)
source('multiplot.R')
source('ind2sub.R')
source('sub2ind.R')
source('get_legend.R')


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

c2vals = c(1,2)
xcorr2 = length(c2vals)

wvals = c(1,2)
xw = length(wvals)


## --- error as a function of  group size and perc vals
plots = list()	
	#fix perception window and vary the other parameters 
	c1 = 5
	pcat = 1
	pind = 1
	error_cat_mean_tmp = array(NA,c(prod(xN,xw,xcorr2),1))
	error_ind_mean_tmp = array(NA,c(prod(xN,xw,xcorr2),1))
	for( p in 1:prod(xN,xw,xcorr2)){
		v = ind2sub(c(xN,xw,xcorr2),p)
		i = sub2ind(d,c(v[1],c1,wvals[v[2]],pcat,pind,c2vals[v[3]]))
		error_cat_mean_tmp[p] = error_cat_mean[i]
		error_ind_mean_tmp[p] = error_ind_mean[i]
	}
	
	error = data.frame(categ = error_cat_mean_tmp, indiv=error_ind_mean_tmp, groupsize=rep(N_vals,times=xw*xcorr2), window =(rep(rep(0:(xw-1),each=xN),times=xcorr2)) ,  sigcorr = as.factor(rep(corr_vals[c2vals],each=xN*xw)))
	windandtype = rep(error$window,times=2)+rep(c(0,2),each=xN*xw*xcorr2)
	windandtype[windandtype==0] = paste('Categ, ','Wind = ',wind_vals[wvals[1]],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Wind = ',wind_vals[wvals[2]],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Wind = ',wind_vals[wvals[1]],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Wind = ',wind_vals[wvals[2]],sep='')
	stacked = with(error,data.frame(error=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xw,xcorr2)))),groupsize=rep(groupsize,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	stacked=stacked[-intersect(which(stacked$categ=='indiv'),which(stacked$sigcorr==corr_vals[c2vals[1]])),]
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =error, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) + scale_x_continuous(breaks=N_vals) + 
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab("Group Size, N")+ylab("Error") +
		ggtitle(substitute(paste(delta," = ",perc,sep=""),list(perc=perc_vals[c1])))
		
		 legend <- get_legend(plots[[1]])
	plots[[1]] = plots[[1]] + theme(legend.position='none')

	
# pdf(file="/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/Copenhagen symposium/summary_error_groupsize.pdf",width=5,height=3)		
# multiplot(plotlist=plots,cols=3)
grid.arrange(plots[[1]],legend,ncol=2,widths=c(1,0.5))
# dev.off()
		
		#fix group size and vary the other parameters
		n = 3
		pcat = 1
		pind = 1
		error_cat_mean_tmp = array(NA,c(prod(xperc,xw,xcorr2),1))
		error_ind_mean_tmp = array(NA,c(prod(xperc,xw,xcorr2),1))
		for( p in 1:prod(xperc,xw,xcorr2)){
			v = ind2sub(c(xperc,xw,xcorr2),p)
			i = sub2ind(d,c(n,v[1],wvals[v[2]],pcat,pind,c2vals[v[3]]))
			error_cat_mean_tmp[p] = error_cat_mean[i]
			error_ind_mean_tmp[p] = error_ind_mean[i]
		}
		
	error = data.frame(categ = error_cat_mean_tmp, indiv=error_ind_mean_tmp, percwind=rep(perc_vals,times=xw*xcorr2), window =(rep(wind_vals[wvals],each=xperc,times=xcorr2)) ,  sigcorr = as.factor(rep(corr_vals[c2vals],each=xperc*xw)))
	indmean = c(mean(error_ind_mean_tmp[error$window==wind_vals[wvals[1]]]),mean(error_ind_mean_tmp[error$window==wind_vals[wvals[2]]]))
	error$window[error$window==wind_vals[wvals[1]]] = paste('Categ, ','Wind = ',wind_vals[wvals[1]],sep='')
	error$window[error$window==wind_vals[wvals[2]]] = paste('Categ, ','Wind = ',wind_vals[wvals[2]],sep='')
	error$window=as.factor(error$window)
	indivwindow=as.factor(c(paste('Indiv, ','Wind = ',wind_vals[wvals[1]],sep=''),paste('Indiv, ','Wind = ',wind_vals[wvals[2]],sep='')))
		
	plots[[2]] = ggplot(error, aes(x=percwind, y =categ, colour = window, linetype = sigcorr)) + 
		geom_segment(aes(x=perc_vals[1],xend=perc_vals[xperc],y=indmean[1],yend=indmean[1],color=indivwindow[1])) + 
		geom_segment(aes(x=perc_vals[1],xend=perc_vals[xperc],y=indmean[2],yend=indmean[2],color=indivwindow[2])) +
		geom_line() + geom_point() +
		scale_y_continuous(limits=c(0,0.5)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.title=element_text(size=10),plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)]) +	 scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab(expression(paste("Perception window, ",delta,sep='')))+ylab("Error") +
		ggtitle(paste( "N = ", N_vals[n],sep="")) 
		
		legend <- get_legend(plots[[2]])
	plots[[2]] = plots[[2]] + theme(legend.position='none')

		
# pdf(file="/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/Copenhagen symposium/summary_error_percwindow.pdf",width=5,height=3)		
# multiplot(plotlist=plots,cols=3)
grid.arrange(plots[[2]],legend,ncol=2,widths=c(1,0.5))
# dev.off()

##---- learning time as a function of group size and perc vals
plots=list()
	#fix perception window and vary the other parameters 
	c1 = 5
	pcat = 1
	pind = 1
	time_cat_mean_tmp = array(NA,c(prod(xN,xw,xcorr2),1))
	time_ind_mean_tmp = array(NA,c(prod(xN,xw,xcorr2),1))
	for( p in 1:prod(xN,xw,xcorr2)){
		v = ind2sub(c(xN,xw,xcorr2),p)
		i = sub2ind(d,c(v[1],c1,wvals[v[2]],pcat,pind,c2vals[v[3]]))
		time_cat_mean_tmp[p] = time_cat_mean[i]
		time_ind_mean_tmp[p] = time_ind_mean[i]
	}	
	
	time = data.frame(categ = log(time_cat_mean_tmp), indiv=log(time_ind_mean_tmp), groupsize=rep(N_vals,times=xw*xcorr2), window =(rep(rep(0:(xw-1),each=xN),times=xcorr2)) ,  sigcorr = as.factor(rep(corr_vals[c2vals],each=xN*xw)))
	windandtype = rep(time$window,times=2)+rep(c(0,2),each=xN*xw*xcorr2)
	windandtype[windandtype==0] = paste('Categ, ','Wind = ',wind_vals[wvals[1]],sep='')
	windandtype[windandtype==1] = paste('Categ, ','Wind = ',wind_vals[wvals[2]],sep='')
	windandtype[windandtype==2] = paste('Indiv, ','Wind = ',wind_vals[wvals[1]],sep='')
	windandtype[windandtype==3] = paste('Indiv, ','Wind = ',wind_vals[wvals[2]],sep='')
	stacked = with(time,data.frame(time=c(categ,indiv),categ=factor(rep(c('categ','indiv'),each=prod(c(xN,xw,xcorr2)))),groupsize=rep(groupsize,2),windandtype = as.factor(windandtype),sigcorr = rep(sigcorr,2)))
	stacked=stacked[-intersect(which(stacked$categ=='indiv'),which(stacked$sigcorr==corr_vals[c2vals[1]])),]
	
	plots[[1]] = ggplot(stacked, aes(x=groupsize, y =time, colour = windandtype, linetype = sigcorr)) + 
		geom_line() + geom_point() +
		scale_y_continuous(limits=log(c(700,10000)),breaks=log(c(1000,2000,4000,8000)),labels=c(1000,2000,4000,8000)) + scale_x_continuous(breaks=N_vals) + 
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab("Group size, N")+ylab("Time") +
		ggtitle(substitute(paste(delta," = ",perc,sep=""),list(perc=perc_vals[c1])))
		
		legend <- get_legend(plots[[1]])
	plots[[1]] = plots[[1]] + theme(legend.position='none')

	
# pdf(file="/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/Copenhagen symposium/summary_time_groupsize.pdf",width=5,height=3)		
# multiplot(plotlist=plots,cols=3)
grid.arrange(plots[[1]],legend,ncol=2,widths=c(1,0.5))
# dev.off()
		
		#fix group size and vary the other parameters
		n = 3
		pcat = 1
		pind = 1
		time_cat_mean_tmp = array(NA,c(prod(xperc,xw,xcorr2),1))
		time_ind_mean_tmp = array(NA,c(prod(xperc,xw,xcorr2),1))
		for( p in 1:prod(xperc,xw,xcorr2)){
			v = ind2sub(c(xperc,xw,xcorr2),p)
			i = sub2ind(d,c(n,v[1],wvals[v[2]],pcat,pind,c2vals[v[3]]))
			time_cat_mean_tmp[p] = time_cat_mean[i]
			time_ind_mean_tmp[p] = time_ind_mean[i]
		}	
	
	time = data.frame(categ = log(time_cat_mean_tmp), indiv=log(time_ind_mean_tmp), percwind=rep(perc_vals,times=xw*xcorr2),window =(rep(wind_vals[wvals],each=xperc,times=xcorr2)),  sigcorr = as.factor(rep(corr_vals[c2vals],each=xperc*xw)))
	
	indmean = log(c(mean(time_ind_mean_tmp[time$window==wind_vals[wvals[1]]]),mean(time_ind_mean_tmp[time$window==wind_vals[wvals[2]]])))
	time$window[error$window==wind_vals[wvals[1]]] = paste('Categ, ','Wind = ',wind_vals[wvals[1]],sep='')
	time$window[error$window==wind_vals[wvals[2]]] = paste('Categ, ','Wind = ',wind_vals[wvals[2]],sep='')
	time$window=as.factor(error$window)
	indivwindow=as.factor(c(paste('Indiv, ','Wind = ',wind_vals[wvals[1]],sep=''),paste('Indiv, ','Wind = ',wind_vals[wvals[2]],sep='')))
	
	plots[[2]] = ggplot(time, aes(x=percwind, y =categ, colour = window, linetype = sigcorr)) + 
		geom_segment(aes(x=perc_vals[1],xend=perc_vals[xperc],y=indmean[1],yend=indmean[1],color=indivwindow[1])) + 
		geom_segment(aes(x=perc_vals[1],xend=perc_vals[xperc],y=indmean[2],yend=indmean[2],color=indivwindow[2])) +
		geom_line() + geom_point() +
		scale_y_continuous(limits=log(c(700,10000)),breaks=log(c(1000,2000,4000,8000)),labels=c(1000,2000,4000,8000)) +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10),plot.margin=unit(c(0,0,0,0),"cm")) + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+	scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab(expression(paste("Perception window, ",delta,sep="")))+ylab("Time") +
		ggtitle(paste( "N = ", N_vals[n],sep=""))
		
		legend <- get_legend(plots[[2]])
	plots[[2]] = plots[[2]] + theme(legend.position='none')
	

# pdf(file="/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/Copenhagen symposium/summary_time_percwindow.pdf",width=5,height=3)		
# multiplot(plotlist=plots,cols=2)
grid.arrange(plots[[2]],legend,ncol=2,widths=c(1,0.5))
# dev.off()

