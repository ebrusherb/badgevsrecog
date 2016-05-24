

# ##---- error as a function of time
plots=list()

q = 1
n = toplot$n[q]
	c1 = toplot$c1[q]
	w = toplot$w[q]
	pcat = toplot$pcat[q]
	pind = toplot$pind[q]
	
	error=error_time[[q]] 
	
	plots[[q]] = ggplot(error, aes(x=fights, y =error, colour = categ, linetype = sigcorr)) + 
		geom_line() + 
		scale_y_continuous(limits=c(0,0.5)) + scale_x_continuous(breaks=seq(0,Tfights_min,by=2500)) + 
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10),plot.title=element_text(size=10) , plot.margin=unit(c(0,0.25,0,0),"cm")) + 
		scale_linetype_manual(values=c(3,1)) +
		scale_color_manual(values=divpal[c(1,5)])+
		labs(linetype="Corr", colour="") + 
		xlab("Fights")+ylab("Error") +
		ggtitle(substitute(paste( delta,'=',perc,', N=',N,', w=',wind,sep=""),list(perc = perc_vals[c1], N = N_vals[n], wind = wind_vals[w]))) 
		
	legend <- get_legend(plots[[1]])
	plots[[1]] = plots[[1]] + theme(legend.position='none')+
		geom_ribbon(aes(ymin=error-sd,ymax=error+sd,fill=categ,colour=NA),alpha=0.3)+
		scale_fill_manual(values=divpal[c(1,5)])

for(q in 2:dim(toplot)[1]){

	n = toplot$n[q]
	c1 = toplot$c1[q]
	w = toplot$w[q]
	pcat = toplot$pcat[q]
	pind = toplot$pind[q]
	
	error=error_time[[q]] 
	
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
		scale_fill_manual(values=divpal[c(1,5)])
	
}

pdf(file="/homes/ebrush/priv/badgevsrecog/learning_curves.pdf",width=6.83,height=3)				
grid.arrange(plots[[1]],plots[[2]],plots[[3]],legend,plots[[4]],plots[[5]],plots[[6]],ncol=4,widths=c(1,1,1,.5))
dev.off()

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
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.margin=unit(c(0,0,0,0),"cm"),legend.position="none") + 
		scale_color_manual(values=divpal[c(2,1,4,5)])+scale_linetype_manual(values=c(3,1)) +
		labs(linetype="Corr", colour="") + 
		xlab("Group Size, N")+ylab("Error") +
		ggtitle(substitute(paste(delta," = ",perc,sep=""),list(perc=perc_vals[c1])))
		
		#fix group size and vary the other parameters
		n = 4
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

		
pdf(file="/homes/ebrush/priv/badgevsrecog/summary_error.pdf",width=6.83,height=3)		
# multiplot(plotlist=plots,cols=3)
grid.arrange(plots[[1]],plots[[2]],legend,ncol=3,widths=c(1,1,0.5))
dev.off()

##---- learning time as a function of group size and perc vals
low = floor(min(log(c(range(time_cat_mean),range(time_ind_mean))/500,base=2)))
high = ceiling(max(log(c(range(time_cat_mean),range(time_ind_mean))/500,base=2)))
ybreaks= 500*(2^(low:high))

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
		scale_y_continuous(limits=log(range(ybreaks)),breaks=log(ybreaks),labels=ybreaks) + scale_x_continuous(breaks=N_vals) + 
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
		scale_y_continuous(limits=log(range(ybreaks)),breaks=log(ybreaks),labels=ybreaks) +
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


