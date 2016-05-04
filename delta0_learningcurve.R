##---- error as a function of time
plots=list()
error_time = list()

toplot = data.frame(n = rep(c(1,4),each=3), c1 = rep(c(2,7,7),2), w = rep(c(1,1,2),2), pcat = rep(1,6), pind = rep(1,6)) 

q=1
#fix perception and window and group size and look at error over time
	n = 2
	c1 = xperc
	w = 1
	pcat = 1
	pind = 1
	
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
		scale_fill_manual(values=divpal[c(1,5)])
		
pdf(file="/homes/ebrush/priv/badgevsrecog/delta0_learning_curves.pdf",width=3.4,height=3)				
grid.arrange(plots[[1]],legend,ncol=2,widths=c(1,.5))
dev.off()