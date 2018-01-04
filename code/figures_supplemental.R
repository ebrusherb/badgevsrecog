
##heat maps of error and time --- 
plots.Nw=list()
plots.cw = list()
plots.pw = list()
plots.Nc = list()
c2 = 2
marg = c(0.3,0.2,0,0)

error_Nw = error[which(with(parameters,interaction(c1,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
if(is.element(Inf,wind_vals)){
# error_Nw = error_Nw[-which(error_Nw$w==Inf),]}
error_Nw$w[which(error_Nw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
error_Nw$diff = error_Nw$categ-error_Nw$indiv

time_Nw = time[which(with(parameters,interaction(c1,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
if(is.element(Inf,wind_vals)){
# time_Nw = time_Nw[-which(time_Nw$w==Inf),]}
time_Nw$w[which(time_Nw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
time_Nw$diff = time_Nw$categ-time_Nw$indiv

error_Nw_subset = error_Nw[which(error_Nw$c2==corr_vals[c2]),]
time_Nw_subset = time_Nw[which(time_Nw$c2==corr_vals[c2]),]

error_cw = error[which(with(parameters,interaction(N,pcat,pind,pobs,sep=','))==paste(N_vals[n],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
if(is.element(Inf,wind_vals)){
# error_cw = error_cw[-which(error_cw$w==Inf),]}
error_cw$w[which(error_cw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
error_cw$diff = error_cw$categ-error_cw$indiv

time_cw = time[which(with(parameters,interaction(N,pcat,pind,pobs,sep=','))==paste(N_vals[n],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
if(is.element(Inf,wind_vals)){
# time_cw = time_cw[-which(time_cw$w==Inf),]}
time_cw$w[which(time_cw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
time_cw$diff = time_cw$categ-time_cw$indiv

error_cw_subset = error_cw[which(error_cw$c2==corr_vals[c2]),]
time_cw_subset = time_cw[which(time_cw$c2==corr_vals[c2]),]

error_pw = error[which(with(parameters,interaction(N,c1,pcat,pind,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]
if(is.element(Inf,wind_vals)){
# error_pw = error_pw[-which(error_pw$w==Inf),]}
error_pw$w[which(error_pw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
error_pw$diff = error_pw$categ-error_pw$indiv

time_pw = time[which(with(parameters,interaction(N,c1,pcat,pind,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]
if(is.element(Inf,wind_vals)){
# time_pw = time_pw[-which(error_pw$w==Inf),]}
time_pw$w[which(time_pw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
time_pw$diff = time_pw$categ-error_pw$indiv

error_pw_subset = error_pw[which(error_pw$c2==corr_vals[c2]),]
time_pw_subset = time_pw[which(error_pw$c2==corr_vals[c2]),]

error_Nc = error[which(with(parameters,interaction(w,pcat,pind,pobs,sep=','))==paste(wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
error_Nc$diff = error_Nc$categ-error_Nc$indiv

time_Nc = time[which(with(parameters,interaction(w,pcat,pind,pobs,sep=','))==paste(wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
time_Nc$diff = time_Nc$categ-time_Nc$indiv

error_Nc_subset = error_Nc[which(error_Nc$c2==corr_vals[c2]),]
time_Nc_subset = time_Nc[which(time_Nc$c2==corr_vals[c2]),]
	
M = max(c(max(error_Nw_subset$categ),max(error_cw_subset$categ),max(error_pw_subset$categ),max(error_Nc_subset$categ)))
m = min(c(min(error_Nw_subset$categ),min(error_cw_subset$categ),min(error_pw_subset$categ),min(error_Nc_subset$categ)))
# m = 0
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
legend_breaks = round(seq(.1,.4,by=0.1),2)
legend_labels = legend_breaks
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),breaks = legend_breaks, labels = legend_labels)+labs(fill='Error')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(),legend.text=element_text(size=textsz))

contour_leg_error = get_legend(legend_plot)

plots.Nw[[1]] = ggplot(error_Nw_subset,aes(x=N,y=w,z=categ))+	
	geom_tile(aes(fill = categ)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Group size') + ylab('Memory window')

plots.cw[[1]] = ggplot(error_cw_subset,aes(x=c1,y=w,z=categ))+	
	geom_tile(aes(fill = categ)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Category width') + ylab('Memory window')
	
plots.pw[[1]] = ggplot(error_pw_subset,aes(x=pobs,y=w,z=categ))+
	geom_tile(aes(fill = categ)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Probability of obs') + ylab('Memory window')

plots.Nc[[1]] = ggplot(error_Nc_subset,aes(x=N,y=c1,z=categ))+
	geom_tile(aes(fill = categ)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
	ylab('Category width') + xlab('Group size')

	
M = max(c(max(time_Nw_subset$categ),max(time_cw_subset$categ),max(time_pw_subset$categ),max(time_Nc_subset$categ)))
m = min(c(min(time_Nw_subset$categ),min(time_cw_subset$categ),min(time_pw_subset$categ),min(time_Nc_subset$categ)))
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
legend_breaks =time_breaks
legend_labels = 10^legend_breaks
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),breaks = legend_breaks, labels = legend_labels)+labs(fill='Time')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(),legend.text=element_text(size=textsz))

contour_leg_time = get_legend(legend_plot)

plots.Nw[[2]] = ggplot(time_Nw_subset,aes(x=N,y=w,z=categ))+	
	geom_tile(aes(fill = categ)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Group size') + ylab('Memory window')

plots.cw[[2]] = ggplot(time_cw_subset,aes(x=c1,y=w,z=categ))+	
	geom_tile(aes(fill = categ)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Category width') + ylab('Memory window')		
	

plots.pw[[2]] = ggplot(time_pw_subset,aes(x=pobs,y=w,z=categ))+	
	geom_tile(aes(fill = categ)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Probability of obs') + ylab('Memory window')	
	
plots.Nc[[2]] = ggplot(time_Nc_subset,aes(y=c1,x=N,z=categ))+
	geom_tile(aes(fill = categ)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
	ylab('Category width') + xlab('Group size')	
		
pdf(file=paste(wd,"/parameter_interactions_badge.pdf",sep=''),width=6.8,height=5)		
grid.arrange(plots.Nw[[1]],plots.Nc[[1]],plots.cw[[1]],plots.pw[[1]],contour_leg_error,plots.Nw[[2]],plots.Nc[[2]],plots.cw[[2]],plots.pw[[2]],contour_leg_time,ncol=5,widths=c(0.3,0.3,0.3,0.3,0.13))	
dev.off()

M = max(c(max(error_Nw_subset$indiv),max(error_cw_subset$indiv),max(error_pw_subset$indiv),max(error_Nc_subset$indiv)))
m = min(c(min(error_Nw_subset$indiv),min(error_cw_subset$indiv),min(error_pw_subset$indiv),min(error_Nc_subset$indiv)))
# m = 0
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
legend_breaks = round(seq(0,0.5,by=0.1),2)
legend_labels = legend_breaks
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),breaks = legend_breaks, labels = legend_labels)+labs(fill='Error')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(),legend.text=element_text(size=textsz))

contour_leg_error = get_legend(legend_plot)

plots.Nw[[1]] = ggplot(error_Nw_subset,aes(x=N,y=w,z=indiv))+	
	geom_tile(aes(fill = indiv)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Group size') + ylab('Memory window')

plots.cw[[1]] = ggplot(error_cw_subset,aes(x=c1,y=w,z=indiv))+	
	geom_tile(aes(fill = indiv)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Category width') + ylab('Memory window')
	
plots.pw[[1]] = ggplot(error_pw_subset,aes(x=pobs,y=w,z=indiv))+
	geom_tile(aes(fill = indiv)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Probability of obs') + ylab('Memory window')
	
M = max(c(max(time_Nw_subset$indiv),max(time_cw_subset$indiv),max(time_pw_subset$indiv)))
m = min(c(min(time_Nw_subset$indiv),min(time_cw_subset$indiv),min(time_pw_subset$indiv)))
M = max(c(max(time_Nw_subset$indiv),max(time_pw_subset$indiv)))
m = min(c(min(time_Nw_subset$indiv),min(time_pw_subset$indiv)))
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
legend_breaks =time_breaks
legend_labels = 10^legend_breaks
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),breaks = legend_breaks, labels = legend_labels)+labs(fill='Time')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(),legend.text=element_text(size=textsz))

contour_leg_time = get_legend(legend_plot)

plots.Nw[[2]] = ggplot(time_Nw_subset,aes(x=N,y=w,z=indiv))+	
	geom_tile(aes(fill = indiv)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Group size') + ylab('Memory window')

plots.cw[[2]] = ggplot(time_cw_subset,aes(x=c1,y=w,z=indiv))+	
	geom_tile(aes(fill = indiv)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Category width') + ylab('Memory window')		
	

plots.pw[[2]] = ggplot(time_pw_subset,aes(x=pobs,y=w,z=indiv))+	
	geom_tile(aes(fill = indiv)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Probability of obs') + ylab('Memory window')		
		
pdf(file=paste(wd,"/parameter_interactions_indiv.pdf",sep=''),width=6.8,height=4)		
grid.arrange(plots.Nw[[1]],plots.pw[[1]],contour_leg_error,plots.Nw[[2]],plots.pw[[2]],contour_leg_time,ncol=3,widths=c(0.3,0.3,0.13))	
dev.off()

#####  how categorization works
N=100
qual_vals = rnorm(N, mean = 0, sd = 1) 
sig_vals = rnorm(N, mean = 0, 1)
sig_vals = fixCorr(qual_vals,sig_vals,sig_qual_corr)
sig_cats_temp = array(NA, dim = N)

perc_wind = 0.5
confus_prob_cat = Inf

left = 1:N
cat_now = 1
cat_med = array(NA,dim=0)
while(length(left)>0){
	if(length(left)>1){
	first = sample(left,size=1)} else{ first = left}
	to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
	if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf ){
				to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
			}
	sig_cats_temp[left[to_categorize]] = cat_now
	cat_now = cat_now+1
	cat_med = c(cat_med,mean(sig_vals[left[to_categorize]]))
	left = left[-to_categorize]
}


o = order(cat_med)
sig_cats = array(NA, dim =N)
for(i in 1:length(o)){
	sig_cats[sig_cats_temp==o[i]] = i
}
cat_med = sort(cat_med)
catnum = max(sig_cats)

setpal = brewer.pal(9,'Set1')
ind = (1:catnum)%%9
ind[ind==0] =9
palnow = setpal[ind]

o = order(sig_vals)

sig_vals = sig_vals[o]
sig_cats = as.factor(sig_cats[o])
ind = 1:N
# categories = data.frame(ind,sig_vals, sig_cats)
categories = data.frame(sig_vals=c(sig_vals,sig_vals),sig_cats=c(sig_vals,cat_med[sig_cats]),delta=c(rep(1,N),rep(0,N)))

perc_wind = 2
confus_prob_cat = Inf

left = 1:N
cat_now = 1
cat_med = array(NA,dim=0)
while(length(left)>0){
	if(length(left)>1){
	first = sample(left,size=1)} else{ first = left}
	to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
	if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf ){
				to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
			}
	sig_cats_temp[left[to_categorize]] = cat_now
	cat_now = cat_now+1
	cat_med = c(cat_med,mean(sig_vals[left[to_categorize]]))
	left = left[-to_categorize]
}


o = order(cat_med)
sig_cats = array(NA, dim =N)
for(i in 1:length(o)){
	sig_cats[sig_cats_temp==o[i]] = i
}
cat_med = sort(cat_med)
catnum = max(sig_cats)

categories = rbind(categories,data.frame(sig_vals=c(sig_vals),sig_cats=c(cat_med[sig_cats]),delta=c(rep(-1,N))))
categories$colors =(round(1/2*(categories$sig_cats+1),2)*100)
num_cols=diff(range(categories$colors))+1
offset = 5
reds = colorRampPalette(brewer.pal(9,'Reds'))(num_cols+offset)
reds = reds[(1:num_cols)+offset]
reds = reds[sort(unique(categories$colors-min(categories$colors)+1))]
categories$colors=as.factor(categories$colors)

m=min(-1,min(sig_vals))
M=max(1,max(sig_vals))

plot_cats = ggplot(categories, aes(y = delta, x = sig_vals, colour = colors)) + 
		geom_point(aes(size=sig_cats))  +
		theme_bw() +
		theme(
        	axis.ticks.y=element_blank(),
        	axis.text.y=element_blank(), axis.line.y=element_blank(),text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.title=element_text(size=10),legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.margin=unit(c(1,1,1,1),"cm"),panel.border=element_blank()) +		scale_color_manual(values=reds)+	scale_x_continuous(limit=c(m,M))+
		xlab("Signal")+ylab('')
		
pdf(file=paste(wd,'/categories.pdf',sep=''),width=6,height=4)
print(plot_cats)
dev.off()

# ##--- how many categories?

N_vals = c(20,40,80,100)
xN = length(N_vals)
perc_vals = seq(2,0,by=-0.2)
xp = length(perc_vals)
corr_vals = c(0.5,0.9)
xc = length(corr_vals)

ybreaks = c(2,5,20,80)

cat_mean = array(NA,dim=c(xN,xp,xc))
cat_sd = array(NA,dim=c(xN,xp,xc))

runs = 100

for(n in 1:xN){
	for(p in 1:xp){
		for(c2 in 1:xc){
			N = N_vals[n]
			perc_wind = perc_vals[p]
			sig_qual_corr = corr_vals[c2]
			hold_catnum = array(NA,dim=runs)
			for(r in 1:runs){
				qual_vals = rnorm(N, mean = 0, sd = 1) 
				sig_vals = rnorm(N, mean = 0, 1)
				sig_vals = fixCorr(qual_vals,sig_vals,sig_qual_corr)
				sig_cats_temp = array(NA, dim = N)
				left = 1:N
				cat_now = 1
				cat_med = array(NA,dim=0)
				while(length(left)>0){
					if(length(left)>1){
					first = sample(left,size=1)} else{ first = left}
					to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
					if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf ){
								to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
							}
					sig_cats_temp[left[to_categorize]] = cat_now
					cat_now = cat_now+1
					cat_med = c(cat_med,mean(sig_vals[left[to_categorize]]))
					left = left[-to_categorize]
				}
				hold_catnum[r] = max(sig_cats_temp)
			}
		cat_mean[n,p,c2] = mean(hold_catnum)
		cat_sd[n,p,c2] = sd(hold_catnum)	
		}
	}
}

cat_mean = melt(cat_mean,varnames=c('groupsize','percwind','corr'))
names(cat_mean)[4]='catnum'
cat_mean$groupsize = as.factor(rep(N_vals,xp*xc))
cat_mean$percwind = rep(perc_vals,each=xN,times=xc)
cat_mean$corr = as.factor(rep(corr_vals,each=xN*xp))
cat_mean$catnum = log(cat_mean$catnum)
cat_sd = melt(cat_sd,varnames=c('groupsize','percwind','corr'),value.name='sd')
names(cat_sd)[4]='sd'
cat_sd$groupsize = as.factor(rep(N_vals,xp*xc))
cat_sd$percwind = rep(perc_vals,each=xN,times=xc)
cat_sd$corr = as.factor(rep(corr_vals,each=xN*xp))
cat_sd$sd = log(cat_sd$sd)

plot_catnum = ggplot(cat_mean, aes(x=percwind,y=catnum, colour = groupsize, linetype = corr)) + 	
	geom_line() + geom_point() +
	theme_bw() + scale_y_continuous(limits=log(c(2,100)),breaks=log(ybreaks),labels=ybreaks) +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ) + 
		scale_color_manual(values=mypal[1:xN]) + 
		labs(linetype="Corr", colour="Group size")  + xlab('Category width')+ylab("Number of categories")

pdf(file=paste(wd,'/number_of_categories.pdf',sep=''),width=5,height=3.4)
print(plot_catnum)
dev.off()

###### learning time example
Tfights = 500 #total number of fights 
# N = 20 # individuals
# perc_wind = 0.1 # difference that animals can perceive
# memory_window = Inf #how many fights ago they can remember
# confus_prob_cat = 1000 #maximum probability of misidentifying categories, which decreases with dissimilarity
# confus_prob_ind = 0 #probability of misidentifying individuals
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
# sig_qual_corr = 0.5 #correlation between quality and signal
learn_rate = 0.2 #how much the quality of the opponent affects the new assessment
learn_noise = 0.01 #how noisy an updated assessment is
dominance = 2 #how quickly the probability switches from A winning to A losing
error_threshold = 0.2

qual_vals_tot = list()
a_cat_tot = list()
a_ind_tot = list()
error_cat_tot =list()
error_ind_tot =list()
time_cat_tot = list()
time_ind_tot = list()

w = 1

N = 6
sig_qual_corr = 0.7
perc_wind = 1
memory_window = Inf
confus_prob_cat = Inf
confus_prob_ind = 0
qual_vals = array(data = NA, dim = N) # quality values
	# #option: one animal in each of N bins
	# qual_vals = seq(0, 1, by = 1/N) 
	# #option: uniformly distributed quality values
	# qual_vals = runif(N , min = 0, max = 1) 
	# #option: lognormally distributed quality values
	# qual_vals = exp(rnorm(N, mean = 0, sd = qual_sd)) 
	#option: normally distributed quality values
	qual_vals = rnorm(N, mean = qual_mean, sd = qual_sd) 
	
	# #matrix of columns of quality values
	# qual_mat = matrix(rep(qual_vals,N), ncol=N, byrow=TRUE) 
	
	sig_vals = array(data = NA, dim = N) # signal values
	#option: signals are correlated with quality 
	sig_vals = rnorm(N, mean = 0, 1)
	sig_vals = fixCorr(qual_vals,sig_vals,sig_qual_corr)
	# #option: uniformly distributed sigals
	# sig_vals = runif(N, min =0, max = 1) 
	# #option: lognormally distributed signals: 
	# sig_sd = 0.5; sig_vals = exp(rnorm(N, mean =0, sd = sig_sd)) 
	# #option: normally distributed signal values
	# sig_sd = 0.5; sig_vals = rnorm(N, mean =0, sd =sig_sd) 
	
	sig_cats = array(NA, dim =c(N,N))
	categor_num = array(0, dim=N)
	confus_mat = as.list(rep(NA,N))
	
	for(i in 1:N){
		sig_cats_temp = array(NA, dim = N)
		left = 1:N
		cat_now = 1
		cat_med = array(NA,dim=0)
		while(length(left)>0){
			if(length(left)>1){
			first = sample(left,size=1)} else{ first = left}
			to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
			sig_cats_temp[left[to_categorize]] = cat_now
			cat_now = cat_now+1
			cat_med = c(cat_med,median(sig_vals[left[to_categorize]]))
				left = left[-to_categorize]
		} 
		
		o = order(cat_med)
		
		for(j in 1:length(o)){
			sig_cats[i,sig_cats_temp==o[j]] = j
		}
		cat_med = sort(cat_med)
		categor_num[i] = max(sig_cats_temp)
		confus_mat[[i]] = array(0, dim=c(N,categor_num[i])) #confus_mat[[i]][j,k] = prob i perceives j to be in cat k
		if(confus_prob_cat==Inf){
			m = matrix(1,N,categor_num[i])
			m[lower.tri(m,diag=FALSE)] = 0 
			m = m[sig_cats[i,],]
			confus_mat[[i]] = m
		} else {for(j in 1:N){
			confus_mat[[i]][j,]=cumsum(exp(-confus_prob_cat*abs(sig_vals[j]-cat_med))/sum(exp(-confus_prob_cat*abs(sig_vals[j]-cat_med)))) #easier to keep track of cumulative confusion probabilities
		} }
	}
	
	categor_num_max = max(categor_num)
	
	#the fighting and learning dynamics: 
	wins = list() # keep track of qualities of members involved in wins and losses for each animal
	losses = list()
	for(i in 1:N){ 
		wins[[i]] = numeric()
		losses[[i]] = numeric()
	}
	
	last_fights_cat = array(Inf, dim=c(N,categor_num_max)) #last time each individual thought it encountered each category
	last_fights_ind = array(Inf, dim=c(N,N)) #last time each individual thought it encountered each category
	
	a_cat_bycat = array(NA, dim=c(N,categor_num_max,Tfights+1)) #assessment by each individual of each other individual, using categories
	a_cat_byind = array(NA, dim=c(N,N,Tfights+1)) #assessment by each indivdual of each category
	a_ind = array(NA, dim=c(N,N,Tfights+1)) #assessment by each indivdual of each category
		
	for(t in 1:Tfights){
		pair = sample(1:N, 2, replace = FALSE) #draw two animals at random
		p = win_prob(qual_vals[pair]) # probability of first animal winning
		outcome = sample(0:1,1,prob = c(p,1-p)) # see who wins and loses
		if(outcome == 0){
			wins[[pair[1]]] = cbind(wins[[pair[1]]],qual_vals[pair]) #add quality values to current wins for pair[1]
			losses[[pair[2]]] = cbind(losses[[pair[2]]],qual_vals[rev(pair)]) #add quality values to current losses for pair[2], reversed so focal animal is first
		} else{
			wins[[pair[2]]] = cbind(wins[[pair[2]]],qual_vals[rev(pair)])
			losses[[pair[1]]] = cbind(losses[[pair[1]]],qual_vals[pair])
		}
		
		#learning about the signal of one's opponent:
			draw = matrix(runif(N*N,0,1),nrow=N) #random numbers to generate confusion events
			perc_cats = array(NA, dim = c(N,N)) #each individual's perception of every other's category
			for(i in 1:N){
				perc_cats[i,] = rowSums(matrix(rep(draw[i,],categor_num[i]),ncol=categor_num[i])>confus_mat[[i]])+1 #use confus_mat to see which category perceptions get switched to
			}
			# if(sum(perc_cats[1,]-sig_cats[1,])!=0){print(rbind(perc_cats[1,],sig_cats[1,]))}
				# perc_cats = matrix(rep(sig_cats,N),nrow=N,byrow=TRUE) #no switching categories
			
			last_fights_cat[pair[1],perc_cats[pair[1],pair[2]]] = 0 #each animal thinks it just fought with the category it perceived
			last_fights_cat[pair[2],perc_cats[pair[2],pair[1]]] = 0
			new_as_cat_bycat = a_cat_bycat[,,t] #if you weren't involved in the fight your assessment doesn't change
			new_as_cat_bycat[last_fights_cat>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago
			a_cat_bycat[,,t+1] = new_as_cat_bycat
			a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]],t+1] = update(a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]],t],qual_vals[pair[2]]) #each animal updates its assessment of the category it perceives based on the quality it experiences
			a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]],t+1] = update(a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]],t],qual_vals[pair[1]])
			
			new_as_cat_byind = array(NA,dim=c(N,N))
			for(i in 1:N){
				new_as_cat_byind[i,]=a_cat_bycat[i,perc_cats[i,],t+1] #each animal assigns quality values to individuals based on its sloppy assignment of individuals to categories
			}
			a_cat_byind[,,t+1] = new_as_cat_byind
			
			last_fights_cat = last_fights_cat+1
		
		#learning about the identity of one's opponent:
			draw = runif(2,0,1) #random numbers to generate confusion events
			perc_pair = pair  
			if(draw[1]<confus_prob_ind){
				perc_pair[1] = sample(setdiff(1:N,pair[1]),1) #with low probability draw a different individual that the focal thinks it's interacting with
			}
			if(draw[2]<confus_prob_ind){
				perc_pair[2] = sample(setdiff(1:N,pair[2]),1)
			}
			
			last_fights_ind[pair[1],perc_pair[2]] = 0
			last_fights_ind[pair[2],perc_pair[1]] = 0
			new_as_ind = a_ind[,,t] #if you weren't involved in the fight your assessment doesn't change
			new_as_ind[last_fights_ind>memory_window] = NA #if you fought more than memory_window fights ago you forget your assessments
			a_ind[,,t+1] = new_as_ind
			a_ind[pair[1],perc_pair[2],t+1] = update(a_ind[pair[1],perc_pair[2],t],qual_vals[pair[2]]) #each animal updates its assessment of the individual it perceives based on the quality it experiences
			a_ind[pair[2],perc_pair[1],t+1] = update(a_ind[pair[2],perc_pair[1],t],qual_vals[pair[1]])
			last_fights_ind = last_fights_ind+1
	}
	
	#how well did they learn?
	error_cat = array(NA, dim = c(N,Tfights+1))
	error_ind = array(NA, dim = c(N,Tfights+1))
	#how quickly did they get close to their final assessment?
	time_cat = array(NA, dim=c(N,1))
	time_ind = array(NA, dim=c(N,1))
	# #what was everyone else's opinion of me over time?
	# highrank_cat = array(NA, dim=c(N,Tfights+1))
	# highrank_ind = array(NA, dim=c(N,Tfights+1))
	# #costs of losing? while fights are random, these don't depend on the recognition system, but we'll need two if we consider strategic fights.
	# losing_costs_cat = array(NA, dim=c(N,Tfights+1))
	# losing_costs_ind = array(NA, dim=c(N,Tfights+1))
	time_cat_temp = array(NA,dim=c(N,N))
		time_ind_temp = array(NA, dim=c(N,N))
	for(i in 1:N){
		for(t in 1:(Tfights+1)){
	
			if(sum(!is.na(a_cat_byind[i,,t]))>=1){
				error_cat[i,t] = sum(abs(a_cat_byind[i,,t]-qual_vals) ,na.rm=TRUE) /sum(!is.na(a_cat_byind[i,,t])) #/ abs(qual_vals)
				}
			if(sum(!is.na(a_ind[i,,t]))>=1){
				error_ind[i,t] = sum(abs(a_ind[i,,t]-qual_vals ) ,na.rm=TRUE) /sum(!is.na(a_ind[i,,t]))  #/ abs(qual_vals)
				}
			# if(sum(!is.na(a_cat_byind[,i,t]))>=1){
			# highrank_cat[i,t] = sum(a_cat_byind[,i,t],na.rm=TRUE) /sum(!is.na(a_cat_byind[,i,t]))
				# }
			# if(sum(!is.na(a_ind[,i,t]))>=1){
				# highrank_ind[i,t] = sum(a_ind[,i,t],na.rm=TRUE) /sum(!is.na(a_ind[,i,t]))
				# }
		}
		# time_cat_temp = array(NA,dim=N)
		# time_ind_temp = array(NA, dim=N)
		for(j in setdiff(1:N,i)){
			# v = which(abs(a_cat_byind[i,j,]-a_cat_byind[i,j,Tfights+1])<= error_threshold)
			v = which(abs(a_cat_byind[i,j,]-qual_vals[j])<=error_threshold)
			if(length(v)>0){
				time_cat_temp[i,j] = v[1]-1
			} else{ time_cat_temp[i,j] = Tfights}
			# v = which(abs(a_ind[i,j,]-a_ind[i,j,Tfights+1])<= error_threshold)
			v = which(abs(a_ind[i,j,]-qual_vals[j])<=error_threshold)
			if(length(v)>0){ 
				time_ind_temp[i,j] = v[1]-1
			} else{ time_ind_temp[i,j] = Tfights}
		}
		time_cat[i] = median(time_cat_temp,na.rm=TRUE)
		time_ind[i] = median(time_ind_temp,na.rm=TRUE)
	}
	
qual_vals_tot[[w]] = qual_vals
error_cat_tot[[w]] = error_cat
error_ind_tot[[w]] = error_ind
a_cat_tot[[w]] = a_cat_byind
a_ind_tot[[w]] = a_ind
time_cat_tot[[w]] = time_cat_temp
time_ind_tot[[w]] = time_ind_temp

pal = brewer.pal(N-1,'Set1')

Ylim = c(0,1)

w = 1
i = 1
error_ex = abs(a_cat_tot[[w]][i,,]-matrix(rep(qual_vals_tot[[w]],Tfights+1),nrow=N,byrow=FALSE))
error_ex = error_ex[-i,]
error_mean = colMeans(error_ex,na.rm=TRUE)
error_ex = rbind(error_ex,error_mean)
error_ex = data.frame(error=as.vector(t(error_ex)),fights=rep(1:(Tfights+1),N),ind=as.factor(rep(c(setdiff(1:N,i),'mean'),each=Tfights+1)))
for(k in setdiff(1:N,i)){
	tmp = data.frame(error=c(Ylim[1],Ylim[2]),fights=rep(time_cat_tot[[w]][i,k],2),ind=as.factor(rep(k,2)))
	error_ex = rbind(error_ex,tmp)
}
tmp = tmp = data.frame(error=c(Ylim[1],Ylim[2]),fights=rep(mean(time_cat_tot[[w]][i,],na.rm=TRUE),2),ind=as.factor(rep('mean',2)))
error_ex = rbind(error_ex,tmp)


ex = ggplot(error_ex,aes(x=fights, y = error, colour = ind)) + 
	geom_line() + 
	theme_bw() + 
	scale_y_continuous(limits=Ylim) +
	theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10),plot.margin=unit(c(0,0.25,0,0),"cm"),legend.position='none') +
	xlab("Time")+ylab("Error") +
	scale_color_manual(values=c(pal,'black'))+
	geom_segment(aes(x=1,xend=Tfights+1,y=error_threshold,yend=error_threshold,color='mean')) 

pdf(file="/Users/eleanorbrush/Desktop/learning_time_example.pdf",width=5,height=3)		
print(ex)
dev.off()

# cost functions

poss_perc_vals = seq(0,2,length.out=100)
poss_wind_vals = seq(0,3000,length.out=100)

perc_cost = data.frame(c1=rep(poss_perc_vals,times=length(a_perc_vals)),a_perc=rep(a_perc_vals,each=length(poss_perc_vals)))
# perc_cost$cost = with(perc_cost,(2/(1+exp(-a_perc*(2-c1)))-1))
perc_cost$cost = with(perc_cost,((2-c1)/2)^a_perc)
perc_cost$a_perc = as.factor(round(perc_cost$a_perc,2))

perc_ex = ggplot(perc_cost,aes(x=c1,y=cost,linetype=a_perc))+geom_line()+
	scale_linetype_manual(values=c(3,1)) +
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(colour="",linetype="") + scale_y_continuous(limits=c(0,1))+
	xlab("Category width")+ylab("Cost")

cost_leg = get_legend(perc_ex)

perc_ex = perc_ex + theme(legend.position='none')

wind_cost = data.frame(w=rep(poss_wind_vals,times=length(a_wind_vals)),a_wind=rep(a_wind_vals,each=length(poss_wind_vals)))
# wind_cost$cost = with(wind_cost,(2/(1+exp(-a_wind*w))-1))
wind_cost$cost = with(wind_cost,(w/3000)^a_wind)
wind_cost$a_wind = as.factor(wind_cost$a_wind)

wind_ex = ggplot(wind_cost,aes(x=w,y=cost,linetype=a_wind))+geom_line()+
	scale_linetype_manual(values=c(3,1)) +
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(colour="",linetype="") + scale_y_continuous(limits=c(0,1))+
	xlab("Memory window")+ylab("Cost")+theme(legend.position='none')

pdf(file=paste(wd,'/cost_functions.pdf',sep=""),width=6.8,height=2)
grid.arrange(perc_ex,wind_ex,cost_leg,ncol=3,widths=c(1,1,0.3))
dev.off()