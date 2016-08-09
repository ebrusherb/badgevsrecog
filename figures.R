library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)
library(car)
source('multiplot.R')
source('ind2sub.R')
source('sub2ind.R')
source('get_legend.R')
source('combine_files.R')
setwd('/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/')

param_plot <- function(data,mapping,ymin,ymax,ybreaks=NULL,ylabels=NULL){
	plot = ggplot(data,mapping)+ 
	geom_line() + geom_point() +
	guides(color=guide_legend(order=1),linetype=guide_legend(order=3)) + 	
	scale_color_manual(values=c(divpal[1],divpal[5]))+scale_linetype_manual(values=c(3,1)) +
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(colour="",linetype="")
	if(is.null(ybreaks) && is.null(ylabels)){
		plot = plot+scale_y_continuous(limits=c(ymin,ymax))
	} else{ plot = plot + scale_y_continuous(limits=c(ymin,ymax),breaks=ybreaks,labels=ylabels)
		}
	return(plot)
}

perc_cost<-function(perc_wind){
	cost = 2/(1+exp(a_perc*(perc_wind-2)))-1
	return(cost)
}

wind_cost<-function(wind){
	cost = 2/(1+exp(a_wind*wind))-1
	return(cost)
}

mypal=brewer.pal(9,'Set1')
divpal = brewer.pal(5,'RdBu')

load("/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/summary_stats_2016_08_06.Rdata")
error = cbind(data.frame(categ=as.vector(error_cat_mean),indiv=as.vector(error_ind_mean)),parameters)
time = cbind(data.frame(categ=log(as.vector(time_cat_mean),base=10),indiv=log(as.vector(time_ind_mean),base=10)),parameters)

N_vals = sort(unique(parameters$N))
xN = length(N_vals)
perc_vals = sort(unique(parameters$c1))
xperc = length(perc_vals)
wind_vals = sort(unique(parameters$w))
xwind = length(wind_vals)
confus_cat_vals = sort(unique(parameters$pcat),decreasing=TRUE)
xconfus_cat = length(confus_cat_vals)
confus_ind_vals = sort(unique(parameters$pind))
xconfus_ind = length(confus_ind_vals)
corr_vals = sort(unique(parameters$c2))
xcorr = length(corr_vals)

n = which(N_vals==50)
c1 = 3
w = which(wind_vals==1000)
c2 = 2
pcat = 1 
pind = 1
error_max = 0.42
time_min = min(time[,1:2])-0.1
time_max = max(time[,1:2])+0.1
time_labels = c(500,1000,2000,4000,8000)
time_breaks = log(time_labels,base=10)


marg = c(0.1,0.1,0,0)
textsz = 10

plots.error = list()	
plots.time = list()
#group size

error_N = error[intersect(which(error$c1==perc_vals[c1]),intersect(which(error$w==wind_vals[w]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1])))),]
error_N = with(error_N,data.frame(error=c(categ,indiv),N=rep(N,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_N)[1]))))
error_N = error_N[-intersect(which(error_N$categ=='Indiv'),which(error_N$corr==corr_vals[1])),]

plots.error[[1]] = param_plot(error_N, aes(x=N, y = error, colour = categ, linetype = corr),0,error_max) + 
	xlab("Group size") + ylab("Error") 	

legend <- get_legend(plots.error[[1]])
plots.error[[1]] = plots.error[[1]] + theme(legend.position='none')

time_N = time[intersect(which(time$c1==perc_vals[c1]),intersect(which(time$w==wind_vals[w]),intersect(which(time$pcat==confus_cat_vals[1]),which(time$pind==confus_ind_vals[1])))),]
time_N = with(time_N,data.frame(time=c(categ,indiv),N=rep(N,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_N)[1]))))
time_N = time_N[-intersect(which(time_N$categ=='Indiv'),which(time_N$corr==corr_vals[1])),]

plots.time[[1]] = param_plot(time_N, aes(x=N, y = time, colour = categ, linetype = corr),time_min,time_max,ybreaks=time_breaks,ylabels=time_labels) + 
	xlab("Group size") + ylab("Time") + theme(legend.position='none')

#memory window
error_w = error[intersect(which(error$c1==perc_vals[c1]),intersect(which(error$N==N_vals[n]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1])))),]
error_w = with(error_w,data.frame(error=c(categ,indiv),w=rep(w,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_w)[1]))))
error_w = error_w[-intersect(which(error_w$categ=='Indiv'),which(error_w$corr==corr_vals[1])),]
if(is.element(Inf,wind_vals)){
error_w = error_w[-which(error_w$w==Inf),]}

plots.error[[2]] = param_plot(error_w, aes(x=w, y = error, colour = categ, linetype = corr),0,error_max) + 	
	xlab("Memory window")+ ylab("") + theme(legend.position='none')

time_w = time[intersect(which(time$c1==perc_vals[c1]),intersect(which(time$N==N_vals[n]),intersect(which(time$pcat==confus_cat_vals[1]),which(time$pind==confus_ind_vals[1])))),]
time_w = with(time_w,data.frame(time=c(categ,indiv),w=rep(w,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_w)[1]))))
time_w = time_w[-intersect(which(time_w$categ=='Indiv'),which(time_w$corr==corr_vals[1])),]
if(is.element(Inf,wind_vals)){
time_w = time_w[-which(time_w$w==Inf),]}

plots.time[[2]] = param_plot(time_w, aes(x=w, y = time, colour = categ, linetype = corr),time_min,time_max,ybreaks=time_breaks,ylabels=time_labels) + 	
	xlab("Memory window")+ ylab("") + theme(legend.position='none')
	
	
#perception window
error_c1 = error[intersect(which(error$w==wind_vals[w]),intersect(which(error$N==N_vals[n]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1])))),]
error_c1 = with(error_c1,data.frame(error=c(categ,indiv),c1=rep(c1,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_c1)[1]))))
error_c1 = error_c1[-intersect(which(error_c1$categ=='Indiv'),which(error_c1$corr==corr_vals[1])),]
# ind_mean = mean(error_c1$error[which(error_c1$categ=='Indiv')])
# error_c1$error[which(error_c1$categ=='Indiv')] = ind_mean
wo_error = subset(error_c1,select=-error)
agg_mean = aggregate(error_c1$error,by=list(with(wo_error,interaction(c1,corr,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
error_c1 = cbind(data.frame(error=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(error_c1)[2:4]=colnames(wo_error)
error_c1$c1=as.numeric(levels(error_c1$c1))[error_c1$c1]

plots.error[[3]] = param_plot(error_c1, aes(x=c1, y = error, colour = categ, linetype = corr),0,error_max) + 	
	xlab("Category width")+ ylab("") + theme(legend.position='none')

time_c1 = time[intersect(which(time$w==wind_vals[w]),intersect(which(time$N==N_vals[n]),intersect(which(time$pcat==confus_cat_vals[1]),which(time$pind==confus_ind_vals[1])))),]
time_c1 = with(time_c1,data.frame(time=c(categ,indiv),c1=rep(c1,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_c1)[1]))))
time_c1 = time_c1[-intersect(which(time_c1$categ=='Indiv'),which(time_c1$corr==corr_vals[1])),]
# ind_mean = mean(time_c1$time[which(time_c1$categ=='Indiv')])
# time_c1$time[which(time_c1$categ=='Indiv')] = ind_mean
wo_time = subset(time_c1,select=-time)
agg_mean = aggregate(time_c1$time,by=list(with(wo_time,interaction(c1,corr,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
time_c1 = cbind(data.frame(time=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(time_c1)[2:4]=colnames(wo_time)
time_c1$c1=as.numeric(levels(time_c1$c1))[time_c1$c1]

plots.time[[3]] = param_plot(time_c1, aes(x=c1, y = time, colour = categ, linetype = corr),time_min,time_max,ybreaks=time_breaks,ylabels=time_labels) + 	
	xlab("Category width")+ ylab("") + theme(legend.position='none')


#probability of switching perception 
source('confus_cat_to_confus_prob.R')
error_p = error[intersect(which(error$w==wind_vals[w]),intersect(which(error$N==N_vals[n]),which(error$c1==perc_vals[c1]))),]
recode_vec = c()
for(i in 1:length(confus_cat_vals)){
	recode_vec = paste(recode_vec,'confus_cat_vals[',i,']=confus_prob_vals[',i,'];',sep="")
}
recode_vec = substr(recode_vec,start=1,stop=nchar(recode_vec)-1)
error_p$pcat =recode(error_p$pcat,recode_vec)
error_p = with(error_p,data.frame(error=c(categ,indiv),p=c(pcat,pind),corr=as.factor(rep(c2,times=2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_p)[1]))))
error_p = error_p[-intersect(which(error_p$categ=='Indiv'),which(error_p$corr==corr_vals[1])),]
wo_error = subset(error_p,select=-error)
agg_mean = aggregate(error_p$error,by=list(with(wo_error,interaction(p,corr,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
error_p = cbind(data.frame(error=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(error_p)[2:4]=colnames(wo_error)
error_p$p=as.numeric(levels(error_p$p))[error_p$p]

plots.error[[4]] = param_plot(error_p, aes(x=p, y = error, colour = categ, linetype = corr),0,error_max) + 	
	xlab("Probability of recategorizing")+ylab("") + theme(legend.position='none')	
	

time_p = time[intersect(which(time$w==wind_vals[w]),intersect(which(time$N==N_vals[n]),which(time$c1==perc_vals[c1]))),]
time_p$pcat =recode(time_p$pcat,"confus_cat_vals[1]=confus_prob_vals[1];confus_cat_vals[2]=confus_prob_vals[2];confus_cat_vals[3]=confus_prob_vals[3];confus_cat_vals[4]=confus_prob_vals[4]")
time_p = with(time_p,data.frame(time=c(categ,indiv),p=c(pcat,pind),corr=as.factor(rep(c2,times=2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_p)[1]))))
time_p = time_p[-intersect(which(time_p$categ=='Indiv'),which(time_p$corr==corr_vals[1])),]
wo_time = subset(time_p,select=-time)
agg_mean = aggregate(time_p$time,by=list(with(wo_time,interaction(p,corr,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
time_p = cbind(data.frame(time=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(time_p)[2:4]=colnames(wo_time)
time_p$p=as.numeric(levels(time_p$p))[time_p$p]
plots.time[[4]] = param_plot(time_p, aes(x=p, y = time, colour = categ, linetype = corr),time_min,time_max,ybreaks=time_breaks,ylabels=time_labels) + 	
	xlab("Probability of recategorizing")+ylab("") + theme(legend.position='none')	
	
	
# pdf(file="/Users/eleanorbrush/Desktop/parameters.pdf",width=6.8,height=8)		
# grid.arrange(plots.error[[1]],legend,plots.error[[2]],plots.error[[3]],plots.error[[4]],ncol=2,widths=c(1,0.1))
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,3,heights=rep(1,4)/4,widths=c(0.45,0.45,0.1))))
print(plots.error[[1]], vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(plots.error[[2]], vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(plots.error[[3]], vp=viewport(layout.pos.row=3,layout.pos.col=1))
print(plots.error[[4]], vp=viewport(layout.pos.row=4,layout.pos.col=1))
print(plots.time[[1]], vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(plots.time[[2]], vp=viewport(layout.pos.row=2,layout.pos.col=2))
print(plots.time[[3]], vp=viewport(layout.pos.row=3,layout.pos.col=2))
print(plots.time[[4]], vp=viewport(layout.pos.row=4,layout.pos.col=2))
legend$vp=viewport(layout.pos.row=1,layout.pos.col=3)
grid.draw(legend)
# dev.off()

##contours of comparison
plots.diff.Nw=list()
plots.diff.cw = list()
c2 = 1

error_Nw = error[intersect(which(error$c1==perc_vals[c1]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1]))),]
error_Nw = error_Nw[-which(error_Nw$w==wind_vals[xwind]),]
error_Nw$diff = error_Nw$categ-error_Nw$indiv

error_cw = error[intersect(which(error$N==N_vals[n]),intersect(which(error$pcat==confus_cat_vals[pcat]),which(error$pind==confus_ind_vals[pind]))),]
error_cw = error_cw[-which(error_cw$w==wind_vals[xwind]),]
error_cw$diff = error_cw$categ-error_cw$indiv

M = max(max(error_Nw$diff),max(error_cw$diff))
m = -M
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
legend_breaks = c(round(m,1),0,round(M,1))
legend_labels = legend_breaks
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=rev(divpal),limits=c(m,M),breaks = legend_breaks, labels = legend_labels)+labs(fill='')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(),legend.text=element_text(size=textsz))

contour_leg = get_legend(legend_plot)

for(c2 in 1:xcorr){
	error_Nw_subset = error_Nw[which(error_Nw$c2==corr_vals[c2]),]
	error_cw_subset = error_cw[which(error_cw$c2==corr_vals[c2]),]
	
	plots.diff.Nw[[c2]] = ggplot(error_Nw_subset,aes(x=N,y=w,z=diff))+	
		geom_tile(aes(fill = diff)) + 
		scale_fill_gradientn(colours=rev(divpal),limits=c(m,M),guide="colorbar")+
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
		scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
		xlab('Group size') + ylab('Memory window')
	
	plots.diff.cw[[c2]] = ggplot(error_cw_subset,aes(x=c1,y=w,z=diff))+	
		geom_tile(aes(fill = diff)) + 
		scale_fill_gradientn(colours=rev(divpal),limits=c(m,M),guide="colorbar")+
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
		scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
		xlab('Category width') + ylab('Memory window')
}
# pdf(file="/Users/eleanorbrush/Desktop/comparison.pdf",width=6.8,height=3)		
grid.arrange(plots.diff.Nw[[1]],plots.diff.cw[[1]],contour_leg,plots.diff.Nw[[2]],plots.diff.cw[[2]],ncol=3,widths=c(0.45,0.45,0.1))	
# dev.off()

###########costs 
n = which(N_vals==50)
c1 = 4
w = which(wind_vals==1000)
c2 = 1

w_error =10
w_time = 0.001
w_perc = 1
w_wind = 10
cost_max = 40

a_perc_vals = -c(0.1,3)
xaperc = length(a_perc_vals)
a_wind_vals = c(-0.0001,-.001)
xawind = length(a_wind_vals)
total_cost = function(error,time,c1_eff,w,ap=a_perc,aw=a_wind){	
	w_error*error+w_time*time+w_perc*(2/(1+exp(ap*(2-c1_eff)))-1)+w_wind*(2/(1+exp(aw*w))-1)
}

plots.cost = list()

a_perc = a_perc_vals[1]
a_wind = a_wind_vals[1]
cost_N = error[intersect(which(error$c1==perc_vals[c1]),intersect(which(error$w==wind_vals[w]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1])))),]
cost_N = with(cost_N,data.frame(error=c(categ,indiv),N=rep(N,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(cost_N)[1])),c1_eff=rep(c1,2),w=rep(w,2)))
cost_N$c1_eff[which(cost_N$categ=='Indiv')]=0
cost_N = cbind(10^(as.vector(unlist(time[intersect(which(error$c1==perc_vals[c1]),intersect(which(error$w==wind_vals[w]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1])))),1:2]))),cost_N)
names(cost_N)[1] = 'time'
cost_N = cost_N[-intersect(which(cost_N$categ=='Indiv'),which(cost_N$corr==corr_vals[1])),]
cost_N = with(cost_N,data.frame(N=N,corr=corr,categ=categ,cost=total_cost(error,time,c1_eff,w,a_perc,a_wind)))

plots.cost[[1]] = param_plot(cost_N, aes(x=N, y = cost, colour = categ, linetype = corr),0,cost_max) + 
	xlab("Group size") + ylab("Cost") 
	
w_error =10
w_time = 0.00001
w_perc = 0
w_wind = 5
cost_max = 10

cost_w = error[intersect(which(error$c1==perc_vals[c1]),intersect(which(error$N==N_vals[n]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1])))),]
cost_w = with(cost_w,data.frame(error=c(categ,indiv),w=rep(w,2),corr=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(cost_w)[1])),c1_eff=rep(c1,2)))
cost_w$c1_eff[which(cost_w$categ=='Indiv')]=0
cost_w = cbind(10^(as.vector(unlist(time[intersect(which(error$c1==perc_vals[c1]),intersect(which(error$N==N_vals[n]),intersect(which(error$pcat==confus_cat_vals[1]),which(error$pind==confus_ind_vals[1])))),1:2]))),cost_w)
names(cost_w)[1] = 'time'
cost_w = cost_w[which(cost_w$corr==corr_vals[c2]),]
cost_w = cost_w[-which(cost_w$w==Inf),]
a_wind_vec = rep(a_wind_vals,each=dim(cost_w)[1])
cost_w = rbind(cost_w,cost_w)
cost_w$a_wind = a_wind_vec
cost_w =with(cost_w,data.frame(w=w,categ=categ,a_wind=as.factor(a_wind),cost=total_cost(error,time,c1_eff,w,a_perc,a_wind)))

plots.cost[[2]] = param_plot(cost_w, aes(x=w, y = cost, colour = categ, linetype = a_wind),0,cost_max) + 	
	xlab("Memory window")+ ylab("Cost") #+ theme(legend.position='none')

