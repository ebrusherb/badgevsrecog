setwd('/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/')
wd = '/Users/eleanorbrush/Desktop'
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(foreach)
# library(parallel)
# library(doParallel)
library(car)
library(matrixStats)
library(plyr)
library(gtable)
library(reshape)
source('multiplot.R')
source('ind2sub.R')
source('sub2ind.R')
source('get_legend.R')
source('model.R')
# source('combine_files.R')

param_plot <- function(data,mapping,ymin,ymax,ybreaks=NULL,ylabels=NULL,indiv_agg = FALSE,to_agg = NULL){	
	if(indiv_agg){
		if(is.null(to_agg)){to_agg = which(names(data)=='categ')}
		agg_mean = aggregate(data[data$categ=='Indiv',1],by=list(interaction(data[data$categ=='Indiv',to_agg])),FUN='mean')
		for(i in 1:dim(agg_mean)[1]){
			data[which(interaction(data[,to_agg])==agg_mean[i,1]),1] = agg_mean[i,2]
		}		
	}
	plot = ggplot(data,mapping)+ 
	geom_line() +
	guides(color=guide_legend(order=1),linetype=guide_legend(order=3)) + 	
	scale_color_manual(values=c(divpal[1],divpal[5]))+scale_linetype_manual(values=c(3,1,5)) +
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(colour="",linetype="")
	if(is.null(ybreaks) && is.null(ylabels)){
		plot = plot+scale_y_continuous(limits=c(ymin,ymax))
	} else{ plot = plot + scale_y_continuous(limits=c(ymin,ymax),breaks=ybreaks,labels=ylabels)
		}
	if(indiv_agg){
		plot = plot + geom_point(data=data[which(data$categ=='Badge'),],mapping) } else{
			plot = plot + geom_point()
		}
	return(plot)
}

transparent_legend =  theme(
  legend.background = element_rect(fill = "transparent"),
  legend.key = element_rect(fill = "transparent", 
                            color = "transparent"))


mypal=brewer.pal(9,'Set1')
divpal = brewer.pal(5,'RdBu')
seqpal = rev(brewer.pal(5,'YlOrRd'))
seqpal2 = rev(brewer.pal(5,'YlGnBu'))

# load("/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/summary_stats_2016_08_17.Rdata")
source('/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/parameters.R')
# error = cbind(data.frame(categ=as.vector(error_cat_mean),indiv=as.vector(error_ind_mean)),parameters)
# time = cbind(data.frame(categ=log(as.vector(time_cat_mean),base=10),indiv=log(as.vector(time_ind_mean),base=10)),parameters)
source("/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/combine_paramsweep.R")


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
p_obs_vals = sort(unique(parameters$pobs))
xpobs = length(p_obs_vals)

n = which(N_vals==N)
c1 = which(perc_vals==c1)
w = which(wind_vals==w)
pcat = which(confus_cat_vals==pcat)
pind = which(confus_ind_vals==pind)
error_max = 0.42
time_min = min(time[,1:2])-0.1
time_max = max(time[,1:2])+0.1
time_labels = c(500,1000,2000,4000,8000,16000)
time_breaks = log(time_labels,base=10)


marg = c(0.1,0.1,0,0)
textsz = 10

################################
#learning curves
plots=list()

down_sample = 100

for(q in 1:((dim(parameters_toplot)[1])/xcorr)){
	N = parameters_toplot$N[q]
	perc = parameters_toplot$c1[q]
	wind = parameters_toplot$w[q]

	error_now=error_time[[q]][seq(1,Tfights_min*3,by=down_sample),]
	if(length(which(error$fights>Tfights_min))!=0){
			error_now=error_now[-which(error$fights>Tfights_min),]}
			
	plots[[q]] = ggplot()+
	geom_ribbon(data=error_now[error_now$categ=='Badge',],aes(ymin=error-sd,ymax=error+sd,x=fights,fill=sigcorr),alpha=0.3)+
	geom_ribbon(data=error_now[error_now$categ=='Indiv',],aes(ymin=error-sd,ymax=error+sd,x=fights,fill=divpal[5]),alpha=0.3)+
    geom_line(data=error_now[error_now$categ=='Badge',],aes(x=fights,y=error,linetype=sigcorr,color='Badge'))+
    geom_line(data=error_now[error_now$categ=='Indiv',],aes(x=fights,y=error,color='Indiv'))+
    guides(fill=FALSE,linetype=guide_legend(override.aes=list(fill=NA)))+ 
	scale_linetype_manual(values=c(5,1)) +	scale_color_manual(values=divpal[c(1,5)])+
	scale_fill_manual(values=divpal[c(5,1,1)])+ 
	scale_y_continuous(limits=c(0,error_max)) + scale_x_continuous(breaks=seq(0,Tfights_min,by=2500)) + 
	theme_bw()+
	theme(text=element_text(family="Helvetica", size=10),plot.title=element_text(size=10) , plot.margin=unit(c(0,0.25,0,0),"cm"))+
	labs(linetype="", colour="") + 
	xlab("Interactions")+ylab("Error")+
	ggtitle(substitute(paste('N = ',N,', w = ',wind,sep=""),list(N = N, wind = wind)))+
	guides(color = guide_legend(order=1),linetype = guide_legend(order=2))+ theme(legend.position='none',legend.justification=c(1,1),legend.box.just = "left"
,legend.box='horizontal')+transparent_legend
}

plots[[6]] = plots[[6]] + theme(legend.position=c(0.95,0.5))


pdf(file=paste(wd,"/learning_curves.pdf",sep=""),width=6.83,height=3)		
grid.arrange(plots[[6]],plots[[5]],plots[[2]],ncol=3,widths=c(1,1,1))
dev.off()

#############################
# effect of various parameters
plots.error = list()	
plots.time = list()
marg =c(3,1,1,0)
#group size

error_N = error[which(with(parameters,interaction(c1,w,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
error_N = with(error_N,data.frame(error=c(categ,indiv),N=rep(N,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_N)[1]))))
error_N = error_N[-intersect(which(error_N$categ=='Indiv'),which(error_N$c2==corr_vals[1])),]

plots.error[[1]] = param_plot(error_N, aes(x=N, y = error, colour = categ, linetype = c2),0,error_max) + 
	xlab("Group size") + ylab("Error") 	+ theme(legend.position=c(0.6,0.25),legend.box.just = "left"
,legend.box='horizontal',plot.margin = unit(x = marg, units = "mm"))+transparent_legend

# legend <- get_legend(plots.error[[1]])
# plots.error[[1]] = plots.error[[1]] + theme(legend.position='none')

time_N = time[which(with(parameters,interaction(c1,w,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
time_N = with(time_N,data.frame(time=c(categ,indiv),N=rep(N,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_N)[1]))))
time_N = time_N[-intersect(which(time_N$categ=='Indiv'),which(time_N$c2==corr_vals[1])),]

plots.time[[1]] = param_plot(time_N, aes(x=N, y = time, colour = categ, linetype = c2),time_min,time_max,ybreaks=time_breaks,ylabels=time_labels) + 
	xlab("Group size") + ylab("Time") + theme(legend.position='none',plot.margin = unit(x = marg, units = "mm"))

#memory window
error_w = error[which(with(parameters,interaction(N,c1,pcat,pind,pobs,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
error_w = with(error_w,data.frame(error=c(categ,indiv),w=rep(w,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_w)[1]))))
error_w = error_w[-intersect(which(error_w$categ=='Indiv'),which(error_w$c2==corr_vals[1])),]
if(is.element(Inf,error_w$w)){
# error_w = error_w[-which(error_w$w==Inf),]}
error_w$w[which(error_w$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}

plots.error[[2]] = param_plot(error_w, aes(x=w, y = error, colour = categ, linetype = c2),0,error_max) + 	
	xlab("Memory window")+ ylab("") + theme(legend.position='none',plot.margin = unit(x = marg, units = "mm"))+
	scale_x_continuous(breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf'))

time_w = time[which(with(parameters,interaction(N,c1,pcat,pind,pobs,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
time_w = with(time_w,data.frame(time=c(categ,indiv),w=rep(w,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_w)[1]))))
time_w = time_w[-intersect(which(time_w$categ=='Indiv'),which(time_w$c2==corr_vals[1])),]
if(is.element(Inf,time_w$w)){
# time_w = time_w[-which(time_w$w==Inf),]}
time_w$w[which(time_w$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}

plots.time[[2]] = param_plot(time_w, aes(x=w, y = time, colour = categ, linetype = c2),time_min,time_max,ybreaks=time_breaks,ylabels=time_labels) + 	
	xlab("Memory window")+ ylab("") + theme(legend.position='none',plot.margin = unit(x = marg, units = "mm"))+
	scale_x_continuous(breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf'))
	
	
#perception window
error_c1 = error[which(with(parameters,interaction(N,w,pcat,pind,pobs,sep=','))==paste(N_vals[n],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
error_c1 = with(error_c1,data.frame(error=c(categ,indiv),c1=rep(c1,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_c1)[1]))))
error_c1 = error_c1[-intersect(which(error_c1$categ=='Indiv'),which(error_c1$c2==corr_vals[1])),]
# ind_mean = mean(error_c1$error[which(error_c1$categ=='Indiv')])
# error_c1$error[which(error_c1$categ=='Indiv')] = ind_mean
wo_error = subset(error_c1,select=-error)
agg_mean = aggregate(error_c1$error,by=list(with(wo_error,interaction(c1,c2,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
error_c1 = cbind(data.frame(error=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(error_c1)[2:4]=colnames(wo_error)
error_c1$c1=as.numeric(levels(error_c1$c1))[error_c1$c1]

plots.error[[3]] = param_plot(error_c1, aes(x=c1, y = error, colour = categ, linetype = c2),0,error_max,indiv_agg=TRUE) + 	
	xlab("Category width")+ ylab("") + theme(legend.position='none',plot.margin = unit(x = marg, units = "mm"))

time_c1 = time[which(with(parameters,interaction(N,w,pcat,pind,pobs,sep=','))==paste(N_vals[n],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
time_c1 = with(time_c1,data.frame(time=c(categ,indiv),c1=rep(c1,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_c1)[1]))))
time_c1 = time_c1[-intersect(which(time_c1$categ=='Indiv'),which(time_c1$c2==corr_vals[1])),]
# ind_mean = mean(time_c1$time[which(time_c1$categ=='Indiv')])
# time_c1$time[which(time_c1$categ=='Indiv')] = ind_mean
wo_time = subset(time_c1,select=-time)
agg_mean = aggregate(time_c1$time,by=list(with(wo_time,interaction(c1,c2,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
time_c1 = cbind(data.frame(time=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(time_c1)[2:4]=colnames(wo_time)
time_c1$c1=as.numeric(levels(time_c1$c1))[time_c1$c1]

plots.time[[3]] = param_plot(time_c1, aes(x=c1, y = time, colour = categ, linetype = c2),time_min,time_max,ybreaks=time_breaks,ylabels=time_labels,indiv_agg=TRUE) + 	
	xlab("Category width")+ ylab("") + theme(legend.position='none',plot.margin = unit(x = marg, units = "mm"))
	
pdf(file=paste(wd,'/parameters.pdf',sep=''),width=6.8,height=4)		
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(4,3,heights=rep(1,4)/4,widths=c(0.45,0.45,0.1))))
# print(plots.error[[1]], vp=viewport(layout.pos.row=1,layout.pos.col=1))
# print(plots.error[[2]], vp=viewport(layout.pos.row=2,layout.pos.col=1))
# print(plots.error[[3]], vp=viewport(layout.pos.row=3,layout.pos.col=1))
# print(plots.error[[4]], vp=viewport(layout.pos.row=4,layout.pos.col=1))
# print(plots.time[[1]], vp=viewport(layout.pos.row=1,layout.pos.col=2))
# print(plots.time[[2]], vp=viewport(layout.pos.row=2,layout.pos.col=2))
# print(plots.time[[3]], vp=viewport(layout.pos.row=3,layout.pos.col=2))
# print(plots.time[[4]], vp=viewport(layout.pos.row=4,layout.pos.col=2))
# legend$vp=viewport(layout.pos.row=1,layout.pos.col=3)
# grid.draw(legend)
grid.arrange(plots.error[[1]],plots.error[[2]],plots.error[[3]],plots.time[[1]],plots.time[[2]],plots.time[[3]],ncol=3,widths=rep(1,3)/4)
dev.off()

################################
#probability of switching perception 
source('confus_cat_to_confus_prob.R')

error_p = error[which(with(parameters,interaction(N,w,c1,pobs,sep=','))==paste(N_vals[n],wind_vals[w],perc_vals[c1],0,sep=',')),]
recode_vec = c()
for(i in 1:length(confus_cat_vals)){
	recode_vec = paste(recode_vec,'confus_cat_vals[',i,']=confus_prob_vals[',i,'];',sep="")
}
recode_vec = substr(recode_vec,start=1,stop=nchar(recode_vec)-1)
error_p$pcat =recode(error_p$pcat,recode_vec)
error_p = with(error_p,data.frame(error=c(categ,indiv),p=c(pcat,pind),c2=as.factor(rep(c2,times=2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_p)[1]))))
error_p = error_p[-intersect(which(error_p$categ=='Indiv'),which(error_p$c2==corr_vals[1])),]
wo_error = subset(error_p,select=-error)
agg_mean = aggregate(error_p$error,by=list(with(wo_error,interaction(p,c2,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
error_p = cbind(data.frame(error=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(error_p)[2:4]=colnames(wo_error)
error_p$p=as.numeric(levels(error_p$p))[error_p$p]

plots.error[[4]] = param_plot(error_p, aes(x=p, y = error, colour = categ, linetype = c2),0.2,0.4) + 	
	xlab("Probability of recategorizing")+ylab("Error") + theme(legend.position=c(0.2,0.95),legend.box.just = "left"
,legend.box='horizontal',plot.margin = unit(x = marg, units = "mm"),legend.key.size=unit(.14,'inch'))+transparent_legend
	# legend.box='horizontal',legend.key.size=unit(.14,'inch'),legend.key.width=unit(.2,'inch'),plot.margin = unit(x = marg, units = "inch")

time_p = time[which(with(parameters,interaction(N,w,c1,pobs,sep=','))==paste(N_vals[n],wind_vals[w],perc_vals[c1],0,sep=',')),]
time_p$pcat =recode(time_p$pcat,"confus_cat_vals[1]=confus_prob_vals[1];confus_cat_vals[2]=confus_prob_vals[2];confus_cat_vals[3]=confus_prob_vals[3];confus_cat_vals[4]=confus_prob_vals[4]")
time_p = with(time_p,data.frame(time=c(categ,indiv),p=c(pcat,pind),c2=as.factor(rep(c2,times=2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_p)[1]))))
time_p = time_p[-intersect(which(time_p$categ=='Indiv'),which(time_p$c2==corr_vals[1])),]
wo_time = subset(time_p,select=-time)
agg_mean = aggregate(time_p$time,by=list(with(wo_time,interaction(p,c2,categ,sep=','))),FUN=mean)
new_facs = agg_mean[,1]
time_p = cbind(data.frame(time=agg_mean[,2]),matrix(unlist(strsplit(toString(unique(new_facs)),',')),ncol=3,byrow=TRUE))
colnames(time_p)[2:4]=colnames(wo_time)
time_p$p=as.numeric(levels(time_p$p))[time_p$p]

time_min_p = min(time_p$time)-0.1
time_max_p = max(time_p$time)+0.05
time_breaks_p = log(2000*(2^seq(0,2)),10)
time_labels_p = 10^time_breaks_p

plots.time[[4]] = param_plot(time_p, aes(x=p, y = time, colour = categ, linetype = c2),time_min_p,time_max_p,ybreaks=time_breaks_p,ylabels=time_labels_p) + 	
	xlab("Probability of recategorizing")+ylab("Time") + theme(legend.position='none',plot.margin = unit(x = marg, units = "mm"))	
	

pdf(file=paste(wd,'/confusion_probs.pdf',sep=''),width=6.8,height=2)
grid.arrange(plots.error[[4]],plots.time[[4]],ncol=2,widths=c(1/2,1/2))
dev.off()
 
# ############ observational learning
marg = c(.12,.07,0,0)
heat_marg = c(0.12,.04,0,0)
width = 6.8	
height = 3.5
bar_height = height/2-0.5
bump_up = 0.23
mem_obs = list()

error_pobs = error[which(with(parameters,interaction(N,w,c1,pcat,pind,sep=','))==paste(N_vals[n],wind_vals[w-2],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]

error_pobs = with(error_pobs,data.frame(error=c(categ,indiv),pobs=rep(pobs,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(error_pobs)[1]))))
error_pobs = error_pobs[-intersect(which(error_pobs$categ=='Indiv'),which(error_pobs$c2==corr_vals[1])),]

plots.error[[5]] = param_plot(error_pobs, aes(x=pobs, y = error, colour = categ, linetype = c2),0,0.45) + 	
	xlab("Prob of observing")+ ylab("Error") + theme(legend.position=c(0.55,0.96),legend.box.just = "left"
,legend.box='horizontal',legend.key.size=unit(.14,'inch'),legend.key.width=unit(.2,'inch'),plot.margin = unit(x = marg, units = "inch"))+transparent_legend

time_pobs = time[which(with(parameters,interaction(N,w,c1,pcat,pind,sep=','))==paste(N_vals[n],wind_vals[w-2],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]

time_pobs = with(time_pobs,data.frame(time=c(categ,indiv),pobs=rep(pobs,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(time_pobs)[1]))))
time_pobs = time_pobs[-intersect(which(time_pobs$categ=='Indiv'),which(time_pobs$c2==corr_vals[1])),]

plots.time[[5]] = param_plot(time_pobs, aes(x=pobs, y = time, colour = categ, linetype = c2),log(500,base=10),log(8000,base=10),ybreaks=time_breaks,ylabels=time_labels) + 	
	xlab("Prob of observing")+ ylab("Time") +theme(legend.position='none')#+ theme(legend.position=c(0.53,0.94),legend.box.just = "left"
#,legend.box='horizontal',legend.key.size=unit(.15,'inch'),legend.key.width=unit(.2,'inch'),plot.margin = unit(x = marg, units = "inch"))+transparent_legend

error_pw = error[which(with(parameters,interaction(N,c1,pcat,pind,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]
if(is.element(Inf,wind_vals)){
# error_pw = error_pw[-which(error_pw$w==Inf),]}
error_pw$w[which(error_pw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
error_pw$diff = error_pw$categ-error_pw$indiv

time_pw = time[which(with(parameters,interaction(N,c1,pcat,pind,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]
if(is.element(Inf,wind_vals)){
# time_pw = time_pw[-which(time_pw$w==Inf),]}
time_pw$w[which(time_pw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
time_pw$diff = time_pw$categ-error_pw$indiv

error_pw_subset = error_pw[which(error_pw$c2==corr_vals[c2]),]
time_pw_subset = time_pw[which(error_pw$c2==corr_vals[c2]),]

M = max(c(max(error_pw_subset$categ),max(error_pw_subset$indiv)))
m = min(c(min(error_pw_subset$categ),min(error_pw_subset$indiv)))
# m = 0
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
legend_breaks = round(seq(m,M-.01,length.out=4),1)
contour_breaks = sort(c(legend_breaks,legend_breaks+diff(legend_breaks)[1]/2))
legend_labels = legend_breaks
pos = (legend_breaks-m)/(M-m)
want = intersect(which(pos>=0),which(pos<=1))
pos = pos[want]
legend_breaks = legend_breaks[want]
legend_labels = legend_labels[want]
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),breaks = legend_breaks, labels = paste(as.character(legend_labels),'   ' ))+labs(fill='Error')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"inch"),legend.title=element_text(size=textsz),legend.box.just = "left")+guides(fill=guide_colorbar(title.vjust=-.2,title.hjust=-.1))

grobnow = ggplotGrob(legend_plot)
contour_leg_error = gtable_filter(grobnow,'guide-box')

# # Raster height
contour_leg_error[[1]][[1]][[1]][[1]][[1]][[2]]$height = unit(bar_height, "inch")

# Positions for labels and tick marks - five breaks, therefore, five positions
pos = unit(pos,'npc')

# Positions the labels 
contour_leg_error[[1]][[1]][[1]][[1]][[1]][[3]]$y = pos

# Positions the tick marks
contour_leg_error[[1]][[1]][[1]][[1]][[1]][[5]]$y0 = pos
contour_leg_error[[1]][[1]][[1]][[1]][[1]][[5]]$y1 = pos

# Legend key height ?
contour_leg_error[[1]][[1]][[1]][[1]]$heights = unit.c(rep(unit(0, "inch"), 3),
                                         unit(bar_height, "inch"),
                                         unit(bump_up, "inch"))
# Legend height
contour_leg_error[[1]][[1]]$heights[[2]] = sum(rep(unit(0,'inch'), 3),
                                 unit(1, "npc"),
                                 unit(0, "inch"))

mem_obs[[1]] = ggplot(error_pw_subset,aes(x=pobs,y=w,z=categ))+
	geom_tile(aes(fill = categ)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(heat_marg,"inch"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Prob of observing') + ylab('Memory window')	
	

mem_obs[[2]] = ggplot(error_pw_subset,aes(x=pobs,y=w,z=indiv))+
	geom_tile(aes(fill = indiv)) + 
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(heat_marg,"inch"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Prob of observing') + ylab('Memory window')	
	
# M = max(c(max(time_pw_subset$categ),max(time_pw_subset$indiv)))
# m = min(c(min(time_pw_subset$categ),min(time_pw_subset$indiv)))
M = log(8000,base=10)+0.01
m = log(500,base=10)
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
legend_breaks = time_breaks
legend_labels = 10^legend_breaks
pos = (legend_breaks-m)/(M-m)
want = intersect(which(pos>=0),which(pos<=1))
pos = pos[want]
legend_breaks = legend_breaks[want]
legend_labels = legend_labels[want]
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),breaks = legend_breaks, labels = legend_labels)+labs(fill='Time')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"inch"),legend.title=element_text(size=textsz),legend.box.just = "left")+guides(fill=guide_colorbar(title.vjust=-.2,title.hjust=-.1))

grobnow = ggplotGrob(legend_plot)
contour_leg_time = gtable_filter(grobnow,'guide-box')

# # Raster height
contour_leg_time[[1]][[1]][[1]][[1]][[1]][[2]]$height = unit(bar_height, "inch")

# Positions for labels and tick marks - five breaks, therefore, five positions
pos = unit(pos,'npc')

# Positions the labels 
contour_leg_time[[1]][[1]][[1]][[1]][[1]][[3]]$y = pos

# Positions the tick marks
contour_leg_time[[1]][[1]][[1]][[1]][[1]][[5]]$y0 = pos
contour_leg_time[[1]][[1]][[1]][[1]][[1]][[5]]$y1 = pos

# Legend key height ?
contour_leg_time[[1]][[1]][[1]][[1]]$heights = unit.c(rep(unit(0, "inch"), 3),
                                         unit(bar_height, "inch"),
                                         unit(bump_up, "inch"))
# Legend height
contour_leg_time[[1]][[1]]$heights[[2]] = sum(rep(unit(0,'inch'), 3),
                                 unit(1, "npc"),
                                 unit(0, "inch"))

mem_obs[[3]] = ggplot(time_pw_subset,aes(x=pobs,y=w,z=categ))+	
	geom_tile(aes(fill = categ)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(heat_marg,"inch"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Prob of observing') + ylab('Memory window')	
	
mem_obs[[4]] = ggplot(time_pw_subset,aes(x=pobs,y=w,z=indiv))+	
	geom_tile(aes(fill = indiv)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=rev(seqpal2),limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(heat_marg,"inch"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Prob of observing') + ylab('Memory window')		

width = 6.8	
height = 3.5
pdf(file=paste(wd,"/observational_learning.pdf",sep=''),width=width,height=height)		
# grid.arrange(plots.error[[5]],mem_obs[[1]],mem_obs[[2]],contour_leg_error,plots.time[[5]],mem_obs[[3]],mem_obs[[4]],contour_leg_time,ncol=4,widths=c(1,1,1,0.5))
widths=c(1,1,1,0.4)
widths = widths / sum(widths)*width
heights = c(1/2,1/2)*height
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,4,heights=heights,widths=widths,default.units='inches')))
print(plots.error[[5]], vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(plots.time[[5]], vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(mem_obs[[1]], vp = viewport(layout.pos.row=1,layout.pos.col=2))
print(mem_obs[[2]], vp = viewport(layout.pos.row=1,layout.pos.col=3))
print(mem_obs[[3]], vp = viewport(layout.pos.row=2,layout.pos.col=2))
print(mem_obs[[4]], vp = viewport(layout.pos.row=2,layout.pos.col=3))
contour_leg_error$vp = viewport(layout.pos.row=1,layout.pos.col=4)
grid.draw(contour_leg_error)
contour_leg_time$vp = viewport(layout.pos.row=2,layout.pos.col=4)
grid.draw(contour_leg_time)
dev.off()


# ###########costs 
w_error = 2
w_time = 10^(-4)
w_perc = 1
w_wind = 1
cost_min = 1.5
cost_max = 3.8
c2_cost = 2

a_perc_vals = c(1/5,5)
a_wind_vals = a_perc_vals
a = 1
# total_cost = function(error,time,c1_eff,w,ap=a_perc,aw=a_wind,po=0){	
	# w_error*error+w_time*(1+po)*time+w_perc*(2/(1+exp(-ap*(2-c1_eff)))-1)+w_wind*(2/(1+exp(-aw*w))-1)
# }
total_cost = function(error,time,c1_eff,w,ap=a_perc,aw=a_wind,po=0){	
	max_w = 3000
	w[w==Inf]=max_w
	w_error*error+w_time*(1+po)*time+w_perc*((2-c1_eff)/2)^ap+w_wind*(w/max_w)^aw
}

plots.cost = list()

options(scipen=999) #needed this when the a parameter was done differently and i didn't want long decimals in the legend

a_perc = a_perc_vals[a]
a_wind = a_wind_vals[a]
cost_N = error[which(with(parameters,interaction(c1,w,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
cost_N = with(cost_N,data.frame(error=c(categ,indiv),N=rep(N,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(cost_N)[1])),c1_eff=rep(c1,2),w=rep(w,2)))
cost_N$c1_eff[which(cost_N$categ=='Indiv')]=0
cost_N = cbind(10^(as.vector(unlist(time[which(with(parameters,interaction(c1,w,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),1:2]))),cost_N)
names(cost_N)[1] = 'time'
cost_N = cost_N[which(cost_N$c2==corr_vals[c2_cost]),]
a_wind_vec = rep(a_wind_vals,each=dim(cost_N)[1])
hold = NULL
for(i in 1:(length(a_wind_vals))){
	hold = rbind(hold,cost_N)}
cost_N = hold
cost_N = with(cost_N,data.frame(N=N,c2=c2,categ=categ,cost=total_cost(error,time,c1_eff,w,a_perc,a_wind)))
cost_N$a_wind = as.factor(a_wind_vec)

plots.cost[[1]] = param_plot(cost_N, aes(x=N, y = cost, colour = categ, linetype=a_wind),cost_min,cost_max) + 
	xlab("Group size") + ylab("Cost") + theme(legend.position=c(0.49,0.25),legend.box.just = "left"
,legend.box='horizontal',legend.key.size=unit(.15,'inch'),legend.key.width=unit(.23,'inch'))+transparent_legend


cost_w = error[which(with(parameters,interaction(c1,N,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],N_vals[n],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
cost_w = with(cost_w,data.frame(error=c(categ,indiv),w=rep(w,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(cost_w)[1])),c1_eff=rep(c1,2)))
cost_w$c1_eff[which(cost_w$categ=='Indiv')]=0
cost_w = cbind(10^(as.vector(unlist(time[intersect(which(error$c1==perc_vals[c1]),which(with(parameters,interaction(c1,N,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],N_vals[n],confus_cat_vals[1],confus_ind_vals[1],0,sep=','))),1:2]))),cost_w)
names(cost_w)[1] = 'time'
cost_w = cost_w[which(cost_w$c2==corr_vals[c2_cost]),]
a_wind_vec = rep(a_wind_vals,each=dim(cost_w)[1])
hold = NULL
for(i in 1:(length(a_wind_vals))){
	hold = rbind(hold,cost_w)}
cost_w = hold
cost_w$a_wind = a_wind_vec
cost_w =with(cost_w,data.frame(w=w,categ=categ,a_wind=as.factor(a_wind),cost=total_cost(error,time,c1_eff,w,a_perc,a_wind)))
# transformed = round(2/(1+exp(-a_wind_vec*(2)))-1,3)
# cost_w$a_wind = factor(transformed,levels=sort(unique(transformed),decreasing=TRUE))
cost_w$a_wind = as.factor(cost_w$a_wind)
if(is.element(Inf,wind_vals)){
# cost_w = cost_w[-which(cost_w$w==Inf),]}
cost_w$w[which(cost_w$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}

plots.cost[[2]] = param_plot(cost_w, aes(x=w, y = cost, colour = categ, linetype = a_wind),cost_min,cost_max) + guides(colour=FALSE) + 
	xlab("Memory window")+ ylab("Cost") + theme(legend.position='none')+
	scale_x_continuous(breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf'))
	
options(scipen=0)

	
cost_c1 = error[which(with(parameters,interaction(N,w,pcat,pind,pobs,sep=','))==paste(N_vals[n],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
cost_c1 = with(cost_c1,data.frame(error=c(categ,indiv),w=rep(w,2),c1=rep(c1,2),c2=as.factor(rep(c2,2)),categ=factor(rep(c('Badge','Indiv'),each=dim(cost_c1)[1])),c1_eff=rep(c1,2)))
cost_c1$c1_eff[which(cost_c1$categ=='Indiv')]=0
cost_c1 = cbind(10^(as.vector(unlist(time[which(with(parameters,interaction(N,w,pcat,pind,pobs,sep=','))==paste(N_vals[n],wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),1:2]
))),cost_c1)
names(cost_c1)[1] = 'time'
cost_c1 = cost_c1[which(cost_c1$c2==corr_vals[c2_cost]),]
ap_vec = rep(a_perc_vals,each=dim(cost_c1)[1])
hold = NULL
for(i in 1:length(a_perc_vals)){
	hold = rbind(hold,cost_c1)
}
cost_c1 = hold
# cost_c1 = rbind(cost_c1,cost_c1)
cost_c1$a_perc = ap_vec
cost_c1=with(cost_c1,data.frame(cost=total_cost(error,time,c1_eff,w,a_perc,a_wind),c1=c1,categ=categ,a_perc=a_perc))
# transformed = round(2*ap_vec*exp(2*ap_vec)/(1+exp(2*ap_vec))^2,2)
# transformed = round(2/(1+exp(-ap_vec*(2)))-1,3)
# transformed = ap_vec
# cost_c1$a_perc = factor(transformed,levels=sort(unique(transformed),decreasing=TRUE))
cost_c1$a_perc = as.factor(cost_c1$a_perc)

plots.cost[[3]] = param_plot(cost_c1, aes(x=c1, y = cost, colour = categ, linetype = a_perc),cost_min,cost_max,indiv_agg=TRUE,to_agg=c(which(names(cost_c1)=='categ'),which(names(cost_c1)=='a_perc'))) + 
	xlab("Category width")+ ylab("Cost") + theme(legend.position='none') # need to aggregate by both categ and a_perc in case a_perc_val affects the cost of individual learners using c1=0
	
# leg_c1 = get_legend(plots.cost[[3]])
# plots.cost[[3]] = plots.cost[[3]]+ theme(legend.position='none ')

cost_pobs = error[which(with(parameters,interaction(N,w,c1,pcat,pind,sep=','))==paste(N_vals[n],wind_vals[w-2],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]
cost_pobs = with(cost_pobs,data.frame(error=c(categ,indiv),w=rep(w,2),c1=rep(c1,2),c2=as.factor(rep(c2,2)),pobs=rep(pobs,2),categ=factor(rep(c('Badge','Indiv'),each=dim(cost_pobs)[1])),c1_eff=rep(c1,2)))
cost_pobs$c1_eff[which(cost_pobs$categ=='Indiv')]=0
cost_pobs = cbind(10^(as.vector(unlist(time[which(with(parameters,interaction(N,w,c1,pcat,pind,sep=','))==paste(N_vals[n],wind_vals[w-2],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),1:2]))),cost_pobs)
names(cost_pobs)[1]='time'
cost_pobs = cost_pobs[which(cost_pobs$c2==corr_vals[c2_cost]),]
cost_pobs=with(cost_pobs,data.frame(cost=total_cost(error,time,c1_eff,w,a_perc,a_wind,po=pobs),pobs=pobs,categ=categ,a_perc=a_perc))

plots.cost[[4]] = param_plot(cost_pobs, aes(x=pobs, y = cost, colour = categ),cost_min,cost_max) + 
	xlab("Prob of observing")+ ylab("Cost") + theme(legend.position='none')

pdf(file=paste(wd,"/costs.pdf",sep=''),width=6.8,height=3)	
widths = c(1,1)
widths = widths/sum(widths)
grid.arrange(plots.cost[[1]],plots.cost[[2]],plots.cost[[3]],plots.cost[[4]],ncol=2,widths=widths)
dev.off()

# ###########cost comparison
width = 6.8	
height = 3.5
bar_height = height/2-0.4
bump_up = 0.25
marg = c(0.18,.09,0,0)
a_perc = a_perc_vals[a]
a_wind = a_wind_vals[a]

cost_Nw = error[which(with(parameters,interaction(c1,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
cost_Nw$c1_eff = cost_Nw$c1
cost_Nw = cbind(10^(time[which(with(parameters,interaction(c1,pcat,pind,pobs,sep=','))==paste(perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),1:2]),cost_Nw)
names(cost_Nw)[1:2] = c('categ_time','indiv_time')
names(cost_Nw)[3:4] = c('categ_error','indiv_error')
cost_Nw = with(cost_Nw,data.frame(N=N,w=w,c2=c2,categ=total_cost(categ_error,categ_time,c1_eff,w,a_perc,a_wind),indiv=total_cost(indiv_error,indiv_time,0,w,a_perc,a_wind))) #c1 is 0 for individual learners
if(is.element(Inf,wind_vals)){
cost_Nw$w[which(cost_Nw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
cost_Nw$diff = cost_Nw$categ-cost_Nw$indiv

cost_cw = error[which(with(parameters,interaction(N,pcat,pind,pobs,sep=','))==paste(N_vals[n],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
cost_cw$c1_eff = cost_cw$c1
cost_cw = cbind(10^(time[which(with(parameters,interaction(N,pcat,pind,pobs,sep=','))==paste(N_vals[n],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),1:2]),cost_cw)
names(cost_cw)[1:2] = c('categ_time','indiv_time')
names(cost_cw)[3:4] = c('categ_error','indiv_error')
cost_cw = with(cost_cw,data.frame(c1=c1,w=w,c2=c2,categ=total_cost(categ_error,categ_time,c1_eff,w,a_perc,a_wind),indiv=total_cost(indiv_error,indiv_time,0,w,a_perc,a_wind))) #c1 is 0 for individual learners
if(is.element(Inf,wind_vals)){
cost_cw$w[which(cost_cw$w==Inf)]=wind_vals[xwind-1]+diff(wind_vals)[1]}
cost_cw$diff = cost_cw$categ-cost_cw$indiv

cost_Nc = error[which(with(parameters,interaction(w,pcat,pind,pobs,sep=','))==paste(wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),]
cost_Nc$c1_eff = cost_Nc$c1
cost_Nc = cbind(10^(time[which(with(parameters,interaction(w,pcat,pind,pobs,sep=','))==paste(wind_vals[w],confus_cat_vals[1],confus_ind_vals[1],0,sep=',')),1:2]),cost_Nc)
names(cost_Nc)[1:2]=c('categ_time','indiv_time')
names(cost_Nc)[3:4]=c('categ_error','indiv_error')
cost_Nc = with(cost_Nc,data.frame(N=N,c1=c1,c2=c2,categ=total_cost(categ_error,categ_time,c1_eff,w,a_perc,a_wind),indiv=total_cost(indiv_error,indiv_time,0,w,a_perc,a_wind)))#c1 is 0 for individual learners
cost_Nc$diff=cost_Nc$categ-cost_Nc$indiv

cost_pw = error[which(with(parameters,interaction(N,c1,pcat,pind,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),]
cost_pw$c1_eff = cost_pw$c1
cost_pw = cbind(10^(time[which(with(parameters,interaction(N,c1,pcat,pind,sep=','))==paste(N_vals[n],perc_vals[c1],confus_cat_vals[1],confus_ind_vals[1],sep=',')),1:2]),cost_pw)
names(cost_pw)[1:2]=c('categ_time','indiv_time')
names(cost_pw)[3:4]=c('categ_error','indiv_error')
cost_pw = with(cost_pw,data.frame(pobs=pobs,w=w,c2=c2,categ=total_cost(categ_error,categ_time,c1_eff,w,a_perc,a_wind,pobs),indiv=total_cost(indiv_error,indiv_time,0,w,a_perc,a_wind,pobs))) #c1 is 0 for individual learners
cost_pw$diff=cost_pw$categ-cost_pw$indiv

M = max(c(max(cost_Nw$diff),max(cost_cw$diff),max(cost_Nc$diff),max(cost_pw$diff)))
m = min(c(min(cost_Nw$diff),min(cost_cw$diff),min(cost_Nc$diff),min(cost_pw$diff)))
M = max(M,abs(m))
m = -M
cut_breaks = seq(m,M,length.out=11)
d  = diff(cut_breaks)[1]
legend_data = data.frame(z = cut(seq(m+d/2,M-d/2,length.out = 22),breaks=cut_breaks), x = 1:22,y = 1:22)
legend_data = data.frame(z = seq(m,M,length.out = 22), x = 1:22,y = 1:22)
shift = 0.1
# legend_breaks = round(seq(m+shift,M-shift,length.out=3),2)
# contour_breaks = round(seq(m+shift,M-shift,length.out=3),2)
legend_breaks = c(-1,0,1)
legend_labels = legend_breaks
legend_labels[which(legend_labels==0)]='0 Equivalent'
# legend_labels[1] = 'Badge cost less by 1'
# legend_labels[2] = 'Costs equal'
# legend_labels[3] = 'Badge cost greater by 1'
pos = (legend_breaks-m)/(M-m)
want = intersect(which(pos>=0),which(pos<=1))
pos = c(0,pos[want],1)
legend_breaks = c(m,legend_breaks[want],M)
legend_labels = c('Badge better',legend_labels[want],'Indiv better')
legend_plot = ggplot(legend_data,aes(x=x,y=y,z=z)) + geom_tile(aes(fill=z)) + scale_fill_gradientn(colours=divpal,limits=c(m,M),breaks = legend_breaks, labels = legend_labels)+labs(fill='')+theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(),legend.text=element_text(size=textsz))+guides(fill= guide_colorbar(barheight=8,barwidth=0.5))

grobnow = ggplotGrob(legend_plot)
contour_leg_cost = gtable_filter(grobnow,'guide-box')

# Raster height
contour_leg_cost[[1]][[1]][[1]][[1]][[1]][[2]]$height = unit(bar_height, "inch")

# Positions for labels and tick marks - three breaks, therefore, three positions
# pos = unit.c(unit(0.01,"npc"),  unit(.5, "npc"), unit(.99, "npc"))
pos = unit(pos,'npc')

# Positions the labels 
contour_leg_cost[[1]][[1]][[1]][[1]][[1]][[3]]$y = pos

# Positions the tick marks
contour_leg_cost[[1]][[1]][[1]][[1]][[1]][[5]]$y0 = pos
contour_leg_cost[[1]][[1]][[1]][[1]][[1]][[5]]$y1 = pos

# Legend key height ?
contour_leg_cost[[1]][[1]][[1]][[1]]$heights = unit.c(rep(unit(0, "inch"), 3.5),
                                         unit(bar_height, "inch"),
                                         unit(bump_up, "inch"))
# Legend height
contour_leg_cost[[1]][[1]]$heights[[2]] = sum(rep(unit(0,'inch'), 3.5),
                                 unit(1, "npc"),
                                 unit(0, "inch"))
                                 
# # #http://stackoverflow.com/questions/19214914/how-can-i-make-the-legend-in-ggplot2-the-same-height-as-my-plot

plots.cost.Nw = list()
plots.cost.cw = list()
plots.cost.Nc = list()
plots.cost.pw = list()

for(c2 in 1:xcorr){

cost_Nw_subset = cost_Nw[which(cost_Nw$c2==corr_vals[c2]),]
cost_cw_subset = cost_cw[which(cost_cw$c2==corr_vals[c2]),]
cost_Nc_subset = cost_Nc[which(cost_Nc$c2==corr_vals[c2]),]
cost_pw_subset = cost_pw[which(cost_pw$c2==corr_vals[c2]),]

plots.cost.Nw[[c2]] = ggplot(cost_Nw_subset,aes(x=N,y=w,z=diff))+	
	geom_tile(aes(fill = diff)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=divpal,limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Group size') + ylab('Memory window')
	
plots.cost.cw[[c2]] = ggplot(cost_cw_subset,aes(x=c1,y=w,z=diff))+	
	geom_tile(aes(fill = diff)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=divpal,limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0),breaks=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],wind_vals[xwind-1]+diff(wind_vals)[1]),labels=c(wind_vals[c(1,seq(2,(xwind-1),by=2))],'Inf')) + scale_x_continuous(expand = c(0,0)) +
	xlab('Category width') + ylab('Memory window')
	
plots.cost.Nc[[c2]] = ggplot(cost_Nc_subset,aes(x=N,y=c1,z=diff))+	
	geom_tile(aes(fill = diff)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=divpal,limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
	xlab('Group size') + ylab('Category width')

plots.cost.pw[[c2]] = ggplot(cost_pw_subset,aes(x=pobs,y=w,z=diff))+geom_tile(aes(fill = diff)) +  
	# stat_contour(breaks=contour_breaks)+
	scale_fill_gradientn(colours=divpal,limits=c(m,M),guide="colorbar")+
	theme_bw() +
	theme(text=element_text(family="Helvetica", size=textsz), plot.title=element_text(size=textsz), plot.margin=unit(marg,"cm"), legend.key =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position='none')+
	scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
	ylab('Memory window') + xlab('Prob of obs')
}
	
pdf(file=paste(wd,"/cost_comparisons.pdf",sep=''),width=width,height=height)	
widths = c(1,1,1,1,0.7)
widths = widths/sum(widths)
grid.arrange(plots.cost.Nw[[1]],plots.cost.Nc[[1]],plots.cost.cw[[1]],plots.cost.pw[[1]],contour_leg_cost,plots.cost.Nw[[2]],plots.cost.Nc[[2]],plots.cost.cw[[2]],plots.cost.pw[[2]],ncol=5,widths=widths)
dev.off()

graphics.off()