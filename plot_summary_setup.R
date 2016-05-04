library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(parallel)
library(foreach)
library(doParallel)
library(matrixStats)
source('/lustre/ebrush/badgevsrecog/multiplot.R')
source('/lustre/ebrush/badgevsrecog/ind2sub.R')
source('/lustre/ebrush/badgevsrecog/sub2ind.R')
source('/lustre/ebrush/badgevsrecog/get_legend.R')

direc = "/homes/ebrush/priv/badgevsrecog/"
files = list.files(path=direc)
paramsweep = intersect(grep('paramsweep',files,value=FALSE),grep('Rdata',files,value=FALSE))
totals = matrix(NA,length(paramsweep))
for(i in 1:length(paramsweep)){
	file = files[paramsweep[i]]
	file = substr(file,1,nchar(file)-6)
	day = strtoi(substr(file,nchar(file)-1,nchar(file)))
	month = strtoi(substr(file,nchar(file)-4,nchar(file)-3))
	year = strtoi(substr(file,nchar(file)-7,nchar(file)-6))
	total = day + month + year
	totals[i] = total
}
file = paste(direc,files[paramsweep[which.max(totals)]],sep="") #find most recent paramsweep file

load(file)

# load('/homes/ebrush/priv/badgevsrecog/badgevsrecog_paramsweep_par_2016_04_28.Rdata')

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

c2vals = c(2,3)
xcorr2 = length(c2vals)

sim_runs = length(error_cat[[1,1,1,1,1,1]])
Tfights = dim(error_cat[[1,1,1,1,1,1]][[1]])
Tfights = Tfights[2]
Tfights_min = 5000

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

time_cat_mean<- foreach(p=1:P,.combine='c') %do% {
		time_cat_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		time_cat[[n,c1,w,pcat,pind,c2]][[k]] }
		mean(time_cat_tmp)
	} 	
	
time_ind_mean<- foreach(p=1:P,.combine='c') %do% {
		time_ind_tmp <- foreach(k=1:sim_runs,.combine='c') %do% {
		v = ind2sub(d,p)
		n = v[1]
		c1 = v[2]
		w = v[3]
		pcat = v[4]
		pind = v[5]
		c2 = v[6]
		time_ind[[n,c1,w,pcat,pind,c2]][[k]] }
		mean(time_ind_tmp)
	} 	