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

# direc = "/homes/ebrush/priv/badgevsrecog/"
# files = list.files(path=direc)
# paramsweep = intersect(grep('paramsweep',files,value=FALSE),grep('Rdata',files,value=FALSE))
# totals = matrix(NA,length(paramsweep))
# for(i in 1:length(paramsweep)){
	# file = files[paramsweep[i]]
	# file = substr(file,1,nchar(file)-6)
	# day = strtoi(substr(file,nchar(file)-1,nchar(file)))
	# month = strtoi(substr(file,nchar(file)-4,nchar(file)-3))
	# year = strtoi(substr(file,nchar(file)-7,nchar(file)-6))
	# total = day + 31*month +365*year
	# totals[i] = total
# }
# file = paste(direc,files[paramsweep[which.max(totals)]],sep="") #find most recent paramsweep file

# load(file)

# load('/homes/ebrush/priv/badgevsrecog/badgevsrecog_paramsweep_par_2016_04_28.Rdata')

direc = "/homes/ebrush/priv/badgevsrecog/"
files = list.files(path=direc)
paramsweep = intersect(grep('summary_stats',files,value=FALSE),grep('Rdata',files,value=FALSE))
totals = matrix(NA,length(paramsweep))
for(i in 1:length(paramsweep)){
	file = files[paramsweep[i]]
	file = substr(file,1,nchar(file)-6)
	day = strtoi(substr(file,nchar(file)-1,nchar(file)))
	month = strtoi(substr(file,nchar(file)-4,nchar(file)-3))
	year = strtoi(substr(file,nchar(file)-7,nchar(file)-6))
	total = day + 31*month +365*year
	totals[i] = total
}
file = paste(direc,files[paramsweep[which.max(totals)]],sep="") #find most recent paramsweep file

load(file)


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

wvals = c(1,3)
xw = length(wvals)

Tfights = Tfights+1
Tfights_min = 5000

