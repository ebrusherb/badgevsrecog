direc = '/Users/eleanorbrush/Dropbox/evo_badgesVSrecognition/code'

files = list.files(path=direc)

paramsweep = intersect(grep('summary_stats',files,value=FALSE),grep('Rdata',files,value=FALSE))

error = c()
time = c()
parameters = c()
hold_env = new.env()

for(i in 1:12){
	to_load = paste(direc,'/',grep(paste('_',i,'.Rdata',sep=''),files[paramsweep],value=TRUE),sep='')
	load(to_load,hold_env)
	error = rbind(error,data.frame(categ=as.vector(hold_env$error_cat_mean),indiv=as.vector(hold_env$error_ind_mean),hold_env$parameters))
	time = rbind(time,data.frame(categ=log(as.vector(hold_env$time_cat_mean),base=10),indiv=log(as.vector(hold_env$time_ind_mean),base=10),hold_env$parameters))
	parameters=rbind(parameters,hold_env$parameters)
}

to_load = paste(direc,'/',grep('learning_curves.Rdata',files[paramsweep],value=TRUE),sep='')
load(to_load,hold_env)
parameters_toplot = hold_env$parameters
error_time = hold_env$error_time
Tfights = hold_env$Tfights
Tfights_min = hold_env$Tfights_min
