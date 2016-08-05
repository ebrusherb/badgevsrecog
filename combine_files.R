combine_files = function(...){
	input_list = list(...)
	variables = c()
	error = c()
	time = c()
	for(i in 1:length(input_list)){
		load(input_list[[i]])
		xN = length(N_vals)
		xperc = length(perc_vals)
		xwind = length(wind_vals)
		xconfus_cat = length(confus_cat_vals)
		xconfus_ind = length(confus_ind_vals)
		xcorr = length(corr_vals)
		hold_variables = data.frame(N=rep(N_vals,times=xperc*xwind*xconfus_cat*xconfus_ind*xcorr),c1=rep(perc_vals,each=xN,times=xwind*xconfus_cat*xconfus_ind*xcorr),w=rep(wind_vals,each=xN*xperc,times=xconfus_cat*xconfus_ind*xcorr),pcat=rep(confus_cat_vals,each=xN*xperc*xwind,times=xconfus_ind*xcorr),pind=rep(confus_ind_vals,each=xN*xperc*xwind*xconfus_cat,times=xcorr),c2=rep(corr_vals,each=xN*xperc*xwind*xconfus_cat*xconfus_ind))
		hold_error = data.frame(categ=error_cat_mean,indiv=error_ind_mean,hold_variables)
		hold_time = data.frame(categ=time_cat_mean,indiv=time_ind_mean,hold_variables)
		variables = rbind(variables,hold_variables)
		error = rbind(error,hold_error)
		time = rbind(time,hold_time)
	}
	return(list(variables,error,time))
}