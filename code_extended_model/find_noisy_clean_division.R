test <- function(v){
	d_var = moving_var(v[start:601],10);
	return(max(d_var[start3:length(d_var)],na.rm=TRUE))
}

V = apply(error_cat,2,test)
M = apply(error_cat,2,function(v) diff(range(v[start2:length(v)],na.rm=TRUE)))