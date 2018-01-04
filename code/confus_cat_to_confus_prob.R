source('base_model.R')
N=N_vals[n]
sig_qual_corr = corr_vals[c2]
perc_wind = 0
runs = 500
confus_prob_vals = confus_cat_vals

for(r in 1:length(confus_cat_vals)){
	hold_prob = array(NA,dim=runs)
	for(s in 1:runs){
	
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
	test = floor(length(cat_med)/2)
	hold_prob[s] = 1-1/sum(exp(-confus_cat_vals[r]*abs(cat_med[test]-cat_med)))
	}
	confus_prob_vals[r] = mean(hold_prob)
}

confus_prob_vals[which(confus_cat_vals==Inf)]=0