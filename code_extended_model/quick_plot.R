noisy = array(NA,dim(parameters_cat)[1])

i=6
w = w_vec[i]
cost_cat = apply(error_cat,2,function(v) 2*w*v+(1-w)*time/30001)
plot(1:601,1:601,ylim=c(0,1),t='n')
for(i in c(1027,1030)){
d = c(rep(NA,start-1),diff(cost_cat[start:length(time),i]))
		k = 70
		d_avg = moving_average(d,k)
		# d_var = moving_var(d,10)
		d_var = moving_var(error_cat[start:length(time),i],10)
		max_var = max(d_var[start3:length(d_var)],na.rm=TRUE)
		M = diff(range(error_cat[start2:length(time),i],na.rm=TRUE))
		# if((diff(range(error_cat[start2:dim(cost_cat)[1],i],na.rm=TRUE)))>0.15){
		if(M>0.075 && max_var<0.00006*M){
			offset = 4
			s = sign(myround(d_avg,4.2))
			# }else if((diff(range(error_cat[start2:dim(cost_cat)[1],i],na.rm=TRUE)))>0.1){
			noisy[i] = FALSE
			}else if((M>0.1 && max_var<0.0004*M && w<1) || max_var<0.00018*M){				
			offset = 4
			s = sign(myround(d_avg,3.5))
			noisy[i] = FALSE
			}else{
			offset = 10
			s = sign(myround(d_avg,3.1))	
			noisy[i] = TRUE
			}
		s[s==0] = 1
		z = apply(matrix(1:length(s),ncol=1),1,function(x) identical(s[(max(0,x-offset):min(length(s),x+offset))],rep(s[x],offset*2+1)))
		if(length(s[z])>1){
			f = which(z)[find_changes(s[z])]-offset
		}else{
			f = NULL
		}	
		if(length(f)==0 || length(f)>3){
			if(sum(z)==0 || max(s[z],na.rm=TRUE)>(-1)){
				best_cat[i] = start2
			} else{
				best_cat[i] = length(time)
			}
		}else {
			if(max(s[f])==-1){
				best_cat[i] = length(time)
			}else{
				best_cat[i] = min(f[which(s[f]>(-1))])
			}
		}
		
lines(cost_cat[,i],t='l',lwd=2,ylim=c(0,1))
abline(v=best_cat[i],col='red',lwd=2)
abline(h=cost_cat[best_cat[i],i],col='red',lwd=2)
}

# i=8
# w = w_vec[i]
# cost_rule = apply(error_rule,2,function(v) 2*w*v+(1-w)*time/30001)
# i=13
# d = c(rep(NA,start-1),diff(cost_rule[start:length(time),i]))
		# k = 5
		# d_avg = moving_average(d,k,keep_end=FALSE)[1:(length(d)-floor((k-1)/2))]	
		# d_var = moving_var(error_rule[start:length(time),i],10)	
		# offset = 5
		# s = sign(myround(d_avg,3))	
		# if(max(d_var[start3:length(d_var)],na.rm=TRUE)<4e-05){				
			# offset = 6
			# s = sign(myround(d_avg,3.1))
		# }else{
			# offset = 10
			# s = sign(myround(d_avg,3))	
		# }
		# z = apply(matrix(1:length(s),ncol=1),1,function(x) identical(s[(max(0,x-offset):min(length(s),x+offset))],rep(s[x],offset*2+1)))
		# if(length(s[z])>1){
			# f = which(z)[find_changes_immediate(s[z])]-offset
		# }else{
			# f = NULL
		# }	
		# if(length(f)==0 || length(f)>3){
			# if(sum(z)==0 || max(s[z],na.rm=TRUE)>(-1)){
				# best_rule[i] = start2
			# } else{
				# best_rule[i] = length(time)
			# }
		# }else {
			# if(max(s[f])==-1){
				# best_rule[i] = length(time)
			# }else{
				# best_rule[i] = min(f[which(s[f]>(-1))])
			# }
		# }

# plot(cost_rule[,i],t='l',lwd=2,ylim=c(0,1))
# abline(v=best_rule[i],col='red',lwd=2)

# ####
# var='delta'
# examples_var = examples
# examples_var[[var]] = NA
# f_cat = find_df(examples_var)
# f_rule = find_df(examples_var[,rule_relevant],p=parameters_rule)	

# noisy = array(NA,dim(parameters_cat)[1])

# i=559
# d = c(rep(NA,start-1),diff(cost_cat[start:length(time),i]))
		# k = 70
		# d_avg = moving_average(d,k)
		# # d_var = moving_var(d,10)
		# d_var = moving_var(error_cat[start:length(time),i],10)
		# max_var = max(d_var[start3:length(d_var)],na.rm=TRUE)
		# M = diff(range(error_cat[start2:length(time),i],na.rm=TRUE))
		# # if((diff(range(error_cat[start2:dim(cost_cat)[1],i],na.rm=TRUE)))>0.15){
		# if(M>0.075 && max_var<0.00006*M){
			# offset = 4
			# s = sign(myround(d_avg,4.2))
			# # }else if((diff(range(error_cat[start2:dim(cost_cat)[1],i],na.rm=TRUE)))>0.1){
			# noisy[i] = FALSE
			# }else if(max_var<0.0002*M){				
			# offset = 4
			# s = sign(myround(d_avg,3.5))
			# noisy[i] = FALSE
			# }else{
			# offset = 10
			# s = sign(myround(d_avg,3))	
			# noisy[i] = TRUE
			# }
		# s[s==0] = 1
		# z = apply(matrix(1:length(s),ncol=1),1,function(x) identical(s[(max(0,x-offset):min(length(s),x+offset))],rep(s[x],offset*2+1)))
		# if(length(s[z])>1){
			# f = which(z)[find_changes(s[z])]-offset
		# }else{
			# f = NULL
		# }	
		# if(length(f)==0 || length(f)>3){
			# if(sum(z)==0 || max(s[z],na.rm=TRUE)>(-1)){
				# best_cat[i] = start2
			# } else{
				# best_cat[i] = length(time)
			# }
		# }else {
			# if(max(s[f])==-1){
				# best_cat[i] = length(time)
			# }else{
				# best_cat[i] = min(f[which(s[f]>(-1))])
			# }
		# }

# plot(cost_cat[,i],t='l',lwd=2,ylim=c(0,1))
# abline(v=best_cat[i],col='red',lwd=2)


#####
# i=24

# d = c(rep(NA,start-1),diff(cost_rule[start:length(time),i]))
		# k = 5
		# d_avg = moving_average(d,k,keep_end=FALSE)[1:(length(d)-floor((k-1)/2))]	
		# d_var = moving_var(error_cat[start:length(time),i],10)	
		# offset = 5
		# s = sign(myround(d_avg,3))	
		# if(max(d_var[start3:length(d_var)],na.rm=TRUE)<4e-05){				
			# offset = 6
			# s = sign(myround(d_avg,3.15))
		# }else{
			# offset = 10
			# s = sign(myround(d_avg,3))	
		# }
		# z = apply(matrix(1:length(s),ncol=1),1,function(x) identical(s[(max(0,x-offset):min(length(s),x+offset))],rep(s[x],offset*2+1)))
		# if(length(s[z])>1){
			# f = which(z)[find_changes_immediate(s[z])]-offset
		# }else{
			# f = NULL
		# }	
		# if(length(f)==0 || length(f)>3){
			# if(sum(z)==0 || max(s[z],na.rm=TRUE)>(-1)){
				# best_rule[i] = start2
			# } else{
				# best_rule[i] = length(time)
			# }
		# }else {
			# if(max(s[f])==-1){
				# best_rule[i] = length(time)
			# }else{
				# best_rule[i] = min(f[which(s[f]>(-1))])
			# }
		# }
	
# plot(cost_rule[,i],t='l',lwd=2,xlim=c(0,600),ylim=c(0,1));abline(v=best_rule[i],col='red',lwd=2);parameters_rule[i,]	