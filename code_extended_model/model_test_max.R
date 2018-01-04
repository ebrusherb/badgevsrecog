update_test <- function(a, q){ # given current assessment a and true quality q, update to new assessment
	noise = rnorm(1, mean =0, sd = learn_noise)
	a[is.na(a)] = 0 # rnorm(sum(is.na(a)), mean = 0, sd =0.2) #0
	anew =(1- learn_rate) * a + learn_rate*(q + noise)
	return(anew)
}


##---- the_whole_process ---------------------------

dynamics_cat_max <- function(){
	
	self_memory_window = Inf
	memory_mat = array(memory_window,dim=c(N,N))
	diag(memory_mat) = self_memory_window
	
	#set up group with quality values and signal values: 
	
	qual_vals = rnorm(N, mean = qual_mean, sd = qual_sd) 	
	
	sig_vals = rnorm(N, mean = 0, 1)
	sig_vals = fixCorr(qual_vals,sig_vals,rho)	
	
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
			to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=delta/2)
			if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf){
				to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
			}
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
	fights = array(NA,dim=c(2,Tfights))
	num_updates = array(0,dim=c(1,N))
	wins = list() # keep track of qualities of members involved in wins and losses for each animal
	losses = list()
	for(i in 1:N){ 
		wins[[i]] = numeric()
		losses[[i]] = numeric()
	}
	
	last_update_cat = array(Inf, dim=c(N,categor_num_max)) #last time each individual thought it encountered each category	
	
	a_cat_bycat = array(NA, dim=c(N,categor_num_max,Tfights+1)) #assessment by each individual of each category
	a_ind_bycat = array(NA, dim=c(N,N,Tfights+1)) #assessment by each individual of each other individual, using categories		
		
	for(t in 1:Tfights){
		pair = sample(1:N, 2, replace = FALSE) #draw two animals at random
		fights[,t] = pair;
		num_updates[pair] = num_updates[pair]+1
		p = win_prob(qual_vals[pair]) # probability of first animal winning
		outcome = sample(0:1,1,prob = c(p,1-p)) # see who wins and loses
		if(outcome == 0){
			wins[[pair[1]]] = cbind(wins[[pair[1]]],qual_vals[pair]) #add quality values to current wins for pair[1]
			losses[[pair[2]]] = cbind(losses[[pair[2]]],qual_vals[rev(pair)]) #add quality values to current losses for pair[2], reversed so focal animal is first
		} else{
			wins[[pair[2]]] = cbind(wins[[pair[2]]],qual_vals[rev(pair)])
			losses[[pair[1]]] = cbind(losses[[pair[1]]],qual_vals[pair])
		}
		
		observers = setdiff(1:N,pair)
		observers = observers[as.logical(rbinom(N-2,1,p_obs))]				
		
		#perception of categories
		
		last_update_cat = last_update_cat+1 #everyone last update time increments by 1	
		new_a_cat_bycat = a_cat_bycat[,,t] 			
		new_a_cat_bycat[last_update_cat>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago
		
		new_a_ind_bycat = a_ind_bycat[,,t]
		
		draw = matrix(runif(N*N,0,1),nrow=N) #random numbers to generate confusion events
		perc_cats = array(NA, dim = c(N,N)) #each individual's perception of every other's category
		for(i in 1:N){
			perc_cats[i,] = rowSums(matrix(rep(draw[i,],categor_num[i]),ncol=categor_num[i])>confus_mat[[i]])+1 #use confus_mat to see which category perceptions get switched to
		}
			# perc_cats = matrix(rep(sig_cats,N),nrow=N,byrow=TRUE) #no switching categories	
											
		last_update_cat[pair[1],perc_cats[pair[1],pair[2]]] = 0 #each animal thinks it just fought with the category it perceived
		last_update_cat[pair[2],perc_cats[pair[2],pair[1]]] = 0														 							
						
			#learning about the signal of one's opponent:											
													
			new_a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]]] = update(new_a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]]],qual_vals[pair[2]]) #each animal updates its assessment of the category it perceives based on the quality it experiences
			new_a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]]] = update(new_a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]]],qual_vals[pair[1]])
			
			if(observation_happens){
				#observation			
				if(length(observers)>0){
					for(o in observers){
						last_update_cat[o,perc_cats[o,pair]]=0 #here, observer remembers interacting with observed regardless of whether they're in the same category. if i move inside the if statement, only remember if he updates his opinion
						if(diff(perc_cats[o,pair])!=0){
							new_a_cat_bycat[o,perc_cats[o,pair]] = update_obs(new_a_cat_bycat[o,perc_cats[o,pair]],diff(rev(qual_vals[pair])))
						}
					}
				}
			}
						
			for(i in 1:N){
				new_a_ind_bycat[i,setdiff(1:N,i)]=new_a_cat_bycat[i,perc_cats[i,setdiff(1:N,i)]] #each animal assigns quality values to individuals based on its sloppy assignment of individuals to categories				
			}
			new_a_ind_bycat[pair[1],pair[1]] = update(new_a_ind_bycat[pair[1],pair[1]],qual_vals[pair[1]])
			new_a_ind_bycat[pair[2],pair[2]] = update(new_a_ind_bycat[pair[2],pair[2]],qual_vals[pair[2]])
			
			a_cat_bycat[,,t+1] = new_a_cat_bycat
			a_ind_bycat[,,t+1] = new_a_ind_bycat
											
	}
	
	#how well did they learn?
	error_cat = array(NA, dim = c(N,length(seq(2,Tfights+1,by=down_sample))))	
	
	for(i in 1:N){
		for(t in 1:length(seq(2,Tfights+1,by=down_sample))){
			
			time = seq(2,Tfights+1,by=down_sample)[t]
			
			if(sum(!is.na(a_ind_bycat[i,,time]))>=1){
				# error_cat[i,t] = sum(abs(a_ind_bycat[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)]) ,na.rm=TRUE) /sum(!is.na(a_ind_bycat[i,setdiff(1:N,i),time])) #/ abs(qual_vals)
				error_cat[i,t] = max(abs(a_ind_bycat[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)]) ,na.rm=TRUE)
				}						
		}		
	}
	return(list(error_cat))
}

dynamics_rule_max <- function(){
	
	self_memory_window = Inf
	memory_mat = array(memory_window,dim=c(N,N))
	diag(memory_mat) = self_memory_window
	
	#set up group with quality values and signal values: 
	
	qual_vals = rnorm(N, mean = qual_mean, sd = qual_sd) 	
	
	sig_vals = rnorm(N, mean = 0, 1)
	sig_vals = fixCorr(qual_vals,sig_vals,rho)		
	
	#the fighting and learning dynamics: 
	fights = array(NA,dim=c(2,Tfights))
	num_updates = array(0,dim=c(1,N))
	wins = list() # keep track of qualities of members involved in wins and losses for each animal
	losses = list()
	for(i in 1:N){ 
		wins[[i]] = numeric()
		losses[[i]] = numeric()
	}
		
	rule_observations = array(NA,dim=c(N,3,2*min(memory_window,Tfights+1))) #x2 so that observers can add two new observations
	rules = array(NA,dim=c(N,2))
	a_ind_byrule = array(NA,dim=c(N,N,Tfights+1)) #assessment by each individual of each other individual, using a rule
		
	for(t in 1:Tfights){
		pair = sample(1:N, 2, replace = FALSE) #draw two animals at random
		fights[,t] = pair;
		num_updates[pair] = num_updates[pair]+1
		p = win_prob(qual_vals[pair]) # probability of first animal winning
		outcome = sample(0:1,1,prob = c(p,1-p)) # see who wins and loses
		if(outcome == 0){
			wins[[pair[1]]] = cbind(wins[[pair[1]]],qual_vals[pair]) #add quality values to current wins for pair[1]
			losses[[pair[2]]] = cbind(losses[[pair[2]]],qual_vals[rev(pair)]) #add quality values to current losses for pair[2], reversed so focal animal is first
		} else{
			wins[[pair[2]]] = cbind(wins[[pair[2]]],qual_vals[rev(pair)])
			losses[[pair[1]]] = cbind(losses[[pair[1]]],qual_vals[pair])
		}
		
		observers = setdiff(1:N,pair)
		observers = observers[as.logical(rbinom(N-2,1,p_obs))]							
			
			#learning the quality-signal rule
						
			to_forget = array(NA,dim=c(N,3,2*min(memory_window,Tfights+1)))
			to_forget[,1,]=rule_observations[,3,]
			to_forget[,2,]=rule_observations[,3,]
			to_forget[,3,]=rule_observations[,3,]
			rule_observations[(t-to_forget)>memory_window] = NA 
			
			w1 = which(is.na(rule_observations[pair[1],3,]))[1]
			rule_observations[pair[1],,w1] = c(sig_vals[pair[2]],qual_vals[pair[2]]+rnorm(1,mean=0,sd=learn_noise),t)
			w2 = which(is.na(rule_observations[pair[2],3,]))[1]
			rule_observations[pair[2],,w2] = c(sig_vals[pair[1]],qual_vals[pair[1]]+rnorm(1,mean=0,sd=learn_noise),t)
			
			if(observation_happens){
				if(length(observers)>0){
					for(o in observers){
						w = which(is.na(rule_observations[o,3,]))[1:2]
						rule_observations[o,,w[1]] = c(sig_vals[pair[1]],qual_vals[pair[1]]+rnorm(1,mean=0,sd=learn_noise),t)
						rule_observations[o,,w[2]] = c(sig_vals[pair[2]],qual_vals[pair[2]]+rnorm(1,mean=0,sd=learn_noise),t)
					}
				}
			}
			
			rules=t(apply(rule_observations,1,function(m){if(sum(!is.na(m[1,]))>=2&&diff(range(m[1,],na.rm=TRUE))!=0){l=lm(m[2,]~m[1,]);summary(l)$coefficients[,1]}else{c(NA,NA)}}))
			
			a_ind_byrule[,,t+1] = t(apply(rules,1,function(v) sig_vals*v[2]+v[1]))				
			
	}
	
	#how well did they learn?	
	error_rule = array(NA, dim=c(N,length(seq(2,Tfights+1,by=down_sample))))	
	
	for(i in 1:N){
		for(t in 1:length(seq(2,Tfights+1,by=down_sample))){
			
			time = seq(2,Tfights+1,by=down_sample)[t]			
			
			if(sum(!is.na(a_ind_byrule[i,,time]))>=1){
				# error_rule[i,t] = sum(abs(a_ind_byrule[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)] ) ,na.rm=TRUE) /sum(!is.na(a_ind_byrule[i,setdiff(1:N,i),time]))  #/ abs(qual_vals)
				error_rule[i,t] = max(abs(a_ind_byrule[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)] ),na.rm=TRUE)
				}
			
		}		
	}
	return(list(error_rule))
}


dynamics_genetic_max <- function(){
	
	qual_vals = rnorm(N, mean = qual_mean, sd = qual_sd) 	
	
	sig_vals = rnorm(N, mean = 0, 1)
	sig_vals = fixCorr(qual_vals,sig_vals,rho)
	
	genetic = genetic_rule(rho)
	a_ind_genetic = array(NA,dim=c(N,N,Tfights+1))
	for(i in 1:N){
		a_ind_genetic[i,,2:(Tfights+1)] = matrix(rep(genetic[1]+genetic[2]*sig_vals,Tfights),ncol=Tfights,byrow=FALSE)
		}
		
	#how well did they learn?	
	error_genetic = array(NA, dim=c(N,length(seq(2,Tfights+1,by=down_sample))))	
	
	for(i in 1:N){
		for(t in 1:length(seq(2,Tfights+1,by=down_sample))){
			
			time = seq(2,Tfights+1,by=down_sample)[t]			
			
			if(sum(!is.na(a_ind_genetic[i,,time]))>=1){
				# error_genetic[i,t] = sum(abs(a_ind_genetic[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)] ) ,na.rm=TRUE) /sum(!is.na(a_ind_genetic[i,setdiff(1:N,i),time]))  #/ abs(qual_vals)
				error_genetic[i,t] = max(abs(a_ind_genetic[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)] ) ,na.rm=TRUE)				}
			
		}		
	}
	
	return(list(error_genetic))
}



dynamics_cat_full <- function(){
	
	self_memory_window = Inf
	memory_mat = array(memory_window,dim=c(N,N))
	diag(memory_mat) = self_memory_window
	
	#set up group with quality values and signal values: 
	
	qual_vals = rnorm(N, mean = qual_mean, sd = qual_sd) 	
	
	sig_vals = rnorm(N, mean = 0, 1)
	sig_vals = fixCorr(qual_vals,sig_vals,rho)	
	
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
			to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=delta/2)
			if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf){
				to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
			}
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
	fights = array(NA,dim=c(2,Tfights))
	num_updates = array(0,dim=c(1,N))
	wins = list() # keep track of qualities of members involved in wins and losses for each animal
	losses = list()
	for(i in 1:N){ 
		wins[[i]] = numeric()
		losses[[i]] = numeric()
	}
	
	last_update_cat = array(Inf, dim=c(N,categor_num_max)) #last time each individual thought it encountered each category	
	
	a_cat_bycat = array(NA, dim=c(N,categor_num_max,Tfights+1)) #assessment by each individual of each category
	a_ind_bycat = array(NA, dim=c(N,N,Tfights+1)) #assessment by each individual of each other individual, using categories	
	
	a_cat_bycat2 = array(NA, dim=c(N,categor_num_max,Tfights+1)) #assessment by each individual of each category
	a_ind_bycat2 = array(NA, dim=c(N,N,Tfights+1)) #assessment by each individual of each other individual, using categories		
		
	for(t in 1:Tfights){
		pair = sample(1:N, 2, replace = FALSE) #draw two animals at random
		fights[,t] = pair;
		num_updates[pair] = num_updates[pair]+1
		p = win_prob(qual_vals[pair]) # probability of first animal winning
		outcome = sample(0:1,1,prob = c(p,1-p)) # see who wins and loses
		if(outcome == 0){
			wins[[pair[1]]] = cbind(wins[[pair[1]]],qual_vals[pair]) #add quality values to current wins for pair[1]
			losses[[pair[2]]] = cbind(losses[[pair[2]]],qual_vals[rev(pair)]) #add quality values to current losses for pair[2], reversed so focal animal is first
		} else{
			wins[[pair[2]]] = cbind(wins[[pair[2]]],qual_vals[rev(pair)])
			losses[[pair[1]]] = cbind(losses[[pair[1]]],qual_vals[pair])
		}
		
		observers = setdiff(1:N,pair)
		observers = observers[as.logical(rbinom(N-2,1,p_obs))]				
		
		#perception of categories
		
		last_update_cat = last_update_cat+1 #everyone last update time increments by 1	
		new_a_cat_bycat = a_cat_bycat[,,t] 			
		new_a_cat_bycat[last_update_cat>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago
		
		new_a_ind_bycat = a_ind_bycat[,,t]
		
		new_a_cat_bycat2 = a_cat_bycat2[,,t] 			
		new_a_cat_bycat2[last_update_cat>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago
		
		new_a_ind_bycat2 = a_ind_bycat2[,,t]
		
		draw = matrix(runif(N*N,0,1),nrow=N) #random numbers to generate confusion events
		perc_cats = array(NA, dim = c(N,N)) #each individual's perception of every other's category
		for(i in 1:N){
			perc_cats[i,] = rowSums(matrix(rep(draw[i,],categor_num[i]),ncol=categor_num[i])>confus_mat[[i]])+1 #use confus_mat to see which category perceptions get switched to
		}
			# perc_cats = matrix(rep(sig_cats,N),nrow=N,byrow=TRUE) #no switching categories	
											
		last_update_cat[pair[1],perc_cats[pair[1],pair[2]]] = 0 #each animal thinks it just fought with the category it perceived
		last_update_cat[pair[2],perc_cats[pair[2],pair[1]]] = 0														 							
						
			#learning about the signal of one's opponent:											
			learn_rate = 0.25										
			new_a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]]] = update_test(new_a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]]],qual_vals[pair[2]]) #each animal updates its assessment of the category it perceives based on the quality it experiences
			new_a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]]] = update_test(new_a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]]],qual_vals[pair[1]])
			
			if(observation_happens){
				#observation			
				if(length(observers)>0){
					for(o in observers){
						last_update_cat[o,perc_cats[o,pair]]=0 #here, observer remembers interacting with observed regardless of whether they're in the same category. if i move inside the if statement, only remember if he updates his opinion
						if(diff(perc_cats[o,pair])!=0){
							new_a_cat_bycat[o,perc_cats[o,pair]] = update_obs(new_a_cat_bycat[o,perc_cats[o,pair]],diff(rev(qual_vals[pair])))
						}
					}
				}
			}
											
			for(i in 1:N){
				new_a_ind_bycat[i,setdiff(1:N,i)]=new_a_cat_bycat[i,perc_cats[i,setdiff(1:N,i)]] #each animal assigns quality values to individuals based on its sloppy assignment of individuals to categories				
			}
			new_a_ind_bycat[pair[1],pair[1]] = update(new_a_ind_bycat[pair[1],pair[1]],qual_vals[pair[1]])
			new_a_ind_bycat[pair[2],pair[2]] = update(new_a_ind_bycat[pair[2],pair[2]],qual_vals[pair[2]])
			
			a_cat_bycat[,,t+1] = new_a_cat_bycat
			a_ind_bycat[,,t+1] = new_a_ind_bycat
			
			#learning about the signal of one's opponent:											
			learn_rate = 0.5										
			new_a_cat_bycat2[pair[1],perc_cats[pair[1],pair[2]]] = update(new_a_cat_bycat2[pair[1],perc_cats[pair[1],pair[2]]],qual_vals[pair[2]]) #each animal updates its assessment of the category it perceives based on the quality it experiences
			new_a_cat_bycat2[pair[2],perc_cats[pair[2],pair[1]]] = update_test(new_a_cat_bycat2[pair[2],perc_cats[pair[2],pair[1]]],qual_vals[pair[1]])
			
			if(observation_happens){
				#observation			
				if(length(observers)>0){
					for(o in observers){
						last_update_cat[o,perc_cats[o,pair]]=0 #here, observer remembers interacting with observed regardless of whether they're in the same category. if i move inside the if statement, only remember if he updates his opinion
						if(diff(perc_cats[o,pair])!=0){
							new_a_cat_bycat2[o,perc_cats[o,pair]] = update_obs(new_a_cat_bycat2[o,perc_cats[o,pair]],diff(rev(qual_vals[pair])))
						}
					}
				}
			}
											
			for(i in 1:N){
				new_a_ind_bycat2[i,setdiff(1:N,i)]=new_a_cat_bycat2[i,perc_cats[i,setdiff(1:N,i)]] #each animal assigns quality values to individuals based on its sloppy assignment of individuals to categories				
			}
			new_a_ind_bycat2[pair[1],pair[1]] = update(new_a_ind_bycat2[pair[1],pair[1]],qual_vals[pair[1]])
			new_a_ind_bycat2[pair[2],pair[2]] = update(new_a_ind_bycat2[pair[2],pair[2]],qual_vals[pair[2]])
			
			a_cat_bycat2[,,t+1] = new_a_cat_bycat2
			a_ind_bycat2[,,t+1] = new_a_ind_bycat2
											
	}
	
	#how well did they learn?
	error_cat = array(NA, dim = c(N,N,length(seq(2,Tfights+1,by=down_sample))))	
	error_cat2 = array(NA, dim = c(N,N,length(seq(2,Tfights+1,by=down_sample))))		
	for(i in 1:N){
		for(t in 1:length(seq(2,Tfights+1,by=down_sample))){
			
			time = seq(2,Tfights+1,by=down_sample)[t]			
			error_cat[i,setdiff(1:N,i),t] = abs(a_ind_bycat[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)])
			}
	}
	for(i in 1:N){
		for(t in 1:length(seq(2,Tfights+1,by=down_sample))){
			
			time = seq(2,Tfights+1,by=down_sample)[t]			
			error_cat2[i,setdiff(1:N,i),t] = abs(a_ind_bycat2[i,setdiff(1:N,i),time]-qual_vals[setdiff(1:N,i)])
			}	
	}
	return(list(error_cat,error_cat2,fights))
}
