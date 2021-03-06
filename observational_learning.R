## ---- functions ----------------------

win_prob <- function(q){ #probability of A winning given qualities of A and B
	d = q[1]-q[2] #differences in qualities
	p = exp(dominance*d)/(exp(dominance*d)+1) #a sigmoidal function that gives probability of A winning 
	return(p)
}

update <- function(a, q){ # given current assessment a and true quality q, update to new assessment
	noise = rnorm(1, mean =0, sd = learn_noise)
	a[is.na(a)] = rnorm(sum(is.na(a)), mean = 0, sd =0.2)
	anew =(1- learn_rate) * a + learn_rate*q + noise
	return(anew)
}

update_obs <-function(a,d){ # given current assessments and true difference in qualities, update to new assessment, d = a1-a2	
	noise = rnorm(1,mean = 0, sd = obs_noise)
	a[is.na(a)] = rnorm(sum(is.na(a)), mean = 0, sd =0.2)
	dnew = (1-obs_learn_rate)*diff(rev(a))+ obs_learn_rate*d + noise
	a1new = mean(a)+dnew/2
	a2new = mean(a)-dnew/2
	return(c(a1new,a2new))
}

fixCorr = function(x1,x2,rho){ #given x1 and x2 produce new x2 with correlation of rho with x1
	n = length(x1)
	theta <- acos(rho)             # corresponding angle
	X     <- cbind(x1, x2)         # matrix
	Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
	orig_means <- colMeans(X)
	Id   <- diag(n)                               # identity matrix
	Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q #this is also just Xctr[,1]/ norm of Xctr[,1]
	P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
	x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
	Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
	Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
	if(rho!=1){
		x2_new <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
		} else { x2_new <- Y[,1]} 
	# x2_new = x2_new + orig_means[2] #recenter x2 at the original mean
	x2_new = scale(x2_new,center=FALSE,scale=0.5*diff(range(x2_new)) ) #for our purposes it's useful to have the same range of x2 regardless of x1 and rho
	return(x2_new)
}

## ---- parameters -------------------------
Tfights = 500 #total number of fights 
N = 20 # individuals
perc_wind = 0 # difference that animals can perceive
memory_window = 200 #how many fights ago they can remember
confus_prob_cat = Inf #maximum probability of misidentifying categories, which decreases with dissimilarity
confus_prob_ind = 0 #probability of misidentifying individuals
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
sig_qual_corr = 1 #correlation between quality and signal
learn_rate = 1 #how much the quality of the opponent affects the new assessment
learn_noise = 0.0 #how noisy an updated assessment is
obs_learn_rate = 1 #how much true difference affects assessment of difference
obs_noise = 0.0 # how noisy observational learning is
p_obs = 1 #probability of observing a fight you're not engaged in
dominance = 2 #how quickly the probability switches from A winning to A losing
error_threshold = 0.2


##---- the_whole_process ---------------------------

dynamics_obs <- function(){
	
	self_memory_window = Inf
	memory_mat = array(memory_window,dim=c(N,N))
	diag(memory_mat) = self_memory_window
	
	#set up group with quality values and signal values: 

	qual_vals = array(data = NA, dim = N) # quality values
	# #option: one animal in each of N bins
	# qual_vals = seq(0, 1, by = 1/N) 
	# #option: uniformly distributed quality values
	# qual_vals = runif(N , min = 0, max = 1) 
	# #option: lognormally distributed quality values
	# qual_vals = exp(rnorm(N, mean = 0, sd = qual_sd)) 
	#option: normally distributed quality values
	qual_vals = rnorm(N, mean = qual_mean, sd = qual_sd) 	
	
	sig_vals = array(data = NA, dim = N) # signal values
	#option: signals are correlated with quality 
	sig_vals = rnorm(N, mean = 0, 1)
	sig_vals = fixCorr(qual_vals,sig_vals,sig_qual_corr)
	# #option: uniformly distributed sigals
	# sig_vals = runif(N, min =0, max = 1) 
	# #option: lognormally distributed signals: 
	# sig_sd = 0.5; sig_vals = exp(rnorm(N, mean =0, sd = sig_sd)) 
	# #option: normally distributed signal values
	# sig_sd = 0.5; sig_vals = rnorm(N, mean =0, sd =sig_sd) 
	
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
			to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
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
	wins = list() # keep track of qualities of members involved in wins and losses for each animal
	losses = list()
	for(i in 1:N){ 
		wins[[i]] = numeric()
		losses[[i]] = numeric()
	}
	
	last_update_cat = array(Inf, dim=c(N,categor_num_max)) #last time each individual thought it encountered each category
	last_update_ind = array(Inf, dim=c(N,N)) #last time each individual thought it encountered each category
	
	a_cat_bycat = array(NA, dim=c(N,categor_num_max,Tfights+1)) #assessment by each individual of each other individual, using categories
	a_cat_byind = array(NA, dim=c(N,N,Tfights+1)) #assessment by each indivdual of each category
	a_ind = array(NA, dim=c(N,N,Tfights+1)) #assessment by each indivdual of each category
		
	for(t in 1:Tfights){
		pair = sample(1:N, 2, replace = FALSE) #draw two animals at random
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
		
		#perception of individuals
		
		last_update_ind = last_update_ind+1
		new_a_ind = a_ind[,,t] 
		new_a_ind[last_update_ind>memory_mat] = NA #if you fought more than memory_window fights ago you forget your assessments
		
		perc_pair = rev(pair)  
		
		prob_vec = array(confus_prob_ind/(N-2),dim=N-1)
		prob_vec[which(setdiff(1:N,pair[1])==pair[2])] = 1-confus_prob_ind
		perc_pair[1] = sample(setdiff(1:N,pair[1]),1,prob=prob_vec) #with low probability draw a different individual that the focal thinks it's interacting with
		
		prob_vec = array(confus_prob_ind/(N-2),dim=N-1)
		prob_vec[which(setdiff(1:N,pair[2])==pair[1])] = 1-confus_prob_ind
		perc_pair[2] = sample(setdiff(1:N,pair[2]),1,prob=prob_vec)			

		last_update_ind[pair[1],c(pair[1],perc_pair[1])] = 0
		last_update_ind[pair[2],c(pair[2],perc_pair[2])] = 0
		
		#perception of categories
		
		last_update_cat = last_update_cat+1 #everyone last update time increments by 1	
		new_a_cat_bycat = a_cat_bycat[,,t] 			
		new_a_cat_bycat[last_update_cat>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago
		
		new_a_cat_byind = a_cat_byind[,,t]
		diag(new_a_cat_byind)[which(diag(last_update_ind)>diag(memory_mat))] = NA
		
		draw = matrix(runif(N*N,0,1),nrow=N) #random numbers to generate confusion events
		perc_cats = array(NA, dim = c(N,N)) #each individual's perception of every other's category
		for(i in 1:N){
			perc_cats[i,] = rowSums(matrix(rep(draw[i,],categor_num[i]),ncol=categor_num[i])>confus_mat[[i]])+1 #use confus_mat to see which category perceptions get switched to
		}
			# perc_cats = matrix(rep(sig_cats,N),nrow=N,byrow=TRUE) #no switching categories	
											
		last_update_cat[pair[1],perc_cats[pair[1],pair[2]]] = 0 #each animal thinks it just fought with the category it perceived
		last_update_cat[pair[2],perc_cats[pair[2],pair[1]]] = 0					
								
		#learning about the identity of one's opponent:											
						
			new_a_ind[pair[1],perc_pair[1]] = update(new_a_ind[pair[1],perc_pair[1]],qual_vals[pair[2]]) #each animal updates its assessment of the individual it perceives based on the quality it experiences
			new_a_ind[pair[1],pair[1]] = update(new_a_ind[pair[1],pair[1]],qual_vals[pair[1]])
			new_a_ind[pair[2],perc_pair[2]] = update(new_a_ind[pair[2],perc_pair[2]],qual_vals[pair[1]])
			new_a_ind[pair[2],pair[2]] = update(new_a_ind[pair[2],pair[2]],qual_vals[pair[2]])

			#observation			
			if(length(observers)>0){
				for(o in observers){
					prob_vec = array(confus_prob_ind/(N-2),dim=N-1)
					prob_vec[which(setdiff(1:N,o)==pair[1])] = 1-confus_prob_ind
					perc_pair[1] = sample(setdiff(1:N,o),1,prob=prob_vec) #with low probability draw a different individual that the focal thinks is first individual
					prob_vec = array(confus_prob_ind/(N-3),dim=N-2)
					prob_vec[which(setdiff(1:N,c(o,perc_pair[1]))==pair[2])] = 1-confus_prob_ind
					perc_pair[2] = sample(setdiff(1:N,c(o,perc_pair[1])),1,prob=prob_vec) #with low probability draw a different individual that the focal thinks is second individual	
					last_update_ind[o,perc_pair]=0 
					new_a_ind[o,perc_pair] = update_obs(new_a_ind[o,perc_pair],diff(rev(qual_vals[pair])))
				}
			}	

			a_ind[,,t+1] = new_a_ind
			
			#learning about the signal of one's opponent:											
													
			new_a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]]] = update(new_a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]]],qual_vals[pair[2]]) #each animal updates its assessment of the category it perceives based on the quality it experiences
			new_a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]]] = update(new_a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]]],qual_vals[pair[1]])
			
			#observation			
			if(length(observers)>0){
				for(o in observers){
					last_update_cat[o,perc_cats[o,pair]]=0 #here, observer remembers interacting with observed regardless of whether they're in the same category. if i move inside the if statement, only remember if he updates his opinion
					if(diff(perc_cats[o,pair])!=0){
						new_a_cat_bycat[o,perc_cats[o,pair]] = update_obs(new_a_cat_bycat[o,perc_cats[o,pair]],diff(rev(qual_vals[pair])))
					}
				}
			}
						
			for(i in 1:N){
				new_a_cat_byind[i,setdiff(1:N,i)]=new_a_cat_bycat[i,perc_cats[i,setdiff(1:N,i)]] #each animal assigns quality values to individuals based on its sloppy assignment of individuals to categories				
			}
			new_a_cat_byind[pair[1],pair[1]] = update(new_a_cat_byind[pair[1],pair[1]],qual_vals[pair[1]])
			new_a_cat_byind[pair[2],pair[2]] = update(new_a_cat_byind[pair[2],pair[2]],qual_vals[pair[2]])
			
			a_cat_bycat[,,t+1] = new_a_cat_bycat
			a_cat_byind[,,t+1] = new_a_cat_byind
	}
	
	#how well did they learn?
	error_cat = array(NA, dim = c(N,Tfights+1))
	error_ind = array(NA, dim = c(N,Tfights+1))
	#how quickly did they get close to their final assessment?
	time_cat = array(NA, dim=c(N,1))
	time_ind = array(NA, dim=c(N,1))
	# #what was everyone else's opinion of me over time?
	# highrank_cat = array(NA, dim=c(N,Tfights+1))
	# highrank_ind = array(NA, dim=c(N,Tfights+1))
	# #costs of losing? while fights are random, these don't depend on the recognition system, but we'll need two if we consider strategic fights.
	# losing_costs_cat = array(NA, dim=c(N,Tfights+1))
	# losing_costs_ind = array(NA, dim=c(N,Tfights+1))
	for(i in 1:N){
		for(t in 1:(Tfights+1)){
	
			if(sum(!is.na(a_cat_byind[i,,t]))>=1){
				error_cat[i,t] = sum(abs(a_cat_byind[i,,t]-qual_vals) ,na.rm=TRUE) /sum(!is.na(a_cat_byind[i,,t])) #/ abs(qual_vals)
				}
			if(sum(!is.na(a_ind[i,,t]))>=1){
				error_ind[i,t] = sum(abs(a_ind[i,,t]-qual_vals ) ,na.rm=TRUE) /sum(!is.na(a_ind[i,,t]))  #/ abs(qual_vals)
				}
			# if(sum(!is.na(a_cat_byind[,i,t]))>=1){
			# highrank_cat[i,t] = sum(a_cat_byind[,i,t],na.rm=TRUE) /sum(!is.na(a_cat_byind[,i,t]))
				# }
			# if(sum(!is.na(a_ind[,i,t]))>=1){
				# highrank_ind[i,t] = sum(a_ind[,i,t],na.rm=TRUE) /sum(!is.na(a_ind[,i,t]))
				# }
		}
		time_cat_temp = array(NA,dim=N)
		time_ind_temp = array(NA, dim=N)
		for(j in setdiff(1:N,i)){
			# v = which(abs(a_cat_byind[i,j,]-a_cat_byind[i,j,Tfights+1])<= error_threshold)
			v = which(abs(a_cat_byind[i,j,]-qual_vals[j])<=error_threshold)
			if(length(v)>0){
				time_cat_temp[j] = v[1]-1
			} else{ time_cat_temp[j] = Tfights}
			# v = which(abs(a_ind[i,j,]-a_ind[i,j,Tfights+1])<= error_threshold)
			v = which(abs(a_ind[i,j,]-qual_vals[j])<=error_threshold)
			if(length(v)>0){
				time_ind_temp[j] = v[1]-1
			} else{ time_ind_temp[j] = Tfights}
		}
		time_cat[i] = mean(time_cat_temp,na.rm=TRUE)
		time_ind[i] = mean(time_ind_temp,na.rm=TRUE)
	}
	return(list(error_cat,error_ind,time_cat,time_ind))
}
