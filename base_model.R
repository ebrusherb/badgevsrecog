## ---- functions ----------------------

win_prob <- function(q){ #probability of A winning given qualities of A and B
	d = q[1]-q[2] #differences in qualities
	p = exp(dominance*d)/(exp(dominance*d)+1) #a sigmoidal function that gives probability of A winning 
	return(p)
}

update <- function(a, q){ # given current assessment a and true quality q, update to new assessment
	if(learn_noise == 0 ){
		noise = 0
	} else {
		noise = rnorm(1, mean =0, sd = learn_noise)}
	if(!is.na(a)){
			anew =(1- learn_rate) * a + learn_rate*q + noise
		} else{
			a = qual_mean + rnorm(1, mean =0 , sd = .2)
			anew = (1- learn_rate) * a + learn_rate*q + noise
		}
	return(anew)
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
	x2_new = x2_new + orig_means[2]
	return(x2_new)
}

## ---- parameters -------------------------
Tfights = 100 #total number of fights 
N = 20 # individuals
perc_wind = 1 # difference that animals can perceive
memory_window = 200 #how many fights ago they can remember
confus_prob_cat = 500 #maximum probability of misidentifying categories, which decreases with dissimilarity
confus_prob_ind = 0.1 #probability of misidentifying individuals
qual_mean = 0
qual_sd = 0.5 #standard deviation of quality distribution 
sig_qual_corr = 0.5 #correlation between quality and signal
learn_rate = 0.2 #how much the quality of the opponent affects the new assessment
learn_noise = 0.01 #how noisy an updated assessment is
dominance = 2 #how quickly the probability switches from A winning to A losing
error_threshold = 0.2


##---- the_whole_process ---------------------------
#set up group with quality values and signal values: 

dynamics <- function(){

	qual_vals = array(data = NA, dim = N) # quality values
	# #option: one animal in each of N bins
	# qual_vals = seq(0, 1, by = 1/N) 
	# #option: uniformly distributed quality values
	# qual_vals = runif(N , min = 0, max = 1) 
	# #option: lognormally distributed quality values
	# qual_vals = exp(rnorm(N, mean = 0, sd = qual_sd)) 
	#option: normally distributed quality values
	qual_vals = rnorm(N, mean = qual_mean, sd = qual_sd) 
	
	# #matrix of columns of quality values
	# qual_mat = matrix(rep(qual_vals,N), ncol=N, byrow=TRUE) 
	
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
		for(j in 1:N){
			confus_mat[[i]][j,]=cumsum(exp(-confus_prob_cat*abs(sig_vals[j]-cat_med))/sum(exp(-confus_prob_cat*abs(sig_vals[j]-cat_med)))) #easier to keep track of cumulative confusion probabilities
		}
	}
	
	categor_num_max = max(categor_num)
	
	#the fighting and learning dynamics: 
	wins = list() # keep track of qualities of members involved in wins and losses for each animal
	losses = list()
	for(i in 1:N){ 
		wins[[i]] = numeric()
		losses[[i]] = numeric()
	}
	
	last_fights_cat = array(Inf, dim=c(N,categor_num_max)) #last time each individual thought it encountered each category
	last_fights_ind = array(Inf, dim=c(N,N)) #last time each individual thought it encountered each category
	
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
		
		#learning about the signal of one's opponent:
			draw = matrix(runif(N*N,0,1),nrow=N) #random numbers to generate confusion events
			perc_cats = array(NA, dim = c(N,N)) #each individual's perception of every other's category
			for(i in 1:N){
				perc_cats[i,] = rowSums(matrix(rep(draw[i,],categor_num[i]),ncol=categor_num[i])>confus_mat[[i]])+1 #use confus_mat to see which category perceptions get switched to
			}
				# perc_cats = matrix(rep(sig_cats,N),nrow=N,byrow=TRUE) #no switching categories
			
			last_fights_cat[pair[1],perc_cats[pair[1],pair[2]]] = 0 #each animal thinks it just fought with the category it perceived
			last_fights_cat[pair[2],perc_cats[pair[2],pair[1]]] = 0
			new_as_cat_bycat = a_cat_bycat[,,t] #if you weren't involved in the fight your assessment doesn't change
			new_as_cat_bycat[last_fights_cat>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago
			a_cat_bycat[,,t+1] = new_as_cat_bycat
			a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]],t+1] = update(a_cat_bycat[pair[1],perc_cats[pair[1],pair[2]],t],qual_vals[pair[2]]) #each animal updates its assessment of the category it perceives based on the quality it experiences
			a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]],t+1] = update(a_cat_bycat[pair[2],perc_cats[pair[2],pair[1]],t],qual_vals[pair[1]])
			
			new_as_cat_byind = array(NA,dim=c(N,N))
			for(i in 1:N){
				new_as_cat_byind[i,]=a_cat_bycat[i,perc_cats[i,],t+1] #each animal assigns quality values to individuals based on its sloppy assignment of individuals to categories
			}
			a_cat_byind[,,t+1] = new_as_cat_byind
			
			last_fights_cat = last_fights_cat+1
		
		#learning about the identity of one's opponent:
			draw = runif(2,0,1) #random numbers to generate confusion events
			perc_pair = pair  
			if(draw[1]<confus_prob_ind){
				perc_pair[1] = sample(setdiff(1:N,pair[1]),1) #with low probability draw a different individual that the focal thinks it's interacting with
			}
			if(draw[2]<confus_prob_ind){
				perc_pair[2] = sample(setdiff(1:N,pair[2]),1)
			}
			
			last_fights_ind[pair[1],perc_pair[2]] = 0
			last_fights_ind[pair[2],perc_pair[1]] = 0
			new_as_ind = a_ind[,,t] #if you weren't involved in the fight your assessment doesn't change
			new_as_ind[last_fights_ind>memory_window] = NA #if you fought more than memory_window fights ago you forget your assessments
			a_ind[,,t+1] = new_as_ind
			a_ind[pair[1],perc_pair[2],t+1] = update(a_ind[pair[1],perc_pair[2],t],qual_vals[pair[2]]) #each animal updates its assessment of the individual it perceives based on the quality it experiences
			a_ind[pair[2],perc_pair[1],t+1] = update(a_ind[pair[2],perc_pair[1],t],qual_vals[pair[1]])
			last_fights_ind = last_fights_ind+1
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
