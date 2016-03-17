######
#functions: 

dominance = 0.5 #how quickly the probability switches from A winning to A losing
win_prob <- function(q){ #probability of A winning given qualities of A and B
	d = q[1]-q[2] #differences in qualities
	p = exp(dominance*d)/(exp(dominance*d)+1) #a sigmoidal function that gives probability of A winning 
	return(p)
}

learn_rate = 0.1
learn_noise = 0.005
update <- function(a, q){ # given current assessment a and true quality q, update to new assessment
	if(!is.na(a)){
			anew =(1- learn_rate) * a +learn_rate*q + rnorm(1, mean =0, sd = learn_noise)
		} else{
			anew = learn_rate*q + rnorm(1, mean =0, sd = learn_noise)
		}
	return(anew)
}

correlatedValue = function(x, r){
  r2 = r**2
  ve = 1-r2
  SD = sqrt(ve)
  e  = rnorm(length(x), mean=0, sd=SD)
  y  = r*x + e
  return(y)
}


#######
#parameters to play with:
Tfights = 3000 #total number of fights 
N = 50 # individuals
categor_num = 3
memory_window = Inf
confus_prob_cat = 0
confus_prob_ind = 0.8
sim_runs = 5

N_vals = c(20,50)
xN = length(N_vals)
cat_vals = c(3,10,20)
cat_vals = c(3,10)
xcat = length(cat_vals)

learning_curves_cat = array(NA, dim=c(xN,xcat,sim_runs,Tfights+1))
learning_curves_ind = array(NA, dim=c(xN,xcat,sim_runs,Tfights+1))
learning_times_cat = array(NA, dim=c(xN,xcat))
learning_times_ind = array(NA, dim=c(xN,xcat))

for(n in 1:xN){
	N = N_vals[n]
	for(c in 1:xcat){
		categor_num = cat_vals[c]
		learning_curves_cat_temp = array(NA, dim=c(sim_runs,Tfights+1,N))
		learning_curves_ind_temp = array(NA,dim=c(sim_runs,Tfights+1,N))
		
		learning_time_cat_temp = array(NA, dim=c(sim_runs,N))
		learning_time_ind_temp = array(NA, dim=c(sim_runs,N))
		for(run in 1:sim_runs){
			
			######
			#set up group with quality values and signal values: 
			
			qual_vals = array(data = NA, dim = N) # quality values
			# #option: one animal in each of N bins
			# qual_vals = seq(0, 1, by = 1/N) 
			# #option: uniformly distributed quality values
			# qual_vals = runif(N , min = 0, max = 1) 
			# #option: lognormally distributed quality values
			# qual_sd = 0.5; qual_vals = exp(rnorm(N, mean = 0, sd = qual_sd)) 
			#option: normally distributed quality values
			qual_sd = 0.5; qual_vals = rnorm(N, mean = 0.5, sd = qual_sd) 
			
			#matrix of columns of quality values
			qual_mat = matrix(rep(qual_vals,N), ncol=N, byrow=TRUE) 
			
			sig_vals = array(data = NA, dim = N) # signal values
			#option: signals are correlated with quality 
			sig_qual_corr_sd = 0.5; sig_vals = qual_vals + rnorm(N, mean = 0, sd =sig_qual_corr_sd) 
			# #option: uniformly distributed sigals
			# sig_vals = runif(N, min =0, max = 1) 
			# #option: lognormally distributed signals: 
			# sig_sd = 0.5; sig_vals = exp(rnorm(N, mean =0, sd = sig_sd)) 
			# #option: normally distributed signal values
			# sig_sd = 0.5; sig_vals = rnorm(N, mean =0, sd =sig_sd) 
			
			sig_min = -2 
			sig_max = 2
			categor_breaks = sig_min+1:(categor_num-1)*(sig_max-sig_min)/categor_num
			sig_cats = array(data = NA, dim =N) #which category each animal is in
			for(i in 1:N){
				sig_cats[i] = sum(sig_vals[i] > categor_breaks)+1 
			}
			
			confus_mat = array(0, dim=c(N,categor_num))
			for(i in 1:N){
				if(sig_cats[i]==1){
					confus_mat[i,2] = confus_prob_cat*exp(-abs(sig_vals[i]-categor_breaks[1]))
					confus_mat[i,1] = 1-confus_mat[i,2]
				} else if(sig_cats[i]==categor_num){
					confus_mat[i,categor_num-1] = confus_prob_cat*exp(-abs(sig_vals[i]-categor_breaks[categor_num-1]))
					confus_mat[i,categor_num] = 1-confus_mat[i,categor_num-1]
				} else{
					confus_mat[i,sig_cats[i]-1] = confus_prob_cat*exp(-abs(sig_vals[i]-categor_breaks[sig_cats[i]-1]))
					confus_mat[i,sig_cats[i]+1] = confus_prob_cat*exp(-abs(sig_vals[i]-categor_breaks[sig_cats[i]]))
					confus_mat[i,sig_cats[i]] = 1-confus_mat[i,sig_cats[i]-1] - confus_mat[i,sig_cats[i]+1]
					}
			}
			confus_mat = t(apply(confus_mat,1,cumsum))
			
			##########
			#the fighting and learning dynamics: 
			wins = list() # keep track of qualities of members involved in wins and losses for each animal
			losses = list()
			for(i in 1:N){ 
				wins[[i]] = numeric()
				losses[[i]] = numeric()
			}
			num_fights = array(0,dim=N)
			last_fights_cat = array(Inf, dim=c(N,categor_num))
			last_fights_ind = array(Inf, dim=c(N,N))
			
			a_cat_byind = array(NA, dim=c(N,N,Tfights+1))
			a_cat_bycat = array(NA, dim=c(N,categor_num,Tfights+1))
			a_ind = array(NA, dim=c(N,N,Tfights+1))
			
			for(t in 1:Tfights){
				pair = sample(1:N, 2, replace = FALSE) #draw two animals at random
				num_fights[pair] = num_fights[pair]+1
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
						perc_cats[i,] = rowSums(matrix(rep(draw[i,],categor_num),ncol=categor_num)>confus_mat)+1 #use confus_mat to see which category perceptions get switched to
					}
						# perc_cats = matrix(rep(sig_cats,N),nrow=N,byrow=TRUE)
				
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
					
					# a_cat_byind[,,t+1] = a_cat_bycat[,sig_cats,t+1]
					
					# new_as_cat_byind = a_cat_byind[,,t] #if you weren't involved in the fight your assessment doesn't change
					# new_as_cat_byind[last_fights_cat[,sig_cats]>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago
					# a_cat_byind[,,t+1] = new_as_cat_byind
					# a_cat_byind[pair[1],sig_cats==perc_cats[2],t+1] = update(a_cat_byind[pair[1],pair[2],t],qual_vals[pair[2]]) #opponents assess each other
					# a_cat_byind[pair[2],sig_cats==perc_cats[1],t+1] = update(a_cat_byind[pair[2],pair[1],t],qual_vals[pair[1]])
					
					last_fights_cat = last_fights_cat+1
				
				#learning about the identity of one's opponent:
					draw = runif(2,0,1)
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
					new_as_ind[last_fights_ind>memory_window] = NA #you forget your assessments of the categories you fought more than memory_window fights ago

					a_ind[,,t+1] = new_as_ind
					a_ind[pair[1],perc_pair[2],t+1] = update(a_ind[pair[1],perc_pair[2],t],qual_vals[pair[2]]) #each animal updates its assessment of the individual it perceives based on the quality it experiences
					a_ind[pair[2],perc_pair[1],t+1] = update(a_ind[pair[2],perc_pair[1],t],qual_vals[pair[1]])
					last_fights_ind = last_fights_ind+1
			}
			
			#######
			#how well did they learn over time?
			error_cat = array(NA, dim = c(N,Tfights+1))
			error_ind = array(NA, dim = c(N,Tfights+1))
			#how quickly did they get close to their final assessment?
			error_threshold = 0.5
			time_cat = array(NA, dim=N)
			time_ind = array(NA, dim=N)
			#what was everyone else's opinion of me over time?
			highrank_cat = array(NA, dim=c(N,Tfights+1))
			highrank_ind = array(NA, dim=c(N,Tfights+1))
			#costs of losing? while fights are random, these don't depend on the recognition system, but we'll need two if we consider strategic fights.
			losing_costs_cat = array(NA, dim=c(N,Tfights+1))
			losing_costs_ind = array(NA, dim=c(N,Tfights+1))
			for(i in 1:N){
				for(t in 1:(Tfights+1)){
			
					if(sum(!is.na(a_cat_byind[i,,t]))>=1){
						error_cat[i,t] = sum(abs(a_cat_byind[i,,t]-qual_vals),na.rm=TRUE) /sum(!is.na(a_cat_byind[i,,t]))
						}
					if(sum(!is.na(a_ind[i,,t]))>=1){
						error_ind[i,t] = sum(abs(a_ind[i,,t]-qual_vals),na.rm=TRUE) /sum(!is.na(a_ind[i,,t]))
						}
					if(sum(!is.na(a_cat_byind[,i,t]))>=1){
					highrank_cat[i,t] = sum(a_cat_byind[,i,t],na.rm=TRUE) /sum(!is.na(a_cat_byind[,i,t]))
						}
					if(sum(!is.na(a_ind[,i,t]))>=1){
						highrank_ind[i,t] = sum(a_ind[,i,t],na.rm=TRUE) /sum(!is.na(a_ind[,i,t]))
						}
				}
				time_cat_temp = array(NA,dim=N)
				time_ind_temp = array(NA, dim=N)
				for(j in setdiff(1:N,i)){
					v = which(abs(a_cat_byind[i,j,]-a_cat_byind[i,j,Tfights+1])<= error_threshold)
					if(length(v)>0){
						time_cat_temp[j] = v[1]-1
					} else{ time_cat_temp[j] = Tfights}
					v = which(abs(a_ind[i,j,]-a_ind[i,j,Tfights+1])<= error_threshold)
					if(length(v)>0){
						time_ind_temp[j] = v[1]-1
					} else{ time_ind_temp[j] = Tfights}
				}
				time_cat[i] = median(time_cat_temp,na.rm=TRUE)
				time_ind[i] = median(time_ind_temp,na.rm=TRUE)
			}
		learning_curves_cat_temp[run,,] = t(error_cat)
		learning_curves_ind_temp[run,,] = t(error_ind)
		
		learning_time_cat_temp[run,] = time_cat
		learning_time_ind_temp[run,] = time_ind
		}
		
		learning_curves_cat[n,c,,] = rowMeans(learning_curves_cat_temp,dims=2,na.rm=TRUE)
		learning_curves_ind[n,c,,] = rowMeans(learning_curves_ind_temp,dims=2,na.rm=TRUE)
		
		learning_times_cat[n,c] =  mean(learning_time_cat_temp)
		learning_times_ind[n,c] = mean(learning_time_ind_temp)
	}
}

######
quartz()

par(mfrow=c(xcat,xN))

for(n in 1:xN){
	for(c in 1:xcat){
		plot(learning_curves_cat[n,c,1,],type='l',ylim=c(0,1),xlab='Fights',ylab='Error',main=c('N=',N_vals[n],'Cats=',cat_vals[c]))
		lines(learning_curves_ind[n,c,2,],type='l',ylim=c(0,1),col='red')
		for(run in 2:sim_runs){
			lines(learning_curves_cat[n,c,run,])
			lines(learning_curves_ind[n,c,run,],col='red')
		}
	}
}

quartz()

par(mfrow=c(1,2))

plot(N_vals,learning_times_cat[,1],type='o',ylim=c(0,1000),xlab='Group Size',ylab='Mean Learning Time')
points(N_vals,learning_times_ind[,1],type='o',col='red')

plot(cat_vals,learning_times_cat[1,],type='o',ylim=c(0,200),xlab='Number of Categories',ylab='Mean Learning Time')
points(cat_vals,learning_times_ind[1,],type='o',col='red',ylim=c(0,200))



