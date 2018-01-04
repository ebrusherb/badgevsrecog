source('sub2ind.R')

# group size, category width, memory window, rho
parameters = data.frame(w=c(),N=c(),rho=c(),delta=c(),l=c())

N_vals = c(100)
rho_vals = seq(0.9,by=0.2)
wind_vals = c(seq(500,2500,by=500),Inf,5000)
wind_vals = 2500
delta_vals = c(seq(0,1,by=0.25))
delta_vals = c(0,0.1)
learn_rate_vals = c(0.05,0.1,0.25,0.5)
learn_rate_vals=c(0.5)

xN = length(N_vals)
xrho = length(rho_vals)
xwind = length(wind_vals)
xdelta = length(delta_vals)
xlearn = length(learn_rate_vals)

for(l in 1:xrho){
	for(j in 1:xdelta){		
		for(m in 1:xlearn){
			for(i in 1:xN){
				for(k in 1:xwind){
					ind = sub2ind(c(xN,xdelta,xrho,xlearn,xwind),c(i,j,l,m,k))
					parameters = rbind(parameters,data.frame(w=wind_vals[k],N=N_vals[i],rho=rho_vals[l],delta=delta_vals[j],l=learn_rate_vals[m]))
				}
			}
		}
	}
}

# 
breaks = seq(1,dim(parameters)[1],by=dim(parameters)[1])
num_breaks = length(breaks)
chunk = list()
if(num_breaks>1){
	for(i in 1:(num_breaks-1)){
		chunk[[i]] = breaks[i]:(breaks[i+1]-1)
	}
}
chunk[[num_breaks]] = breaks[num_breaks]:dim(parameters)[1]
