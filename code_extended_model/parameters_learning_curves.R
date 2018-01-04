source('sub2ind.R')

# group size, category width, memory window, rho
parameters = data.frame(N=array(0,6),rho=array(0,6),w=array(0,6),delta=array(0,6),l=array(0,6))

parameters$N = rep(25,6)
parameters$rho = rep(c(0.9,0.7,0.7),times=2)
parameters$w = rep(c(2500,2500,500),times=2)
parameters$delta = rep(c(0,0.25),each=3)
parameters$l = rep(0.1,6)

xN = length(N_vals)
xrho = length(rho_vals)
xwind = length(wind_vals)
xdelta = length(delta_vals)
xlearn = length(learn_rate_vals)