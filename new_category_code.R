N=25
qual_vals = rnorm(N, mean = 0, sd = 1) 
sig_vals = rnorm(N, mean = 0, 1)
sig_vals = fixCorr(qual_vals,sig_vals,sig_qual_corr)
sig_cats_temp = array(NA, dim = N)

perc_wind = 1

left = 1:N
cat_now = 1
cat_med = array(NA,dim=0)
while(length(left)>0){
	if(length(left)>1){
	first = sample(left,size=1)} else{ first = left}
	to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
	if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0){
				to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
				print('problem fixed')
			}
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


setpal = brewer.pal(9,'Set1')
ind = (1:max(sig_cats))%%9
ind[ind==0] =9
palnow = setpal[ind]

o = order(sig_vals)

sig_vals = sig_vals[o]
sig_cats = as.factor(sig_cats[o])
ind = 1:N
categories = data.frame(ind,sig_vals, sig_cats)

confus_mat = array(0, dim=c(N,catnum)) 
for(j in 1:N){
	confus_mat[j,]=(exp(-confus_prob_cat*abs(sig_vals[j]-cat_med))/sum(exp(-confus_prob_cat*abs(sig_vals[j]-cat_med)))) 
}

# plot(sig_vals,col=palnow,ylab='Signal',xlab='',pch=20)
# for(i in 1:length(cat_med)){abline(h=cat_med[i],col=palnow[i])}

plot_cats = ggplot(categories, aes(x = ind, y = sig_vals, colour = sig_cats)) + 
		geom_point()  +
		theme_bw() +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.title=element_text(size=10),legend.position="none") + 
		scale_color_manual(values=palnow[1:catnum])+	 
		xlab("")+ ylab("Signal") + geom_hline(yintercept = cat_med, col = palnow[1:catnum])
		
# pdf(file='/Users/eleanorbrush/Desktop/categories.pdf',width=3.4,height=3.4)
print(plot_cats)
# dev.off()


# # ##--- how many categories?

# N_vals = c(25,50,100,200,400)
# xN = length(N_vals)
# perc_vals = seq(1.2,0,by=-0.2)
# xp = length(perc_vals)
# corr_vals = c(0.3,0.5,0.9)
# xc = length(corr_vals)

# cat_mean = array(NA,dim=c(xN,xp,xc))
# cat_sd = array(NA,dim=c(xN,xp,xc))

# runs = 150

# for(n in 1:xN){
	# for(p in 1:xp){
		# for(c2 in 1:xc){
			# N = N_vals[n]
			# perc_wind = perc_vals[p]
			# sig_qual_corr = corr_vals[c2]
			# hold_catnum = array(NA,dim=runs)
			# for(r in 1:runs){
				# qual_vals = rnorm(N, mean = 0, sd = 1) 
				# sig_vals = rnorm(N, mean = 0, 1)
				# sig_vals = fixCorr(qual_vals,sig_vals,sig_qual_corr)
				# sig_cats_temp = array(NA, dim = N)
				# left = 1:N
					# cat_now = 1
					# cat_med = array(NA,dim=0)
					# while(length(left)>0){
						# if(length(left)>1){
						# first = sample(left,size=1)} else{ first = left}
						# to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
						# sig_cats_temp[left[to_categorize]] = cat_now
						# cat_now = cat_now+1
						# cat_med = c(cat_med,mean(sig_vals[left[to_categorize]]))
						# left = left[-to_categorize]
					# }
				# hold_catnum[r] = max(sig_cats_temp)
			# }
		# cat_mean[n,p,c2] = mean(hold_catnum)
		# cat_sd[n,p,c2] = sd(hold_catnum)	
		# }
	# }
# }

# cat_mean = melt(cat_mean,varnames=c('groupsize','percwind','corr'),value.name='catnum')
# cat_mean$groupsize = as.factor(rep(N_vals,xp*xc))
# cat_mean$percwind = rep(perc_vals,each=xN,times=xc)
# cat_mean$corr = as.factor(rep(corr_vals,each=xN*xp))
# cat_mean$catnum = log(cat_mean$catnum)
# cat_sd = melt(cat_sd,varnames=c('groupsize','percwind','corr'),value.name='sd')
# cat_sd$groupsize = as.factor(rep(N_vals,xp*xc))
# cat_sd$percwind = rep(perc_vals,each=xN,times=xc)
# cat_sd$corr = as.factor(rep(corr_vals,each=xN*xp))
# cat_sd$sd = log(cat_sd$sd)

# plot_catnum = ggplot(cat_mean, aes(x=percwind,y=catnum, colour = groupsize, linetype = corr)) + 	
	# geom_line() + geom_point() +
	# theme_bw() + scale_y_continuous(breaks=log(c(3,6,12,N_vals)),labels=c(3,6,12,N_vals)) +
		# theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ) + 
		# scale_color_manual(values=mypal[1:xN]) + 
		# labs(linetype="Corr", colour="Group size")  + xlab(expression(paste("Perception window, ",delta,sep='')))+ylab("Number of categories")

# # pdf(file='/Users/eleanorbrush/Desktop/number_of_categories.pdf',width=5,height=3.4)
# print(plot_catnum)
# # dev.off()



