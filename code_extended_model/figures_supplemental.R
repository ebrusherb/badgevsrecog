#####  how categorization works
N=100
qual_vals = rnorm(N, mean = 0, sd = 1) 
sig_vals = rnorm(N, mean = 0, 1)
sig_vals = fixCorr(qual_vals,sig_vals,sig_qual_corr)
sig_cats_temp = array(NA, dim = N)

perc_wind = 0.5
confus_prob_cat = Inf

left = 1:N
cat_now = 1
cat_med = array(NA,dim=0)
while(length(left)>0){
	if(length(left)>1){
	first = sample(left,size=1)} else{ first = left}
	to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
	if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf ){
				to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
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
ind = (1:catnum)%%9
ind[ind==0] =9
palnow = setpal[ind]

o = order(sig_vals)

sig_vals = sig_vals[o]
sig_cats = as.factor(sig_cats[o])
ind = 1:N
# categories = data.frame(ind,sig_vals, sig_cats)
categories = data.frame(sig_vals=c(sig_vals,sig_vals),sig_cats=c(sig_vals,cat_med[sig_cats]),delta=c(rep(1,N),rep(0,N)))

perc_wind = 2
confus_prob_cat = Inf

left = 1:N
cat_now = 1
cat_med = array(NA,dim=0)
while(length(left)>0){
	if(length(left)>1){
	first = sample(left,size=1)} else{ first = left}
	to_categorize = which(abs(sig_vals[left]-sig_vals[first])<=perc_wind/2)
	if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf ){
				to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
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

categories = rbind(categories,data.frame(sig_vals=c(sig_vals),sig_cats=c(cat_med[sig_cats]),delta=c(rep(-1,N))))
categories$colors =(round(1/2*(categories$sig_cats+1),2)*100)
num_cols=diff(range(categories$colors))+1
offset = 5
reds = colorRampPalette(brewer.pal(9,'Reds'))(num_cols+offset)
reds = reds[(1:num_cols)+offset]
reds = reds[sort(unique(categories$colors-min(categories$colors)+1))]
categories$colors=as.factor(categories$colors)

m=min(-1,min(sig_vals))
M=max(1,max(sig_vals))

plot_cats = ggplot(categories, aes(y = delta, x = sig_vals, colour = colors)) + 
		geom_point(aes(size=sig_cats))  +
		theme_bw() +
		theme(
        	axis.ticks.y=element_blank(),
        	axis.text.y=element_blank(), axis.line.y=element_blank(),text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ,plot.title=element_text(size=10),legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.margin=unit(c(1,1,1,1),"cm"),panel.border=element_blank()) +		scale_color_manual(values=reds)+	scale_x_continuous(limit=c(m,M))+
		xlab("Signal")+ylab('')
		
pdf(file=paste(wd,'/categories.pdf',sep=''),width=6,height=4)
print(plot_cats)
dev.off()

#### how many categories?

N_vals = c(20,40,80,100)
xN = length(N_vals)
perc_vals = seq(2,0,by=-0.2)
xp = length(perc_vals)
corr_vals = c(0.5,0.9)
xc = length(corr_vals)

ybreaks = c(2,5,20,80)

cat_mean = array(NA,dim=c(xN,xp,xc))
cat_sd = array(NA,dim=c(xN,xp,xc))

runs = 100

for(n in 1:xN){
	for(p in 1:xp){
		for(c2 in 1:xc){
			N = N_vals[n]
			perc_wind = perc_vals[p]
			sig_qual_corr = corr_vals[c2]
			hold_catnum = array(NA,dim=runs)
			for(r in 1:runs){
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
					if(length(which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0))>0 && confus_prob_cat!=Inf ){
								to_categorize = to_categorize[-which(exp(-confus_prob_cat*abs(sig_vals[left[to_categorize]]-median(sig_vals[left[to_categorize]])))==0)]
							}
					sig_cats_temp[left[to_categorize]] = cat_now
					cat_now = cat_now+1
					cat_med = c(cat_med,mean(sig_vals[left[to_categorize]]))
					left = left[-to_categorize]
				}
				hold_catnum[r] = max(sig_cats_temp)
			}
		cat_mean[n,p,c2] = mean(hold_catnum)
		cat_sd[n,p,c2] = sd(hold_catnum)	
		}
	}
}

cat_mean = melt(cat_mean,varnames=c('groupsize','percwind','corr'))
names(cat_mean)[4]='catnum'
cat_mean$groupsize = as.factor(rep(N_vals,xp*xc))
cat_mean$percwind = rep(perc_vals,each=xN,times=xc)
cat_mean$corr = as.factor(rep(corr_vals,each=xN*xp))
cat_mean$catnum = log(cat_mean$catnum)
cat_sd = melt(cat_sd,varnames=c('groupsize','percwind','corr'),value.name='sd')
names(cat_sd)[4]='sd'
cat_sd$groupsize = as.factor(rep(N_vals,xp*xc))
cat_sd$percwind = rep(perc_vals,each=xN,times=xc)
cat_sd$corr = as.factor(rep(corr_vals,each=xN*xp))
cat_sd$sd = log(cat_sd$sd)

plot_catnum = ggplot(cat_mean, aes(x=percwind,y=catnum, colour = groupsize, linetype = corr)) + 	
	geom_line() + geom_point() +
	theme_bw() + scale_y_continuous(limits=log(c(2,100)),breaks=log(ybreaks),labels=ybreaks) +
		theme(text=element_text(family="Helvetica", size=10), plot.title=element_text(size=10) ) + 
		scale_color_manual(values=mypal[1:xN]) + 
		labs(linetype="Corr", colour="Group size")  + xlab('Category width')+ylab("Number of categories")

pdf(file=paste(wd,'/number_of_categories.pdf',sep=''),width=5,height=3.4)
print(plot_catnum)
dev.off()

