if(is.element('vals',ls())){
	rm(vals)}
vals=ls(pattern='_vals')
x=list()
for(i in 1:length(vals)){
	x[[i]]=get(vals[i])
}
x[[i+1]]=Tfights
x=setNames(x,c(vals,'Tfights'))
print(x)