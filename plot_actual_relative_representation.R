plot_actual_relative_representation <- function(counts_per_yr,words, normalize=TRUE){ 

#we may plot up to 5 words
count=length(words);
if(count>5){
print('only operating on first 5 words given');
count=5
}

#read in table of counts per year
Table <- read.table(counts_per_yr,sep=' ',header=T,check.names=F);

#prep yrs data
yrs=colnames(Table);
yrs= yrs[3:length(yrs)];
yrsnum=as.numeric(yrs);
yrs_cnt=length(yrs);

#prep some variables
colors=c('red','blue','green','purple','orange');
colors=colors[1:count]
title=c("Ground truth for", words);
data<-array(dim=c(count, yrs_cnt));

#select required data from table, put into an array
for(i in c(1:count)){a=subset(Table, Table$WORD==words[[i]]);
a=unlist(a[3:length(a)]);
data[i,]=a;
}

#normalize if requested
if(normalize){for(i in c(1:yrs_cnt)){data[,i]=data[,i]/sum(data[,i])}}

#create the plot
if(normalize){
plot(0,type="n",xlim=c(min(yrsnum),max(yrsnum)), col='blue', ylim=c(0,1), ylab=c('relative frequencies'), xlab="Years", xaxt="n", main=title);
}
else{
plot(0,type="n",xlim=c(min(yrsnum),max(yrsnum)), col='blue', ylim=c(0,max(data)), ylab=c('occurences'), xlab="Years", xaxt="n", main=title);
}
axis(1, at=seq(min(yrsnum),max(yrsnum),by=10), labels=TRUE);

#plot the data
for(d in c(1:count)){
Line=data[d,];
lines(yrsnum,Line, type='l', col=colors[d]);
}

legend(legend=words, x="bottomright", col=colors, lty = c(1, 1));
}