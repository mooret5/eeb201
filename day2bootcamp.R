s1=rnorm(1000,mean=0,sd=1)
s3=rnorm(1000,mean=0,sd=3)
head(s1)
head(s3)
bins=seq(-15,15,by=1)
hist(s1,breaks=bins)$breaks
hist(s3,breaks=bins)$breaks
counts_s1<-hist(s1,breaks=bins)$counts
counts_s3<-hist(s3,breaks=bins)$counts
par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) 
barplot(rbind(counts_s1,counts_s3),col=c(1,5),beside=T,names.arg=seq(-15,14.5,by=1),xlab="Value",ylab="Count")
legend(6,350,c(expression(paste(sigma,"=1")),expression(paste(sigma,"=3"))),col=c(1,5),lwd=4, bty="n")
