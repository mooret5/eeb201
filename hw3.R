#part 1

snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld-1.txt',header=TRUE)

compute_chisquare=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)
}

compute_chisquare_2=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  #here we use the built-in function for the chi-sq distribution:
  exp_pvals=c((1-freq)^2,2*freq*(1-freq),freq^2) #note, here we don't multiply by n
  chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
  return(chisq)
}


chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)

pvals=pchisq(chisqs,1,lower.tail=FALSE)

signifthres<-0.05
sum(pvals<signifthres) 
mean(pvals<signifthres) 

signifthres<-0.01
sum(pvals<signifthres) 
mean(pvals<signifthres) 

signifthres<-0.001
sum(pvals<signifthres) 
mean(pvals<signifthres) 

length(pvals)
num_pval <- length(pvals)
sort_pvals <- sort(pvals)
log_sort_pvals <- -log10(sort_pvals)
log_exp_pvals <- -log10(pvals)
plot(log_exp_pvals, log_sort_pvals)

#part 2

zz=read.table("pheno.sim.2014-1.txt",header=TRUE)

plot(zz,col=2,lwd=4,xlab="Value",xlim=c(-15,15),main="Blood Glucose")
abline(v=quantile(0.25),lty=2,lwd=3,col=4) #puts a vertical line onto the plot
abline(v=quantile(0.75),lty=2,lwd=3,col=5)



case_genotypes=snps["rs7584086_T",])
control_genotypes=factor(snps["rs7584086_T",])
m=lm(case_genotypes)
summary(m)

m1=2m1(case_genotypes)
summary(m1)

m2=0m2(case_genotypes)
summary(m2)

t=lt(control_genotypes)
summary(t)

t1=2t1(control_genotypes)
summary(t1)

t2=0t2(control_genotypes)
summary(t2)

