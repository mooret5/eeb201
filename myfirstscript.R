#This is my scratch script for practicing R
library(ape)
library(geiger)
library(ggplot2)

#trying in class exercise
cat("hello world")
#use stackoverflow for help
tips <- c("avoid excel", "use a reference manager", "learn a programing language")
tips
source("source.example.R")
all.I.know.about.life.I.learned.in.grad.school()
#read in tree
tt <- read.tree("tree.tre")
#structure of data set
str(tt)
#labels of data set
tt$tip.label
#beginning of labels
head(tt$tip.label)
#read in data file, headerline, leave characters as is, seperate data
dd <- read.table("data.txt", header = T, as.is = T, sep = "\t")
#info about file
attributes(dd)
head(dd)
dim(dd)
#get info from table
dim(dd)[1]
dim(dd)[2]
#change length of table
dflength <- dim(dd)[1]
runif(1)
?runif
#generate some random data
size <- runif(dflength)
newdd <- cbind(dd,size)
head(cbind(dd, size))
head(newdd)
head(dd$species)
newdd[1,]
#comparison, asking if true or nah
1 == 2
1 == 1
xx = 100
xx <- 101
xx
102 -> xx
xx
#create variables, can't start with numbers
a1 = 12
a1
dd$mode == "MPF"
which(dd$mode == "MPF")
just_mpf <- newdd[which(dd$mode == "MPF"),] #assign just MPF fish to table
head(just_mpf)
newdd[1:10,2] #rows 1-10, column 2
newdd[,1] #all rows. column 1
newdd[13,] #row 13, all columns
length(tt$tip.label)
#control statements
#for loops, need range of numbers, and function
for (ii in 1:5){cat("\nthe number is", ii)}
notfish <- c("bat", "dolphin", "toad", "soldier")
for(animal in notfish){cat(animal, "fish", " are tasty\n", sep="")}
#while statements (some condition is true) {do something}
xx <- 1
while(xx < 10){
 cat("value of xx", xx, "\n") 
 xx <- xx+1; 
 if (xx == 7) {
 cat(" lucky number ", xx, "\n") }
 else{ 
 cat ("not excited about the number ", xx, "\n)}
}
print(xx)
#Pseudo-code, model examples
#N(t+1)=R*N(t)
#should use a for loop for this
#set N=100
#set R=1.05
#loop over 10 years
#each yeah multiply N by R to update population size
#show final value of N
#initialize a vector to hold all values through N time, set first element to 100
#store the new value in the next empty spot
#plot N vector against time

#geometric model example
#initial conditions
N=100
R=1.05
T=10
#list of spaces to store values
NN=matrix(NA, nrow=1, ncol=T+1)
NN[1]=N
#making for loop
for(T in 1:10) {NN[T+1]=R*NN[T]}
#plotting loop
plot(1:11,NN, xlab='time', ylab= 'N', col='red')
#making function of loop data
geomFun= function(R, N, Tmax){NN=matrix(NA, nrow=1, ncol=Tmax+1)
NN[1]=N 
for(T in 1:Tmax) {NN[T+1]=R*NN[T]}
 plot(1:(Tmax+1),NN, xlab='time', ylab= 'N', col='red')
}

#discreet model example
#initial conditions
N=100
R=1.05
Tmax=10
K=100
#list of spaces to store values
NN=matrix(NA, nrow=1, ncol=T+1)
NN[1]=N
#making for loop
for(T in 1:Tmax) {NN[T+1]=NN[T]*(1+R*(1-NN[T]/K))}
#plotting loop
plot(1:(Tmax+1),NN, xlab='time', ylab= 'N', col='red')

#making function of loop data
geomFun= function(R, N, Tmax){NN=matrix(NA, nrow=1, ncol=Tmax+1)
NN[1]=N 
for(T in 1:Tmax) {NN[T+1]=NN(T)(1+R(1-NN[T]/K))}
plot(1:(Tmax+1),NN, xlab='time', ylab= 'N', col='red')
}



