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
#mini-exercise 3.2.1
for(T in 1:10) {NN[T+1]=R*NN[T]}
#plotting loop
plot(1:11,NN, xlab='time', ylab= 'N', col='red')
#making function of loop data
#mini-exercise 3.2.2
geomFun= function(R, N, Tmax){NN=matrix(NA, nrow=1, ncol=Tmax+1)
NN[1]=N 
for(T in 1:Tmax) {NN[T+1]=R*NN[T]}
 plot(1:(Tmax+1),NN, xlab='time', ylab= 'N', col='red')
}

#discrete model example

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
#mini-exercise 3.3.1
#making function of loop data
discretelosgisticfun= function(R, NN, K){NN=matrix(NA, nrow=1, ncol=Tmax+1)
NN[1]=N 
for(T in 1:Tmax) {NN[T+1]=NN(T)*(1+R*(1-NN[T]/K))}
plot(1:(Tmax+1),NN, xlab='time', ylab= 'N', col='red')
}

#mini-exercise 3.3.2
#mini-exercise 4.1.1
plot(ICVec, nnVec, xlab=’Initial population size’,
ylab=’Population at time=5’, type=’b’, col=’red’)
#4.2.1
for (ii in 1:length(ICVec)) {
init <- ICVec[ii]
logisticOutput <- lsoda(init, tseq, logisticGrowthODE, pars)
nnVec[ii] <- logistic.output[index,2]
 }

#5.2
nnVec <- rep(NA, 1, 1:5(ICVec))
plot(ICVec, nnVec, xlab=’Initial population size’,
ylab=’Population at time=5’, type=’b’, col=’red’)

#6
# source("LV_script.R")
# A script to run a simple 2-dimensional ODE system
# equations are
# dN/dt = r N - c N P
# dP/dt = e c N P - d P
library(deSolve)
# pred-prey cycles
pars <- c(rr = 2, cc = 0.1, ee = 0.1, dd = 0.2); init <- c(200, 20)
tseq <- seq(0, 50, by=0.02)
# define the model equations
# note that yy is now a vector with two elements for the two state variables
lvPredpreyODE <- function(tt, yy, pars) {
derivs <-rep(NA, 2)
derivs[1] <- pars[’rr’] * yy[1] - pars[’cc’] * yy[1] * yy[2]
derivs[2] <- pars[’ee’] * pars[’cc’] * yy[1] * yy[2] - pars[’dd’] * yy[2]
return(list(c(derivs)))
}
lvPredpreyOutput <- lsoda( init, tseq, lvPredpreyODE, pars)
# make array of two plots
par(mfrow=c(1,2))
# plot output as time series
plot(lvPredpreyOutput[,1], lvPredpreyOutput[,2],
col="blue", type="l",
xlab="time", ylab="# of individuals",
ylim = c(0,1.2*max(lvPredpreyOutput[,2])))
   # add a line to the plot with the predators
   points(lvPredpreyOutput[,1], lvPredpreyOutput[,3], col="red", type="l")
   # add a simple legend
   legText = c("Prey", "Predator")
   legend("topright", legText, lty=1, col = c(’blue’,’red’))
   10
   # plot output as a phase plot, in second subplot
   plot(lvPredpreyOutput[,2], lvPredpreyOutput[,3],
        xlab="Prey", ylab="Predators", type="l", col=’black’)
