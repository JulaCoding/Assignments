# Loading necessary packages
library(datasets)
library(ggplot2)
library(knitr)
library(dplyr)
# Part 1: Simulation exercise
## Central Limit Theorem and Exponential Distribution Comparison
lambda<- 0.2
n<- 40
# Generate 1000 simulations of 40-size samples of EXPD
simData<- NULL
simMeans<- NULL
simSD<- NULL
for(i in 1:1000){
          set.seed(i)
          simData<- c(simData, rexp(n, lambda))
          simMeans<- c(simMeans, mean(rexp(n, lambda)))
          simSD<- c(simSD, sd(rexp(n, lambda)))
}
# Comparing simulated mean and theoretical mean
sampleMean<- mean(simMeans)
sampleMean
theoreticalMean<- 1/lambda
theoreticalMean
#Histogram plotting
hist(simMeans, main = "Figure1. Comparison of Sample Meand and Theoretical Mean", xlab="Mean")
abline(v= sampleMean, lw = 4, col= "red")
abline(v= theoreticalMean, lw = 4, col = "green")
# Conclusion : The sample mean is 5.011 whereas the theoretical mean is 5. The center of distribution of averages of 40 exponential is very close to the theoretical center of the distribution

# Comparing simulated variance to theoretical
sampleVar<- var(simMeans)
sampleVar
theoreticalSD<- (1/lambda)/sqrt(n) 
theoreticalSD
theoreticalVAr<- theoreticalSD^2
theoreticalVAr
