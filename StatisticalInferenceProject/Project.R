# Loading necessary packages
library(datasets)
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyr)
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
}
simMeans<- apply(simData, 2, mean)
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
sampleSD<- sd(simMeans)
sampleSD
sampleVar<- sampleSD^2
sampleVar
theoreticalSD<- (1/lambda)/sqrt(n) 
theoreticalSD
theoreticalVAr<- theoreticalSD^2
theoreticalVAr
#Table summarising differences in simulated and theoretical 
table_data <- data.frame(
          Variable = c("Mean", "Standard Deviation", "Variance"),
          Simulated = c(sampleMean, sampleSD, sampleVar),
          Theoretical = c(theoreticalMean, theoreticalSD, theoreticalVAr)
)
print(table_data)
#Showing that the distribution is normal
xfit<- seq(min(simMeans), max(simMeans), length=100)
yfit<- dnorm(xfit, mean = 1/lambda, sd= (1/lambda)/sqrt(n))
hist(simMeans, breaks=n, prob = T, main = "Density of means", xlab = "Means", ylab = "Density" )
lines(xfit, yfit, pch = 22, lw=3, lty = 5, col="red")
# Conclusion: Due to the CLT the distribution of averages of 40 exponential is closely similar to a normal distribution.

#Part2: Basic Inferential Data Analysis 
data("ToothGrowth")
dt<- ToothGrowth
#There are three unique values for dose which I will convert to factors
dt$dose<- factor(dt$dose)
str(dt)
ggplot(dt, aes(x=dose, y=len, fill = supp))+
          geom_boxplot()+
          ggtitle("Tooth growth length based on supplement type and dose")+
          theme_classic()
dt %>% 
          group_by(supp, dose)%>%
          summarise(mean = mean(len), .groups = "drop")%>%
          spread(supp, mean) %>%
          mutate(diff = abs(VC-OJ))

# T test hypothesis testing. Null hypothesis there is no significant difference between OJ and VC
alpha<- 0.05
# Filtering data for testing
dose_half<- filter(dt, dose==0.5)
dose_one<- filter(dt, dose==1)
dose_two<- filter(dt, dose==2)
# t-test for 0.5 dose
t.test(len~supp, dose_half)
# t-test for 1 dose
t.test(len~supp, dose_one)
# t-test for 2 dose
t.test(len~supp, dose_two)

# Table summarising 
table_data <- data.frame(
          Dose = c(0.5, 1, 2),
          p_value = c(0.006359, 0.001038, 0.9639),
          Conf.Int = c("1.719057 8.780943", "2.802148 9.057852", "-3.79807 3.63807"),
          Decision = c("Reject Null", "Reject Null", "Do not Reject Null")
)

print(table_data)
