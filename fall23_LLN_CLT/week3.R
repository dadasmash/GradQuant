##week3
#Law of Large Number

#Example: roll a dice for N times
#population: N is infinity
#expected mean of the population : (1+2+3+4+5+6)/6=3.5
#sample mean? 
#LLN: as sample size going to infinity,the sample mean converge to the population mean.
# N=10 (10 observations)
sample(1:6,10,replace=TRUE)

mean(sample(1:6,10,replace=TRUE))
# (1+2+3+4+5+6)/6=3.5


#x<-(sample(1:6,10,replace=TRUE))
#y<-mean(x)


# N=100 (100 observations)
mean(sample(1:6,100,replace=TRUE))


# N=1000 (1000 observations)
mean(sample(1:6,1000,replace=TRUE))

#simulation
avg=rep(0,1)
N=rep(0,1)

for(j in 10:100)
{ x<-mean(sample(1:6,j,replace=TRUE))
avg[j-9]=x
N[j-9]=j
}

print(avg)
print(N)

plot(N,avg,xlab="Sample size", ylab="Sample mean")


for(j in 10:500)
{ x<-mean(sample(1:6,j,replace=TRUE))
avg[j-9]=x
N[j-9]=j
}
plot(N,avg,xlab="Sample size", ylab="Sample mean")

for(j in 10:1000)
{ x<-mean(sample(1:6,j,replace=TRUE))
avg[j-9]=x
N[j-9]=j
}
plot(N,avg,xlab="Sample size", ylab="Sample mean")

for(j in 10:10000)
{ x<-mean(sample(1:6,j,replace=TRUE))
avg[j-9]=x
N[j-9]=j
}
plot(N,avg,xlab="Sample size", ylab="Sample mean")
##https://umatter.github.io/courses/berkstats/Berkstats_LLM_CLT.html



#Weak Law of Large Number and The Central Limit Theorem
#As sample size grows larger, the probabilty that sample mean converge to population mean 
#becomes larger.

#CLT: The central limit theorem (CLT) states that the distribution of sample statistics (e.g. means) 
# approximates a normal distribution 
#as the sample size gets larger, regardless of the population's distribution.

#Sample sizes equal to or greater than 30 are often considered sufficient for the CLT to hold.
#crystal clear explanation:
#https://statisticsbyjim.com/basics/central-limit-theorem/


#Example 1 : suppose the total population of country A is 10000. the mean wage is 10 dollar 
#            and the variance of the wage is 1.
#           Unluckly, this information is not known by economists, they have to do a survey about wage!
#           they hope that the sample could represent the population characteristics (mean) well!


# generate Country A population (wage) (Normal Distributed)
wage.pop <- rnorm(10000, 10, 1)
hist(wage.pop)
#repeat the survey for 100 times
# sample from the population and estimate the mean
#sample size 5,repeat the survey for 5000 times
wage.sample1 <- replicate(expr= mean(sample(x = wage.pop, size = 5)), n = 5000) # expr is expression function.https://www.geeksforgeeks.org/create-an-expression-in-r-programming-expression-function/
wage.sample1
wage.sd1 <- replicate(expr= sd(sample(x = wage.pop, size = 5)), n = 5000) 
#sample size 30,repeat the survey for 1000 times
wage.sample2 <- replicate(expr = mean(sample(x =wage.pop, size = 30)), n = 5000)
wage.sd2 <- replicate(expr= sd(sample(x = wage.pop, size = 30)), n = 5000) 
#sample size 100,repeat the survey for 1000 times
wage.sample3 <- replicate(expr = mean(sample(x =wage.pop, size = 100)), n = 5000)
wage.sd3 <- replicate(expr= sd(sample(x = wage.pop, size = 100)), n = 5000)


#Let's plot!
#population density
plot(density(wage.pop), 
     col = "green", 
     lwd = 2,
     ylim = c(0, 3.5),
     xlab = "Wage",
     main = "Sampling Distributions of Estimators")

# add density estimate for the distribution of the sample mean with size=5 to the plot
lines(density(wage.sample1), 
      col = "steelblue", 
      lwd = 2, 
      bty = "l")

# add density estimate for the distribution of the sample mean with size=30 to the plot
lines(density(wage.sample2), 
      col = "red2", 
      lwd = 2)

# add density estimate for the distribution of the sample mean with size=100 to the plot
lines(density(wage.sample3), 
      col = "orange", 
      lwd = 2)


# add a vertical line at the true parameter
abline(v = 10, lty = 2)


# add a legend
legend("topleft",cex = 0.75,
       legend = c(expression('Wage Population'),
                  '',
                  expression(bar(Wage) ~ sample == 5),
                  '',
                  expression(bar(Wage) ~ sample == 30),
                  '',
                  expression(bar(Wage) ~ sample == 100)
       ), 
       lty = c(2, 1, 1, 1), 
       col = c("green","white", "steelblue","white", "red2", "white", "orange"),
       lwd = 2)





######################################


#How about Standard Deviation 
#https://stats.stackexchange.com/questions/104875/question-about-standard-deviation-and-central-limit-theorem
# add density estimate for the distribution of the sample mean with size=5 to the plot
plot(density(wage.sd1), 
      col = "steelblue", 
      lwd = 2, 
      ylim = c(0, 5.5),
      bty = "l")

# add density estimate for the distribution of the sample mean with size=30 to the plot
lines(density(wage.sd2), 
      col = "red2", 
      lwd = 2)

# add density estimate for the distribution of the sample mean with size=100 to the plot
lines(density(wage.sd3), 
      col = "orange", 
      lwd = 2)


### what if  population is not normally distributed? (e.g. uniform, right/left skewed) ? 
# beta distribution to generate right/left skewed population
wage.pop <- rbeta(10000, 1, 5)






######################################


#When population is not normaly distributed 
wage.pop <- rnorm(10000, 10, 1)^5/10000
hist(wage.pop)
mean(wage.pop)

#repeat the survey for 100 times
# sample from the population and estimate the mean
#sample size 5,repeat the survey for 5000 times
wage.sample1 <- replicate(expr= mean(sample(x = wage.pop, size = 5)), n = 5000) # expr is expression function.https://www.geeksforgeeks.org/create-an-expression-in-r-programming-expression-function/
wage.sample1
wage.sd1 <- replicate(expr= sd(sample(x = wage.pop, size = 5)), n = 5000) 
#sample size 30,repeat the survey for 1000 times
wage.sample2 <- replicate(expr = mean(sample(x =wage.pop, size = 30)), n = 5000)
wage.sd2 <- replicate(expr= sd(sample(x = wage.pop, size = 30)), n = 5000) 
#sample size 100,repeat the survey for 1000 times
wage.sample3 <- replicate(expr = mean(sample(x =wage.pop, size = 100)), n = 5000)
wage.sd3 <- replicate(expr= sd(sample(x = wage.pop, size = 100)), n = 5000)


#Let's plot!
#population density
plot(density(wage.pop), 
     col = "green", 
     lwd = 2,
     ylim = c(0, 1),
     xlim=c(0,25),
     xlab = "Wage",
     main = "Sampling Distributions of Estimators")

# add density estimate for the distribution of the sample mean with size=5 to the plot
lines(density(wage.sample1), 
      col = "steelblue", 
      lwd = 2, 
      bty = "l")

# add density estimate for the distribution of the sample mean with size=30 to the plot
lines(density(wage.sample2), 
      col = "red2", 
      lwd = 2)

# add density estimate for the distribution of the sample mean with size=100 to the plot
lines(density(wage.sample3), 
      col = "orange", 
      lwd = 2)


# add a vertical line at the true parameter
abline(v = mean(wage.pop), lty = 2)


# add a legend
legend("topleft",cex = 0.75,
       legend = c(expression('Wage Population'),
                  '',
                  expression(bar(Wage) ~ sample == 5),
                  '',
                  expression(bar(Wage) ~ sample == 30),
                  '',
                  expression(bar(Wage) ~ sample == 100)
       ), 
       lty = c(2, 1, 1, 1), 
       col = c("green","white", "steelblue","white", "red2", "white", "orange"),
       lwd = 2)


