library(gmodels)
#install.packages("gmodels")
library(tidyverse)

bodytemp <- rnorm(10000, 97.82, sd=.69)
glimpse(bodytemp)

hist(bodytemp)

set.seed(1234)

bodysample <-sample(bodytemp, 10)
mean(bodysample)

bodysample100 <- sample(bodytemp, 100)
mean(bodysample100)

bodysample1000 <- sample(bodytemp, 1000)
mean(bodysample1000)

our_sample <- numeric(100)
for(i in 1:100){
  a_sample <- sample(bodytemp, 50)
  our_sample[i] <- mean(a_sample)
}

hist(our_sample, breaks=50)

our_sample1 <- numeric(10000)
for(i in 1:10000){
  a_sample1 <- sample(bodytemp, 50)
  our_sample1[i] <- mean(a_sample1)
}

hist(our_sample1, breaks=50)

temp_mean <- mean(bodytemp)
temp_stdev <- sd(bodytemp, na.rm = TRUE)
sample_size <- length(bodytemp)

temp_mean

error_n <- qnorm(.975)*temp_stdev/sqrt(sample_size)
left_n <- temp_mean -error_n
right_n <- temp_mean +error_n

print(left_n)
print(right_n)

ci(bodytemp, confidece=.95

realtemps <- read.csv("Desktop/info640-labs/DataSets/Normtemp.csv", header=TRUE)
glimpse(realtemps)

realtemps$Gender <- as.factor(realtemps$Gender)
hist(realtemps)
summary(realtemps)

hist(realtemps$Body.Temp)

real_mean <- mean(realtemps$Body.Temp)

t.test(realtemps$Body.Temp, mu=real_mean, conf.level = .95)

