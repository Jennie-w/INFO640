install.packages("ggthemes")
#install.packages("dplyr")

library(ggthemes)
library(tidyverse)
library(dbplyr)
library(lubridate)

happyrate <- read.csv("Desktop/2016worldhappiness.csv", header=TRUE, check.names = FALSE)

class(happyrate)
dim(happyrate)
names(happyrate)
str(happyrate)
glimpse(happyrate)
summary(happyrate)

head(happyrate)

sum(is.na(happyrate))

ggplot(happyrate, aes(x =`Happiness Rank`, y =`Happiness Score`, color = `Happiness Score`)) + 
  geom_jitter(alpha=0.5)+
  stat_smooth(method="lm", se=FALSE, col="light blue") +
  labs(title = "Happiness Rank by Happiness Score")

ggplot(happyrate, aes(x =`Happiness Rank`, y =`Economy (GDP per Capita)`, color = `Economy (GDP per Capita)`)) +
  geom_jitter(alpha=0.5)+
  stat_smooth(method="lm", se=FALSE, col="light blue") +
  labs(title = "Happiness Rank by Economy (GDP per Capita)")

ggplot(happyrate, aes(x =`Happiness Rank`, y =`Health (Life Expectancy)`, color = `Health (Life Expectancy)`)) + 
  geom_jitter(alpha=0.5)+
  stat_smooth(method="lm", se=FALSE, col="light blue") +
  labs(title = "Happiness Rank by Health (Life Expectancy)")

ggplot(happyrate, aes(x =`Happiness Rank`, y =`Freedom`, color = `Freedom`)) + 
  geom_jitter(alpha=0.5)+
  stat_smooth(method="lm", se=FALSE, col="light blue") +
  labs(title = "Happiness Rank by Freedom")
  
ggplot(happyrate, aes(x =`Happiness Rank`, y =`Region`, color = `Region`)) + 
  geom_jitter(alpha=0.5)+
  stat_smooth(method="lm", se=FALSE, col="light blue") +
  labs(title = "Happiness Rank by Region")

happyrate_more <- ggplot(happyrate, aes(x =`Happiness Rank`, y =`Freedom`, color = `Region`))

happyrate_more + geom_point()
happyrate_more + geom_jitter()
happyrate_more + geom_line()+
  labs(title = "Happiness Rank by Freedom in Regions")

ggplot(happyrate, aes(x =`Happiness Rank`, y =`Freedom`, color=`Freedom`)) + 
  geom_jitter(alpha=0.5) +
  stat_smooth(method="lm", se=FALSE, col="blue") +
  labs(title = "Happiness Rank by Freedom")

ggplot(happyrate, aes(x =`Happiness Rank`, y =`Region`, color = `Region`)) + 
  geom_jitter(alpha=0.5)

`happyrate`$`Region`[`happyrate`$`Region`==""] <- "NA"

posn <- position_jitter(width = .1)
happyrate_more + geom_point(position=posn) +
  facet_grid(.~Region)

posn <- position_jitter(width = .1)
happyrate_more + geom_point(position=posn)

happyrate_summary <- aggregate(happyrate[1:7], list(happyrate$Region), mean)
names(happyrate_summary)[1] <-"Region"
happyrate_summary
warnings()



