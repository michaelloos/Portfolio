library(dplyr)
library(tidyverse)
library(ggplot2)
library(QuantPsyc)
library(PerformanceAnalytics)
# library(car)
library(readr)
library(corrplot)
library(class)
library(Hmisc)
options(scipen=999)

#read data
stats <- read_csv('stats_by_team.csv')
head(stats)
glimpse(stats)

#histograms
ggplot(stats,aes(x=points))+
  geom_histogram(binwidth=3)+
  labs(title="Points",xlab="Points",ylab="Count")

ggplot(stats,aes(x=points_MA))+
  geom_histogram(binwidth=3)+
  labs(title="4 Week Moving Average Points",xlab="Moving Average Points",ylab="Count")

ggplot(stats,aes(x=totalYards))+
  geom_histogram(binwidth=5)+
  labs(title="Total Yards",xlab="Yards",ylab="Count")

ggplot(stats,aes(x=netPassingYards))+
  geom_histogram(binwidth=5)+
  labs(title="Passing Yards",xlab="Passing Yards",ylab="Count")

ggplot(stats,aes(x=rushingYards))+
  geom_histogram(binwidth=5)+
  labs(title="Rush Yards",xlab="Rush Yards",ylab="Count")

ggplot(stats,aes(x=firstDowns))+
  geom_histogram(binwidth=1)+
  labs(title="First Downs",xlab="First Downs",ylab="Count")

ggplot(stats,aes(x=turnovers))+
  geom_histogram(binwidth=1)+
  labs(title="Turnovers",xlab="Turnovers",ylab="Count")

ggplot(stats,aes(x=defensiveTDs))+
  geom_histogram(binwidth=1)+
  labs(title="Defensive TDs",xlab="Defensive TDs",ylab="Count")

ggplot(stats,aes(x=sacks))+
  geom_histogram(binwidth=1)+
  labs(title="Sacks",xlab="Sacks",ylab="Count")

ggplot(stats,aes(x=kickReturnYards))+
  geom_histogram(binwidth=5)+
  labs(title="Kick Return Yards",xlab="Kick Return Yards",ylab="Count")

ggplot(stats,aes(x=kickReturnTDs ))+
  geom_histogram(binwidth=1)+
  labs(title="Kick Return TDs",xlab="Kick Return TDs",ylab="Count")

ggplot(stats,aes(x=talent))+
  geom_histogram(binwidth=20)+
  labs(title="Talent",xlab="Talent",ylab="Count")

ggplot(stats,aes(x=spread))+
  geom_histogram(binwidth=1)+
  labs(title="Spread",xlab="Spread",ylab="Count")

#scatter plots
ggplot(stats,aes(x=points, y=win))+
  geom_point()+
  labs(title="Win by Points",xlab="Points",ylab="Win")+
  geom_smooth(method = "glm", method.args = list(family=binomial), se=FALSE)

ggplot(stats,aes(x=points, y=cover))+
  geom_point()+
  labs(title="Cover by Points",xlab="Points",ylab="Cover")+
  geom_smooth(method = "glm", method.args = list(family=binomial), se=FALSE)

ggplot(stats,aes(x=points, y=cover))+
  geom_point(position="jitter")+
  labs(title="Cover by Points",xlab="Points",ylab="Cover")

ggplot(stats,aes(x=totalYards, y=cover))+
  geom_point()+
  labs(title="Cover by Yards",xlab="Yards",ylab="Cover")




ggplot(stats,aes(x=totalYards, y=points))+
  geom_point(position="jitter")+
  labs(title="Points by Yards",xlab="Yards",ylab="Points")

ggplot(stats,aes(x=netPassingYards, y=points))+
  geom_point()+
  labs(title="Points by Pass Yards",xlab="Pass Yards",ylab="Points")

ggplot(stats,aes(x=rushingYards, y=points))+
  geom_point()+
  labs(title="Points by Rush Yards",xlab="Rush Yards",ylab="Points")

ggplot(stats,aes(x=firstDowns, y=points))+
  geom_point()+
  labs(title="Points by First Downs",xlab="First Downs",ylab="Points")

ggplot(stats,aes(x=turnovers, y=points))+
  geom_point()+
  labs(title="Points by Turnovers",xlab="Turnovers",ylab="Points")

ggplot(stats,aes(x=defensiveTDs, y=points))+
  geom_point()+
  labs(title="Points by Defensive TDs",xlab="Defensive TDs",ylab="Points")

ggplot(stats,aes(x=sacks, y=points))+
  geom_point()+
  labs(title="Points by Sacks",xlab="Sacks",ylab="Points")

ggplot(stats,aes(x=kickReturnYards, y=points))+
  geom_point()+
  labs(title="Points by Kick Return Yards",xlab="Kick Return Yards",ylab="Points")

ggplot(stats,aes(x=kickReturnTDs, y=points))+
  geom_point()+
  labs(title="Points by Kick Return TDs",xlab="Kick Return TDs",ylab="Points")

ggplot(stats,aes(x=points_MA, y=points))+
  geom_point()+
  labs(title="Points by 4 Week Moving Average",xlab="Points 4 Week Average",ylab="Points")

ggplot(stats,aes(x=talent, y=points))+
  geom_point()+
  labs(title="Points by Team Talent",xlab="Talent",ylab="Points")

glimpse(stats)

#split into test/train file
weeks_4_to_10 <- stats %>% filter(week>=4 & week<=10)
weeks_11_to_15 <- stats %>% filter(week>10)
test <- weeks_4_to_10 %>% drop_na()
test <- weeks_4_to_10[c(4:9,12:27)]
test2 <- test[complete.cases(test), ]

#correlation
cor.matrix<-cor(weeks_4_to_10[c(4:12,15:27)], use="complete.obs", method="pearson")
cor.matrix

corrplot(cor.matrix,title="Correlation Matrix Plot")

rcorr(as.matrix(weeks_4_to_10[c(4:9,12:27)]), type="pearson")

#regression
train <- stats %>% filter(week<=10)
test <- stats %>% filter(week>10)

model <- lm(points ~ totalYards+firstDowns+sacks+defensiveTDs+turnovers+talent+home,data=weeks_4_to_10)
summary(model)

model_MA <- lm(points ~ points_MA+totalYards_MA+firstDowns_MA+sacks_MA+defensiveTDs_MA+turnovers_MA+talent+home,data=weeks_4_to_10)
summary(model_MA)

#visual plots of residuals
plot(model)
stats$standard.residuals<-rstandard(model)
hist(stats$standard.residuals,breaks=100)

#model predictions on test data
model1Pred <- predict(model, weeks_11_to_15)
model2Pred <- predict(model_MA, weeks_11_to_15)

actuals_preds <- data.frame(cbind(actuals=weeks_11_to_15$points, predicteds_1=model1Pred))

actuals_preds <- data.frame(cbind(actuals=weeks_11_to_15$points, predicteds_1=model2Pred))


ggplot(actuals_preds,aes(x=predicteds_1, y=actuals))+
  geom_point()+
  labs(title="Model 2 Predicted vs Actuals",xlab="Actuals",ylab="Predicted")