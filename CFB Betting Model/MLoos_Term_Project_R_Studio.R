library(dplyr)
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
stats <- read_csv('stats.csv')
head(stats)

#histograms
ggplot(stats,aes(x=home_points))+
  geom_histogram(binwidth=3)+
  labs(title="Home Team Points",xlab="Points",ylab="Count")

ggplot(stats,aes(x=home_yards))+
  geom_histogram(binwidth=5)+
  labs(title="Home Team Yards",xlab="Yards",ylab="Count")

ggplot(stats,aes(x=home_passYards))+
  geom_histogram(binwidth=5)+
  labs(title="Home Team Passing Yards",xlab="Passing Yards",ylab="Count")

ggplot(stats,aes(x=home_rushYards))+
  geom_histogram(binwidth=5)+
  labs(title="Home Team Rush Yards",xlab="Rush Yards",ylab="Count")

ggplot(stats,aes(x=home_talent))+
  geom_histogram(binwidth=5)+
  labs(title="Home Team Talent",xlab="Talent",ylab="Count")

ggplot(stats,aes(x=spread))+
  geom_histogram(binwidth=1)+
  labs(title="Spread",xlab="Spread",ylab="Count")

#scatter plots
ggplot(stats,aes(x=home_yards, y=home_points))+
  geom_point(position="jitter")+
  labs(title="Points by Yards",xlab="Yards",ylab="Points")

ggplot(stats,aes(x=home_yards, y=home_points))+
  geom_point()+
  labs(title="Points by Yards",xlab="Yards",ylab="Points")

ggplot(stats,aes(x=home_passYards, y=home_points))+
  geom_point()+
  labs(title="Points by Pass Yards",xlab="Pass Yards",ylab="Points")

ggplot(stats,aes(x=home_rushYards, y=home_points))+
  geom_point()+
  labs(title="Points by Rush Yards",xlab="Rush Yards",ylab="Points")

ggplot(stats,aes(x=home_talent, y=home_points))+
  geom_point()+
  labs(title="Points by Team Talent",xlab="Talent",ylab="Points")

glimpse(stats)

#correlation
cor.matrix<-cor(stats[c(5,9,12:16)], method="pearson")
cor.matrix

corrplot(cor.matrix,title="Correlation Matrix Plot")

rcorr(as.matrix(stats[c(5,9,12:16)]), type="pearson")

#regression
train <- stats %>% filter(week<=10)
test <- stats %>% filter(week>10)


model <- lm(home_points ~ home_talent+home_talent+home_yards+home_rushYards+home_passYards+home_yardsPerPass+home_yardsPerRush ,data=train)
summary(model)

model2 <- lm(home_points ~ home_talent+home_talent+home_yards+home_yardsPerPass+home_yardsPerRush ,data=train)
summary(model2)

#model predictions on test data
model1Pred <- predict(model, test)
model2Pred <- predict(model2, test)

actuals_preds <- data.frame(cbind(actuals=test$home_points, predicteds_1=model1Pred, predicteds_2=model2Pred))


ggplot(actuals_preds,aes(x=actuals, y=predicteds_1))+
  geom_point()+
  labs(title="Predicted vs Actuals",xlab="Actuals",ylab="Predicted")
