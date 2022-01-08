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

weeks_4_to_10 <- stats %>% filter(week>=4 & week<=10) %>% drop_na()
weeks_11_to_15 <- stats %>% filter(week>10) %>% drop_na()

linear_model <- lm(points ~ totalYards+firstDowns+sacks+defensiveTDs+turnovers+talent+home,data=weeks_4_to_10)
summary(linear_model)

model1Pred <- predict(linear_model, weeks_11_to_15)

actuals_preds <- data.frame(cbind(actuals=weeks_11_to_15$points, predicteds_1=model1Pred))


ggplot(actuals_preds,aes(x=actuals, y=predicteds_1))+
  geom_point()+
  labs(title="Predicted vs Actuals",xlab="Actuals",ylab="Predicted")


#Moving Average
linear_model2 <- lm(points ~ points_MA+totalYards_MA+firstDowns_MA+sacks_MA+defensiveTDs_MA+turnovers_MA+talent+home,data=weeks_4_to_10,family=binomial)
summary(linear_model2)

model1Pred <- predict(linear_model2, weeks_11_to_15)

actuals_preds <- data.frame(cbind(actuals=weeks_11_to_15$points, predicteds_1=model1Pred))


ggplot(actuals_preds,aes(x=actuals, y=predicteds_1))+
  geom_point()+
  labs(title="Predicted vs Actuals",xlab="Actuals",ylab="Predicted")


