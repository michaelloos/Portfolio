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
test <- weeks_4_to_10 %>% drop_na()
test <- weeks_4_to_10[c(4:9,12:27)]
test2 <- test[complete.cases(test), ]

logisticPseudoR2s <- function(LogModel)  {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.1 <- 1-dev/nullDev
  R.cs <- 1-exp(-(nullDev-dev)/modelN)
  R.n <- R.cs/(1-(exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.1,3),"\n")
  cat("Cox and Snell R^2       ", round(R.cs,3),"\n")
  cat("Nagelkerke R^2          ", round(R.n,3),"\n")
}



logistic_model <- glm(win ~ totalYards+firstDowns+sacks+defensiveTDs+turnovers+talent+home,data=weeks_4_to_10, family=binomial)
summary(logistic_model)

logisticPseudoR2s(logistic_model); 

logistic_model_MA <- glm(win ~ points_MA+totalYards_MA+firstDowns_MA+sacks_MA+defensiveTDs_MA+turnovers_MA+talent+home,data=weeks_4_to_10,family=binomial)
summary(logistic_model_MA)
logisticPseudoR2s(logistic_model_MA); 

#add predictions to data
weeks_4_to_10$predicted.probabilities<-fitted(logistic_model_MA)

#add outcome and prediction data
weeks_4_to_10<-weeks_4_to_10 %>%
  mutate(predicted.outcome=ifelse(round(weeks_4_to_10$predicted.probabilities)>0.5,"Win","Lose"),
         actual.outcome=ifelse(weeks_4_to_10$win==1,"Win","Lose"),
         correct.prediction=ifelse(predicted.outcome==actual.outcome,1,0))

#calculate accuracy
sum(weeks_4_to_10$correct.prediction)/nrow(weeks_4_to_10)



#predict on test data
weeks_11_to_15$predicted.probabilities <- predict(logistic_model_MA, newdata = weeks_11_to_15, type = "response")

#add outcome and prediction data
weeks_11_to_15<-weeks_11_to_15 %>%
  mutate(predicted.outcome=ifelse(round(weeks_11_to_15$predicted.probabilities)>0.5,"Win","Lose"),
         actual.outcome=ifelse(weeks_11_to_15$win==1,"Win","Lose"),
         correct.prediction=ifelse(predicted.outcome==actual.outcome,1,0))

#calculate accuracy
sum(weeks_11_to_15$correct.prediction)/nrow(weeks_11_to_15)
