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
stats <- read_csv('final_stats.csv')
head(stats)
glimpse(stats)

weeks_4_to_10 <- stats %>% filter(week>=4 & week<=10) %>% drop_na()
weeks_11_to_15 <- stats %>% filter(week>10) %>% drop_na()


ggplot(weeks_4_to_10,aes(x=outcome))+
  geom_histogram(binwidth=3)+
  labs(title="Outcome",xlab="Points",ylab="Count")

linear_model <- lm(outcome ~ points_MA*points_MA_opp+MOV_MA*MOV_MA_opp,data=weeks_4_to_10)
summary(linear_model)

linear_model <- lm(outcome ~ MOV_MA*MOV_MA_opp,data=weeks_4_to_10)
summary(linear_model)

linear_model <- lm(outcome ~  MOV_MA+MOV_MA_opp+MOV_MA*MOV_MA_opp,data=weeks_4_to_10)
summary(linear_model)

#calculate home score
home_score <- lm(points ~  points_MA+def_points_MA_opp+points_MA*def_points_MA_opp+totalYards_MA+def_totalYards_MA_opp+
                   +totalYards_MA*def_totalYards_MA_opp, data=weeks_4_to_10)
summary(home_score)

#calculate away score
away_score <- lm(points_opp ~  points_MA_opp+def_points_MA+points_MA_opp*def_points_MA+totalYards_MA_opp+def_totalYards_MA+
                   +totalYards_MA_opp*def_totalYards_MA
                 , data=weeks_4_to_10)
summary(away_score)

weeks_4_to_10$home_prediction <- predict(home_score, weeks_4_to_10)
weeks_4_to_10$away_prediction <- predict(away_score, weeks_4_to_10)
weeks_4_to_10$pred_outcome <- weeks_4_to_10$away_prediction-weeks_4_to_10$home_prediction
weeks_4_to_10$pred_outcome2 <- (weeks_4_to_10$away_prediction-weeks_4_to_10$home_prediction-3)
glimpse(weeks_4_to_10)
weeks_4_to_10[c(31:32,76:78,6:7,4,49)]

ggplot(weeks_4_to_10,aes(x=points, y=prediction))+
  geom_point()+
  labs(title="Points",xlab="Actual",ylab="Predicted")



test <- weeks_4_to_10[c(4,49,6:7,130:133)]
test<-test %>%
  mutate(bet=ifelse(test$pred_outcome<test$spread,"Home","Away"))
test<-test %>%
  mutate(covered=ifelse(test$outcome<test$spread,"Home","Away"))
test<-test %>%
  mutate(correct.prediction=ifelse(test$bet==test$covered,1,0))


test<-test %>%
  mutate(bet=ifelse(test$pred_outcome2<test$spread,"Home","Away"),
         covered=ifelse(test$outcome<test$spread,"Home","Away"),
         correct.prediction=ifelse(test$bet==test$covered,1,0))

sum(test$correct.prediction)/nrow(test)

test$prediction <- predict(linear_model, test)

ggplot(test,aes(x=prediction))+
  geom_histogram(binwidth=1)+
  labs(title="Prediction",xlab="Points",ylab="Count")

#Actual prediction on test set
weeks_11_to_15$home_prediction <- predict(home_score, weeks_11_to_15)
weeks_11_to_15$away_prediction <- predict(away_score, weeks_11_to_15)
weeks_11_to_15$pred_outcome <- weeks_11_to_15$away_prediction-weeks_11_to_15$home_prediction
weeks_11_to_15$pred_outcome2 <- (weeks_11_to_15$away_prediction-weeks_11_to_15$home_prediction-3)

test <- weeks_11_to_15[c(4,49,6:7,129:132)]
test<-test %>%
  mutate(bet=ifelse(test$pred_outcome<test$spread,"Home","Away"))
test<-test %>%
  mutate(covered=ifelse(test$outcome<test$spread,"Home","Away"))
test<-test %>%
  mutate(correct.prediction=ifelse(test$bet==test$covered,1,0))
test<-test %>%
  mutate(is_correct=ifelse(test$correct.prediction==1,"Correct","Incorrect"))

sum(test$correct.prediction)/nrow(test)

ggplot(test, aes(x=spread, color=is_correct)) + 
  geom_histogram(binwidth=3,fill="white", alpha=0.5, position = 'identity')

ggplot(test, aes(x=spread)) + 
  geom_histogram(binwidth=3,fill="white", alpha=0.5, position = 'identity') +
facet_wrap(~ is_correct)
