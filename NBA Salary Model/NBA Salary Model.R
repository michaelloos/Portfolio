#Final Project
#Name: Loos, Michael
#Date: 30 May, 2020

library(dplyr)
library(ggplot2)
library(QuantPsyc)
library(PerformanceAnalytics)
library(car)
library(readr)
library(corrplot)
library(class)
options(scipen=999)

NBA_data <- read_csv("NBA_Players.csv") 
#remove unsigned players/0 salary/
#rookies since they can't negotiate?/remove NA PPG
#remove APG/PPG due to redundancy
#merge positions?
#remove N/A?
NBA_data <- subset(NBA_data, NBA_data$SALARY!='Not signed')
NBA_data <- subset(NBA_data, NBA_data$EXPERIENCE>0)


#convert variable types
NBA_data$SALARY<-gsub(",", "", NBA_data$SALARY)
NBA_data$SALARY<-ifelse(NBA_data$SALARY!='Not signed',NBA_data$SALARY,0)
NBA_data$SALARY<-as.numeric(NBA_data$SALARY)
NBA_data$TEAM<-as.factor(NBA_data$TEAM)
NBA_data$POSITION<-as.factor(NBA_data$POSITION)
NBA_data$AGE<-as.integer(NBA_data$AGE)
NBA_data$COLLEGE<-as.factor(NBA_data$COLLEGE)


#split FGM and FGA
NBA_data <- NBA_data %>% separate("FGM_FGA" ,c("FGM", "FGA"),sep="-")
NBA_data$FGM<-as.numeric(NBA_data$FGM)
NBA_data$FGA<-as.numeric(ifelse(is.na(NBA_data$FGA),0,NBA_data$FGA))


#split THM and THA-3s
NBA_data <- NBA_data %>% separate("THM_THA" ,c("THM", "THA"),sep="-")
NBA_data$THM<-as.numeric(NBA_data$THM)
NBA_data$THA<-as.numeric(ifelse(is.na(NBA_data$FGA),0,NBA_data$FGA))

#split FTM and FTA
NBA_data <- NBA_data %>% separate("FTM_FTA" ,c("FTM", "FTA"),sep="-")
NBA_data$FTM<-as.numeric(NBA_data$FTM)
NBA_data$FTA<-as.numeric(ifelse(is.na(NBA_data$FTA),0,NBA_data$FTA))

#add True Shooting Percentage
NBA_data$TSP<-round(NBA_data$PPG_CAREER/(2*(NBA_data$FGA+(0.44*NBA_data$FTA))),3)
NBA_data$TSP<-ifelse(is.na(NBA_data$TSP),0,NBA_data$TSP)

#Remove Player, URL, and APG, and PPG
NBA_clean<-NBA_data[c(1,3,5:28,30:32,34)]


glimpse(NBA_clean)

#distributions of variables
summary(NBA_clean$SALARY)

ggplot(NBA_clean,aes(x=SALARY))+
  geom_histogram(binwidth=1000000)+
  labs(title="Salary Distribution",xlab="Salary",ylab="Count")

ggplot(NBA_clean,aes(x=TEAM))+
  geom_bar()+
  labs(title="Players by Team",xlab="Team",ylab="Count")+
  theme(axis.text.x=element_blank())

ggplot(NBA_clean,aes(x=POSITION))+
  geom_bar()+
  labs(title="Players by Position",xlab="Position",ylab="Count")

ggplot(NBA_clean,aes(x=COLLEGE))+
  geom_bar()+
  labs(title="Players by College",xlab="Position",ylab="Count")+
  theme(axis.text.x=element_blank())

#Salary by Factor Variables
ggplot(NBA_clean,aes(x=POSITION, y=SALARY, col=POSITION))+
  geom_point(position="jitter")+
  labs(title="Salary by Position",xlab="Position",ylab="Salary")

ggplot(NBA_clean,aes(x=TEAM, y=SALARY, col=TEAM))+
  geom_point()+
  labs(title="Salary by Team",xlab="Team",ylab="Salary")+
  theme(axis.text.x=element_blank())

#set NAs to 0
NBA_clean[is.na(NBA_clean)]=0

cor.matrix<-cor(NBA_clean[c(2,4:6,8:30)])

#lets remove AGE, GP, HT,WT, THA
cor.matrix<-cor(NBA_clean[c(2,8:15,17:21,23:30)])
corrplot(cor.matrix,title="Correlation Matrix Plot")

rcorr(as.matrix(NBA_clean[c(2,4:6,8:30)]))

#season/career stats
cor.matrix<-cor(NBA_clean[c(2,4,8:15)])


season.model <- lm(SALARY ~ EXPERIENCE+PPG_LAST_SEASON+APG_LAST_SEASON+RPG_LAST_SEASON ,data=NBA_clean)
career.model <- lm(SALARY ~ EXPERIENCE+PPG_CAREER+APG_CAREER+RGP_CAREER,data=NBA_clean)


#all significant
test.model <- lm(SALARY ~ EXPERIENCE+PPG_LAST_SEASON+APG_LAST_SEASON+
                   RPG_LAST_SEASON+PPG_CAREER+APG_CAREER+RGP_CAREER,data=NBA_clean)
#taking out unintuitve negatives from above
test.model <- lm(SALARY ~ EXPERIENCE+PPG_LAST_SEASON,data=NBA_clean)
summary(test.model)
summary(all.model)
summary(season.model)
summary(career.model)

#summarize model
summary(test.model)
#beta values
lm.beta(test.model)
#standard deviations of variables
sd(NBA_clean$EXPERIENCE)
sd(NBA_clean$PPG_LAST_SEASON)
#confidence intervals 
confint(test.model)

#ANOVA test<-only if heirarchical?
anova(test.model,season.model)

#casewise diagnostics
NBA_clean$residuals<-resid(test.model)
NBA_clean$standard.residuals<-rstandard(test.model)
NBA_clean$student.residuals<-rstudent(test.model)
NBA_clean$cooks.distance<-cooks.distance(test.model)
NBA_clean$dfbeta<-dfbeta(test.model)
NBA_clean$dffits<-dffits(test.model)
NBA_clean$hats<-hatvalues(test.model)
NBA_clean$covratio<-covratio(test.model)

#standard residuals greater than 2
NBA_clean$large.residual<-NBA_clean$standard.residuals>2 | NBA_clean$standard.residuals< -2
#sum of large residuals
sum(NBA_clean$large.residual)

#average leverage calculation
#k+1/n n=388
avg_lev<-(8/388)

CVR1<-1+3*(7+1)/388
CVR2<-1-3*(7+1)/388
CVR1 
CVR2

#view diagnostics
observe<-NBA_clean[NBA_clean$large.residual, c("SALARY","standard.residuals","cooks.distance","hats","covratio")]

#Durbin-Watson test for independence
#Should be close to 2
dwt(test.model)

#VIF and tolerance for multicollinearity
#VIF<10, average VIF ~1, tolerance above 0.2
vif(test.model)
mean(vif(test.model))
1/vif(test.model)

#visual plots of residuals
plot(test.model)
hist(NBA_clean$standard.residuals,breaks=100)
