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
library(caTools)
options(scipen=999)

#read data
salary <- read_csv('salary.csv')
head(salary)
glimpse(salary)

salary$Q10<-ifelse(salary$Q10=='300000-500001-500001','300000-500000',salary$Q10)
salary$Q10<-ifelse(salary$Q10=='500001-500001','> 500000',salary$Q10)
salary$Q4<-ifelse(salary$Q4=='Some college/university study without earning a bachelor’s degree','Some college',salary$Q4)
salary$Q4<-ifelse(salary$Q4=='No formal education past high school','High School',salary$Q4)

unique(salary[c("Q24")])

salary$Q10 <- factor(salary$Q10, levels=c("15000-19999", "20000-24999", "25000-29999", "30000-39999", "40000-49999", 
                                                 "50000-59999", "60000-69999", "70000-79999", "80000-89999", "90000-99999", 
                                                 "100000-124999", "125000-149999", "150000-199999", "200000-249999", "250000-299999",
                                                 "300000-500000", "> 500000"))
salary$Q4 <- factor(salary$Q4, levels=c("I prefer not to answer", "High School", "Some college", 
                                       "Professional degree", "Bachelor’s degree", "Master’s degree", "Doctoral degree"))
salary$Q1 <- factor(salary$Q1, levels=c("18-21", "22-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-69", "70+"))
salary$Q6 <- factor(salary$Q6, levels=c("0-49 employees", "50-249 employees", "250-999 employees", 
                                        "1000-9,999 employees", "> 10,000 employees"))        
salary$Q23 <- factor(salary$Q23, levels=c("< 1 years", "1-2 years", "2-3 years","3-4 years", "4-5 years",
                                          "5-10 years", "10-15 years", "20+ years"))  

#EDA
summary(salary$mean_salary)

#find range of salaries by role
salary %>% 
  group_by(Q5) %>% 
  summarise(mean = mean(mean_salary),
            median = quantile(mean_salary, 0.5),
            q1 = quantile(mean_salary, 0.25),
            q3 = quantile(mean_salary, 0.75),
            range = quantile(mean_salary, 0.75)-quantile(mean_salary, 0.25))

#find range of salaries by gender
salary %>% 
  group_by(Q2) %>% 
  summarise(mean = mean(mean_salary),
            median = quantile(mean_salary, 0.5),
            q1 = quantile(mean_salary, 0.25),
            q3 = quantile(mean_salary, 0.75),
            range = quantile(mean_salary, 0.75)-quantile(mean_salary, 0.25))

#find range of salaries by education
salary %>% 
  group_by(Q4) %>% 
  summarise(mean = mean(mean_salary),
            median = quantile(mean_salary, 0.5),
            q1 = quantile(mean_salary, 0.25),
            q3 = quantile(mean_salary, 0.75),
            range = quantile(mean_salary, 0.75)-quantile(mean_salary, 0.25))

#find range of salaries by age
salary %>% 
  group_by(Q1) %>% 
  summarise(mean = mean(mean_salary),
            median = quantile(mean_salary, 0.5),
            q1 = quantile(mean_salary, 0.25),
            q3 = quantile(mean_salary, 0.75),
            range = quantile(mean_salary, 0.75)-quantile(mean_salary, 0.25))



#bar chart of salary
ggplot(salary, aes(Q10)) + 
  geom_bar(fill = "steelblue") +
  labs(title="Salary", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#bar chart of age
ggplot(salary, aes(Q1)) + 
  geom_bar(fill = "steelblue") +
  labs(title="Age", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#bar chart of education
ggplot(salary, aes(Q4)) + 
  geom_bar(fill = "steelblue") +
  labs(title="Education", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#bar chart of role
ggplot(salary, aes(Q5)) + 
  geom_bar(fill = "steelblue") +
  labs(title="Role Title", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#bar chart of salary w/age fill
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill=Q1)) +
  labs(title="Salary", x="Age", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0.1))

#bar chart of salary w/sex facet wrap
ggplot(salary, aes(Q10)) + 
  geom_bar() +
  facet_wrap(~ Q2)
  labs(title="Salary", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0))

#bar chart of salary w/sex fill
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill=Q2)) +
  labs(title="Salary", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0))

#bar chart of salary w/education fill
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill=Q4)) +
  labs(title="Salary", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0.1))

#bar chart of salary w/education facet wrap
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill= Q4), show.legend = FALSE) +
  facet_wrap(~ Q4, scales='free') +
  labs(title="Salary", x="Education", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0))

#bar chart of salary w/sex facet wrap
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill= Q2), show.legend = FALSE) +
  facet_wrap(~ Q2, scales='free') +
  labs(title="Salary", x="Gender", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(axis.text.y = element_text(angle = 0)) +
  scale_y_discrete(expand = c(0,0))

#bar chart of salary w/age facet wrap
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill= Q1), show.legend = FALSE) +
  facet_wrap(~ Q1, scales='free') +
  labs(title="Salary", x="Age", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0))

#bar chart of salary w/role facet wrap
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill= Q5), show.legend = FALSE) +
  facet_wrap(~ Q5, scales='free') +
  labs(title="Salary", x="Role", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0))

#bar chart of salary w/ML experience facet wrap
ggplot(salary, aes(Q10)) + 
  geom_bar(aes(fill= Q23), show.legend = FALSE) +
  facet_wrap(~ Q23, scales='free') +
  labs(title="Salary", x="Machine Learning Experience", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0))

#bar chart of education w/age facet wrap
ggplot(salary, aes(Q4)) + 
  geom_bar(aes(fill= Q1), show.legend = FALSE) +
  facet_wrap(~ Q1, scales='free') +
  labs(title="Salary", x="Age", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_discrete(expand = c(0,0))


#languages
salary %>% 
  dplyr::select(Q10, starts_with("Q18_P")) %>% 
  pivot_longer(cols = starts_with("Q18_Part"), values_to = "Languages") %>%
  drop_na() %>% 
  group_by(Languages) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(tot = sum(freq)) %>% 
  ggplot(aes(x=Languages, y = freq)) +                            
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Language", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + 
  geom_text(aes(label = freq), vjust = -0.5, color = "black", size=3)+ 
  scale_y_discrete(expand = expansion(mult = c(0,0.05)))

#salary by language
salary %>% 
  dplyr::select(Q10, starts_with("Q18_P")) %>% 
  pivot_longer(cols = starts_with("Q18_Part"), values_to = "Languages") %>%
  #select(-name) %>%
  drop_na() %>% 
  group_by(Q10,Languages) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x=Q10, y = freq, fill=Languages)) +                            
  geom_bar(stat = "identity") +
  facet_wrap(~ Languages, scales = "free") + 
  theme_minimal() +
  labs(x = "Salary by Language", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + 
  geom_text(aes(label = freq), vjust = -0.5, color = "black", size=3)+ 
  scale_y_discrete(expand = expansion(mult = c(0,0.05)))

#Current Role Title
salary %>% 
  dplyr::select(Q5) %>% 
  pivot_longer(cols = everything(), values_to = "Role") %>% 
  drop_na() %>% 
  group_by(Role) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(Role, freq)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() +
  labs(x = "Role", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
  geom_text(aes(label = freq), vjust = -0.6, color = "gray20", size=3)+ 
  scale_y_discrete(expand = expansion(mult = c(0,0.05)))


#Salary by Role Title
salary %>% 
  dplyr::select(Q10, Q5) %>% 
  drop_na() %>% 
  group_by(Q10,Q5) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x=Q10, y = freq, fill=Q5)) +                            
  geom_bar(stat = "identity") +
  facet_wrap(~ Q5, scales = "free") + 
  theme_minimal() +
  labs(x = "Salary by Role", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + 
  #geom_text(aes(label = freq), vjust = -0.5, color = "black", size=3)+ 
  #scale_y_discrete(expand = expansion(mult = c(0,0.05)))

#Salary by ML Experience
salary %>% 
  dplyr::select(Q10, Q23) %>% 
  drop_na() %>% 
  group_by(Q10,Q23) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x=Q10, y = freq, fill=Q23)) +                            
  geom_bar(stat = "identity") +
  facet_wrap(~ Q23, scales = "free") + 
  theme_minimal() +
  labs(x = "Language", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + 
  geom_text(aes(label = freq), vjust = -0.5, color = "black", size=3)+ 
  scale_y_discrete(expand = expansion(mult = c(0,0.05)))



#Size of company
salary %>% 
  dplyr::select(Q6) %>% 
  pivot_longer(cols = everything(), values_to = "Company_Size") %>% 
  drop_na() %>% 
  group_by(Company_Size) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(Company_Size, freq)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() +
  labs(x = "Company Employees", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
  geom_text(aes(label = freq), vjust = -0.6, color = "gray20", size=3)+ 
  scale_y_discrete(expand = expansion(mult = c(0,0.05)))


#creating model

#format data 
format_salary <- salary %>%
  dplyr::select(Age_ = Q1, Gender_ = Q2, Education_ = Q4, Role_ = Q5, Company_Size = Q6, Language_ = starts_with("Q18_P"), 
                Algo_ = starts_with("Q24_P"), mean_salary) %>%
  mutate_if(is.character, ~ if_else(is.na(.), "No_answer", .)) %>% 
  mutate_if(is.character, as_factor) %>% 
  filter(! is.na(mean_salary))

#format data without age/gender
format_salary <- salary %>%
  dplyr::select(Education_ = Q4, Role_ = Q5, Company_Size = Q6, Language_ = starts_with("Q18_P"), 
                Algo_ = starts_with("Q24_P"), mean_salary) %>%
  mutate_if(is.character, ~ if_else(is.na(.), "No_answer", .)) %>% 
  mutate_if(is.character, as_factor) %>% 
  filter(! is.na(mean_salary))

#format data without age/gender/education/algo
format_salary <- salary %>%
  dplyr::select(Role_ = Q5, Company_Size = Q6, Language_ = starts_with("Q18_P"), 
                 mean_salary) %>%
  mutate_if(is.character, ~ if_else(is.na(.), "No_answer", .)) %>% 
  mutate_if(is.character, as_factor) %>% 
  filter(! is.na(mean_salary))

#format data without 
format_salary <- salary %>%
  dplyr::select(Age_ = Q1, Gender_ = Q2, Role_ = Q5, Company_Size = Q6, Language_ = starts_with("Q18_P"), 
                 mean_salary) %>%
  mutate_if(is.character, ~ if_else(is.na(.), "No_answer", .)) %>% 
  mutate_if(is.character, as_factor) %>% 
  filter(! is.na(mean_salary))

#format data without 
format_salary <- salary %>%
  dplyr::select(Age_ = Q1, Gender_ = Q2, Role_ = Q5, Company_Size = Q6, 
                mean_salary) %>%
  mutate_if(is.character, ~ if_else(is.na(.), "No_answer", .)) %>% 
  mutate_if(is.character, as_factor) %>% 
  filter(! is.na(mean_salary))

#split data into train/test
set.seed(101) 
sample <- sample.split(seq_len(nrow(format_salary)), SplitRatio = .80)
train <- subset(format_salary, sample == TRUE)
test  <- subset(format_salary, sample == FALSE)

#train model
train.model <- lm(mean_salary ~ . ,data=format_salary)

#summary statistics
summary(train.model)$coefficient
plot(train.model)


#model predictions on test data
trainPred <- predict(train.model, train)
testPred <- predict(train.model, test)

train_preds <- data.frame(cbind(actuals=train$mean_salary, predicteds_1=trainPred))
test_preds <- data.frame(cbind(actuals=test$mean_salary, predicteds_1=testPred))


ggplot(test_preds,aes(x=actuals, y=predicteds_1))+
  geom_point()+
  labs(title="Predicted vs Actuals",xlab="Actuals",ylab="Predicted")

#calculate MSE
MSE <- mean(summary(train.model)$residuals^2)
MSE

rmse <- sqrt(sum((exp(trainPred) - test$mean_salary)^2)/length(test$mean_salary))
c(RMSE = rmse, R2=summary(train.model)$r.squared)






#clean summary statistics
cmon <- broom::tidy(test.model) %>% 
  t() %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center", font_size = 9)

test2.model <- lm(mean_salary ~ . ,data=test2)
cmon2 <- broom::tidy(test2.model) %>% 
  t() %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center", font_size = 9)


#trying to remove leading question from headers
test2 <- maybe %>%
  dplyr::select(Education_ = Q4, Role_ = Q5, Company_Size_ = Q6, Language_ = starts_with("Q18_P"), 
                Algo_ = starts_with("Q24_P"), Frame_ = starts_with("Q28_P"), mean_salary) %>%
  sapply(function(x) 
    str_remove_all(x, "[^[:digit:]]+") %>% 
      unlist() %>% 
      as.integer() %>% 
      mean(na.rm = TRUE)) %>%
  mutate_if(is.character, ~ if_else(is.na(.), "No_answer", .)) %>% 
  mutate_if(is.character, as_factor) %>% 
  filter(! is.na(mean_salary))
