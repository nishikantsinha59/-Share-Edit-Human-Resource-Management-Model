
getwd()

setwd("E://Data Science//edureka_project")

#Read Employees data
employees <- read.csv("employeesData.csv")
View(employees)

summary(employees)

str(employees)


# check if any record has missing values

employees[!complete.cases(employees),]


#Correlation values of attributes of data
install.packages('corrplot')
library(corrplot)
?corrplot

m<-cor(employees[,-c(9,10)])
corrplot(m,method = 'number')
    #According to corrplot the satisfaction_level is the correlation coefficient 
    #with a moderate downhill(negative) relationship.


#Convert to factor

employees$left<-as.factor(employees$left)

employees$Work_accident<-as.factor(employees$Work_accident)

employees$promotion_last_5years<-as.factor(employees$promotion_last_5years)

str(employees)

#Separating data of Employees who Left
emp_left <- subset(employees, employees$left == 1)
View(emp_left)

#Plot of complete data set
hist(employees$satisfaction_level)
hist(employees$last_evaluation)
hist(employees$number_project)
hist(employees$average_montly_hours)
hist(employees$time_spend_company)
plot(employees$Work_accident)
plot(employees$promotion_last_5years)
plot(employees$department)
plot(employees$salary)

#Plot of the employees who left
hist(emp_left$satisfaction_level)
hist(emp_left$last_evaluation)
hist(emp_left$number_project)
hist(emp_left$average_montly_hours)
hist(emp_left$time_spend_company)
plot(emp_left$Work_accident)
plot(emp_left$promotion_last_5years)
plot(emp_left$department)
hist(emp_left$salary)



#Finding percentage of employees left from each department

library(dplyr)
library(tidyr) 

?group_by
?transmute

left_emp_percent <- employees %>% group_by(department,left) %>% 
  summarise(total_emp_dept=n()) %>% 
  spread(left, total_emp_dept) %>% 
  ungroup() %>%
  transmute(department = department, left_emp_percent = (`1`/(`1` + `0`))*100)

print(left_emp_percent)

#Splitting data set into train and test
library(caTools)
set.seed(42)

split <- sample.split(employees$left, SplitRatio = 0.7)
employees_train <- subset(employees, split == TRUE)
employees_test <- subset(employees, split == FALSE)

#Coverting salary and department variable into some encoded numeric values
employees_train$salary <- factor(employees_train$salary, levels = c('low','medium','high'), labels = c(1,2,3))
employees_train$department <- factor(employees_train$department, levels = c('accounting','hr','IT','management','marketing','product_mng','RandD','sales','support','technical'), labels = c(1,2,3,4,5,6,7,8,9,10))

employees_test$salary <- factor(employees_test$salary, levels = c('low','medium','high'), labels = c(1,2,3))
employees_test$department <- factor(employees_test$department, levels = c('accounting','hr','IT','management','marketing','product_mng','RandD','sales','support','technical'), labels = c(1,2,3,4,5,6,7,8,9,10))

str(employees_train)
View(employees_test)

#Scaling data

employees_train[,c(1:5)] <- scale(employees_train[,c(1:5)])
employees_test[,c(1:5)] <- scale(employees_test[,c(1:5)])
View(employees_train)
View(employees_test)

#Logistic Regression Model
?glm
model <- glm(data = employees_train, formula = left~ satisfaction_level + last_evaluation + number_project + average_montly_hours + time_spend_company + Work_accident + promotion_last_5years + salary, family = binomial(link = "logit"))
anova(model)

summary(model)

psr <- 1 - (9027.9/11526.4)

print(psr)
      #This Logistic Regression model has very low Psuedo R-square i.e 0.22

######################### Naive Bayes model ##########################
library(caTools)
library(e1071)
library(ggplot2)
library(lattice)
library(caret)

nbmodel <- naiveBayes(x = employees_train[,-7], employees_train[,7])
summary(nbmodel)

#Creating confusion matrix for the model

train_pred <- predict(nbmodel, employees_train[,-7])
confusion_matrix_train <- table(employees_train[,7], train_pred)
print(confusion_matrix_train)


#Calculating Precision and recall

recall <- 1402/(1402+1098)
# 0.5608

precision <- 1402/(1402+584)
# 0.7059416

f1 <- (2*recall*precision)/(recall+precision)
# 0.6250557

#Do same prediction on test data set

test_pred <- predict(nbmodel, employees_test[,-7])
confusion_matrix_test <- table(employees_test[,7], test_pred)
print(confusion_matrix_test)
summary(nbmodel)

#Calculating Precision and recall

recall <- 609/(609+462)
    # 0.5686275

precision <- 609/(609+242)
    # 0.7156287

f1 <- (2*recall*precision)/(recall+precision)
    # 0.6337149





################### Support Vector Machine model ####################
#Read Employees data
employees <- read.csv("employeesData.csv")
View(employees)

summary(employees)

str(employees)

#Convert to factor

employees$left<-as.factor(employees$left)

#Splitting data set into train and test
library(caTools)
set.seed(42)

split <- sample.split(employees$left, SplitRatio = 0.7)
employees_train <- subset(employees, split == TRUE)
employees_test <- subset(employees, split == FALSE)

#Scaling data

employees_train[,c(1:5)] <- scale(employees_train[,c(1:5)])
employees_test[,c(1:5)] <- scale(employees_test[,c(1:5)])
str(employees_train)

#Creating SVM 
?svm
svmodel <- svm(x = employees_train[,-c(7,9,10)], y = employees_train[,c(7)], type = 'C-classification',
                    kernel = 'radial')

y_train_pred <- predict(svmodel,employees_train[,-c(7,9,10)])
table(employees_train$left, y_train_pred)

#Calculating Precision and recall

recall <- 2285/(2285+215)
# 0.914

precision <- 2285/(2285+65)
# 0.9723404

f1 <- (2*recall*precision)/(recall+precision)
# 0.942268

#Do same prediction on test data set

y_test_pred <- predict(svmodel,employees_test[,-c(7,9,10)])
table(employees_test[,7], y_test_pred)

#Calculating Precision and recall

recall <- 962/(962+109)
# 0.898226

precision <- 962/(962+65)
# 0.9367089

f1 <- (2*recall*precision)/(recall+precision)
# 0.9170639



#################### Decision Trees #######################

#Read Employees data
employees <- read.csv("employeesData.csv")
View(employees)

summary(employees)

str(employees)

#Convert to factor

employees$left<-as.factor(employees$left)

employees$Work_accident<-as.factor(employees$Work_accident)

employees$promotion_last_5years<-as.factor(employees$promotion_last_5years)

#Splitting data set into train and test
library(caTools)
set.seed(42)

split <- sample.split(employees$left, SplitRatio = 0.7)
employees_train <- subset(employees, split == TRUE)
employees_test <- subset(employees, split == FALSE)

#Scaling data

employees_train[,c(1:5)] <- scale(employees_train[,c(1:5)])
employees_test[,c(1:5)] <- scale(employees_test[,c(1:5)])
str(employees_train)

#Creating Decision Tree
?rpart
library(rpart)

dtmodel <- rpart(formula = left~., data = employees_train, method = 'class')

y_train_pred <- predict(dtmodel, employees_train[,-c(7)])
    #Here y_train_pred will contain probability of both 0 and 1 in output
    #We need to take max among these two columns, that why we have used type = 'class'

y_train_pred <- predict(dtmodel, employees_train[,-c(7)], type = 'class')
table(employees_train$left, y_train_pred)

#Calculating Precision and recall

recall <- 2299/(2299+201)
# 0.9196

precision <- 2299/(2299+84)
# 0.9647503

f1 <- (2*recall*precision)/(recall+precision)
# 0.9416342

#Do same prediction on test data set

y_test_pred <- predict(dtmodel, employees_test[,-c(7)], type = 'class')
table(employees_test$left, y_test_pred)

#Calculating Precision and recall

recall <- 722/(722+349)
# 0.6741363

precision <- 722/(722+46)
# 0.9401042

f1 <- (2*recall*precision)/(recall+precision)
# 0.7852094


####################### Random Forest Classification ###################

#Read Employees data
employees <- read.csv("employeesData.csv")
View(employees)

summary(employees)

str(employees)

#Convert to factor

employees$left<-as.factor(employees$left)

employees$Work_accident<-as.factor(employees$Work_accident)

employees$promotion_last_5years<-as.factor(employees$promotion_last_5years)

#Splitting data set into train and test
library(caTools)
set.seed(42)

split <- sample.split(employees$left, SplitRatio = 0.7)
employees_train <- subset(employees, split == TRUE)
employees_test <- subset(employees, split == FALSE)

#Scaling data

employees_train[,c(1:5)] <- scale(employees_train[,c(1:5)])
employees_test[,c(1:5)] <- scale(employees_test[,c(1:5)])
str(employees_train)

#Creating Random Forest
install.packages('randomForest')
library(randomForest)

rfmodel <- randomForest(x = employees_train[,-c(7)], y = employees_train[,c(7)], ntree = 190)

y_train_pred <- predict(rfmodel, employees_train[,-c(7)])
table(employees_train$left, y_train_pred)

#Calculating Precision and recall

recall <- 2499/(2499+1)
# 0.9996

precision <- 2499/(2499+3)
# 0.998801

f1 <- (2*recall*precision)/(recall+precision)
# 0.9992003

#Do same prediction on test data set

y_test_pred <- predict(rfmodel, employees_test[,-c(7)])
table(employees_test$left, y_test_pred)

#Calculating Precision and recall

recall <- 898/(898+173)
# 0.8384687

precision <- 898/(898+7)
# 0.9922652

f1 <- (2*recall*precision)/(recall+precision)
# 0.9089069


##### On comparing all the above classification model's result
##### it can be concluded that Random Forest is the most accurate
##### model for the given problem f1 score (accuracy score) of 0.91