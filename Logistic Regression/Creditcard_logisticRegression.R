#import csv files
Credit_card<-read.csv(file.choose(),header = T)
View(Credit_card)

#display first few records
head(Credit_card)

#display structure of the data set
str(Credit_card)     


#display the column names
colnames(Credit_card)

#checking the null values present in a data set
sum(is.na(Credit_card))

#there is no null values in our data set


# convert categorical data numerical, i am using label encoding
Credit_card$card<-as.numeric(factor(Credit_card$card))-1
Credit_card$owner<-as.numeric(factor(Credit_card$owner))-1
Credit_card$selfemp<-as.numeric(factor(Credit_card$selfemp))-1

str(Credit_card)

# so we convert all categorical data into numerical form

##############VISUALIZATION########################

#Here i am visualizing the variable using ggplot

#install.packages("ggplot2")
library(ggplot2)

#visualization of income
ggplot(Credit_card, aes(x = income)) +                           
  geom_density(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "income",
       y = "Count of Values")

#visualization of age
ggplot(Credit_card, aes(x = age)) +                           
  geom_density(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "age",
       y = "Count of Values")

#visualize Number of active credit accounts.
ggplot(Credit_card, aes(x = active)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "active",
       y = "Count of Values")

#visualize Number of dependents
ggplot(Credit_card, aes(x = dependents)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "dependents",
       y = "Count of Values")

#visualize Months living at current address.
ggplot(Credit_card, aes(x = months)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "months",
       y = "Count of Values")

#visualize the individual self-employed?
ggplot(Credit_card, aes(x = selfemp)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "selfemp",
       y = "Count of Values")

#here we can see that very less people are self employes

#visualize  the individual own their home?
ggplot(Credit_card, aes(x = owner)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "owner",
       y = "Count of Values")

# mejority individulal doesn't have home

#visualize Average monthly credit card expenditure.
ggplot(Credit_card, aes(x = expenditure)) +                           
  geom_density(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "expenditure",
       y = "Count of Values")

#most people credit card expenditure between 500to 1000 us doller

#check any ouliers present in our data set using boxplot

boxplot(Credit_card$age)  #many outliers
boxplot(Credit_card$income) #many outliers
boxplot(Credit_card$share)  #many outliers
boxplot(Credit_card$expenditure) #many outliers
boxplot(Credit_card$months) #many outliers
boxplot(Credit_card$active) #many outliers

# so here we detect many outliers, so we have to remove those outliers from our data set

boxplot(Credit_card$age,plot = FALSE)$out 

#removing the outliers of age columns
outliers <- boxplot(Credit_card$age, plot=FALSE)$out
x<- Credit_card
x<- x[-which(x$age %in% outliers),]
Credit_card<-x
boxplot(Credit_card$age)  #no outliers


###SPLITTING THE DATA SET#############3

## create train and test data
#install.packages("caTools")
library(caTools)

##use caTools function to split, SplitRatio for 70%:30% splitting

credit_data= sample.split(Credit_card[-1],SplitRatio = 0.3)

## here I am using 70% of data for training and 30% data for testing
#subsetting into Test data
test_data =subset(Credit_card[-1],credit_data==TRUE)

#subsetting into Train data
train_data=subset(Credit_card[-1],credit_data==FALSE)
## check number of records present  in the data set
nrow(test_data)
nrow(train_data)
head(train_data)
head(test_data)
View((train_data))
View((test_data))
####################################################
#BUILD A MODEL
#Classify whether application accepted or not using Logistic regression

# here "card" is our targrt variable

credit_model <- glm(card~.,data=train_data,family = "binomial")
summary(credit_model)

##predict the model#######

credit_prob <- predict(credit_model,type=c("response"),train_data)
View(credit_prob)
head(credit_prob)

###create confusion matrix table

conf_matrix<-table(credit_prob>0.5,train_data$card)
conf_matrix
# here the probability value>0.5, classified as 1, else classified as 0

#check the  Model Accuracy 
Accuracy<-sum(diag(conf_matrix)/sum(conf_matrix))
Accuracy
# 98.55% of accuracy

#check the error rate
1-Accuracy
#0.01% error rate. very less error rate

########ROC curve##########
#ROC curve is a metric describing the trade-off between 
#the sensitivity (true positive rate, TPR) and specificity (false positive rate, FPR)
#of a prediction in all probability cutoffs (thresholds). 
#It can be used for binary and multiclass classification accuracy checking.

#install.packages("ROCR")

library(ROCR)

roc_prediction<-prediction(credit_prob,train_data$card)

roc_performance<-performance(roc_prediction,'tpr','fpr')

plot(roc_performance,colorize=T,text.adj = c(0.5, 0.5))

# using ROC we can understand how good the model is.
# This is a perfect curve. When two curves don't overlap at all means model 
#has an ideal measure of separability. 
#It is perfectly able to distinguish between positive class and negative class.

######################################################

#Predict on Test data set


test_prediction <- predict(credit_model, newdata = test_data, type = "response")

test_pred_num <- ifelse(test_prediction > 0.5, 1, 0)
pred_test <- factor(test_pred_num, levels=c(0, 1))
test_actual_data<-test_data$card

# compare the predicted value in test_data
head(test_data)

test_data$predict_data<- pred_test
head(test_data)

#compare the accuracy of test data and actual data
mean(pred_test== test_actual_data)       #98.44 %accuracy


new_test_df<-test_data[,c("card","predict_data")]
View(new_test_df)
head(new_test_df,20)

#Conclsion:
# here we have created logistic model using train data set with 98.55 accuracy
#using this model correctly predict whether application accepted or not using test data set