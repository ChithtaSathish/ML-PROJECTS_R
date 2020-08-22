install.packages("plyr")
install.packages("ggplot2")

#importing csv files
View(bank_data)

#display first top records from a data set
head(bank_data)

#display structure of the data set
str(bank_data)
#in our data set we have total 45211 obs. of  17 variables:
#in our data set many categorical variable,to apply logistic regression algorithm
# we would convert the categorical data into numerical format

#display the column names
colnames(bank_data)

#shows dimension of the data set
dim(bank_data)

#shows summary
summary(bank_data)

#checking the null values present in a data set
sum(is.na(bank_data))

#there is no null values in our data set

##############VISUALIZATION########################

#Here i am visualizing the variable using ggplot

#install.packages("ggplot2")
library(ggplot2)

#visualization of age
ggplot(bank_data, aes(x = age)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "age",
       y = "Count of Values")
# here we can see that most of the clients age is between 25-50

##visualization of average yearly balance, in euros (numeric) 
ggplot(bank_data, aes(x = balance)) +                           
  geom_density(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "balance",
       y = "Count of Values")
# here we can see that many custemer has negative and zero balance

#visualization of duration
ggplot(bank_data, aes(x = duration)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "duration",
       y = "Count of Values")



#visualization of number of contacts performed during this campaign and for this client
ggplot(bank_data, aes(x = campaign)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "campaign",
       y = "Count of Values")


count(bank_data, "job")
count(bank_data,"marital")
count(bank_data,"education")
count(bank_data,"default")
count(bank_data,"housing")
count(bank_data,"loan")
count(bank_data,"contact")
count(bank_data,"month")
count(bank_data,"poutcome")
count(bank_data,"y")

#here some variables contain many category, and some variable has less category
#so convert categorical data numerical, i am using  label encoding

bank_data$job<-as.numeric(factor(bank_data$job))-1
bank_data$marital<-as.numeric(factor(bank_data$marital))-1
bank_data$education<-as.numeric(factor(bank_data$education))-1
bank_data$default<-as.numeric(factor(bank_data$default))-1
bank_data$housing<-as.numeric(factor(bank_data$housing))-1
bank_data$loan<-as.numeric(factor(bank_data$loan))-1
bank_data$contact<-as.numeric(factor(bank_data$contact))-1
bank_data$month<-as.numeric(factor(bank_data$month))-1
bank_data$poutcome<-as.numeric(factor(bank_data$poutcome))-1
bank_data$y<-as.numeric(factor(bank_data$y))-1

str(bank_data)
# here we convert all categorical data into a numerical form
View(bank_data)

#visualize job
ggplot(bank_data, aes(x = job)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "job",
       y = "Count of Values")

# Visualization using boxplot
boxplot(bank_data$age)
boxplot(bank_data$job)
boxplot(bank_data$marital)
boxplot(bank_data$education)
boxplot(bank_data$default)
boxplot(bank_data$balance)
boxplot(bank_data$housing)
boxplot((bank_data$loan))
boxplot(bank_data$contact)
boxplot(bank_data$campaign)
boxplot(bank_data$poutcome)
boxplot(bank_data$duration)

#in box plot representation some variable has outlier
#here age variable has some outliers
#in age column above age above 70 considered as outliers
#so remove all those outliers from the data set

#following code shows the outliers present in the age columns
boxplot(bank_data$age, plot=FALSE)$out

#removing the outliers
outliers <- boxplot(bank_data$age, plot=FALSE)$out
x<- bank_data
x<- x[-which(x$age %in% outliers),]
bank_data<-x
boxplot(bank_data$age)

###SPLITTING THE DATA SET#############3

## create train and test data
install.packages("caTools")
library(caTools)

##use caTools function to split, SplitRatio for 70%:30% splitting

data= sample.split(bank_data,SplitRatio = 0.3)

## here I am using 70% of data for training and 30% data for testing
#subsetting into Test data
test =subset(bank_data,data==TRUE)

#subsetting into Train data
train=subset(bank_data,data==FALSE)
## check number of records present  in the data set
nrow(test)
nrow(train)
head(train)
head(test)

####################################################

# BUILD A MODEL###

#Output variable -> y IS Oor target variable
#we have to build a model that predict Whether the client has subscribed a term deposit or not 
#Binomial ("yes" or "no")

logistic_model <- glm(y~.,data=train,family = "binomial")
summary(logistic_model)

##predict the model#######

probability <- predict(logistic_model,type=c("response"),train)
View(probability)
head(probability,10)

###create confusion matrix table

confusion_matrix<-table(probability>0.5,train$y)
confusion_matrix
# here the probability value>0.5, classified as 1, else classified as 0

#check the  Model Accuracy 
Accuracy<-sum(diag(confusion_matrix)/sum(confusion_matrix))
Accuracy
# we get 89.35% accuracy

#check the error rate
1-Accuracy
#10.64% error rate

########ROC curve##########
#ROC curve is a metric describing the trade-off between 
#the sensitivity (true positive rate, TPR) and specificity (false positive rate, FPR)
#of a prediction in all probability cutoffs (thresholds). 
#It can be used for binary and multiclass classification accuracy checking.

install.packages("ROCR")
install.packages("ROCR", dependencies = T)
library(ROCR)

roc_prediction<-prediction(probability,train$y)

roc_performance<-performance(roc_prediction,'tpr','fpr')

plot(roc_performance,colorize=T,text.adj = c(0.5, 0.5))

# using ROC we can understand how good the model is.
# here  our model accuracy between 0.9 to 1, so we we can say that our model is outstanding

###########################################################

#Predict on Test data set


pred <- predict(logistic_model, newdata = test, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act<-test$y

# compare the predicted value in test_data
head(test)

test$predict_data<- y_pred
head(test)

#compare the accuracy of test data and actual data
mean(y_pred== y_act)       #89.66 %accuracy

#
new_test_data<-test[,c("y","predict_data")]
View(new_test_data)
head(new_test_data,10)

###CONCLUSION#####
#Our objective is to create a model for predicting 
#Whether the client has subscribed a term deposit or not
# here we create a good model with accuaracy 89.66%
#using this model we correctly predict test data
