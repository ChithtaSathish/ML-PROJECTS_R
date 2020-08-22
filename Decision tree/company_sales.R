#importing data set
company_data<-read.csv(file.choose(),header = T)
View(company_data)

#display the structure of the data
str(company_data)

#here sales is our target variable and others are independent variables
#here sales are in numerical format
#so convert it into a categorical format

#lets check the data type of sales before conversion
is.numeric(company_data$Sales)
#here we get the output TRUE.
# so convert it into a categorical form
#here i am using cut method for conversion

company_data$Sales<- cut(company_data$Sales,breaks=c(-1,4,8,12,16),labels=c("poor","medium","good","Excellent"))
View(company_data)
head(company_data)
#here i divide the sales data set into four groups
#poor <- -1 to 4
#medium<-4 to 8
#good<-8 to 12
#excellent<-12 to 16

#lets check the data type of sales after conversion
is.numeric(company_data$Sales)
#here we get the output False, means sales is in categorical form

str(company_data)

#install.packages("C50",dependencies = T)
library(C50)

#data partition for model building and testing
#install.packages("caret")
library(caret)

c_data<-createDataPartition(company_data$Sales,p=.75,list = F)
View(c_data)

#create training and testing data set
c_train<-company_data[c_data,]
c_test<-company_data[-c_data,]

View(c_train)
head(c_test)
View(c_test)
head(c_test)

####MODEL BUILDING############

c_model<-C5.0(Sales~. ,data=c_train)
summary(c_model)
plot(c_model)


###PREDICTION##
c_train_pred<-predict.C5.0(c_model,c_train[,-1])
c_train_pred

c_test_pred<-predict.C5.0(c_model,c_test[,-1])
c_test_pred

#using table function compare orwith predicted original data with predicted data
x<-table(c_train$Sales,c_train_pred)
y<-table(c_test$Sales,c_test_pred)
x
y
#check the testing accuracy
sum(diag(y)/sum(y))
