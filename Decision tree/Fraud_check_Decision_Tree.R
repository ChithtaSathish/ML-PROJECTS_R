#importing data set
fraud_data<-read.csv(file.choose(),header = T)
View(fraud_data)
str(fraud_data)
#check any null values present in the data set
sum(is.na(fraud_data))  #no null values

#here Taxable.Income is our target variable, 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"
#convert Taxable.Income as categorical form

#lets check the data type of Taxable.Income before conversion
is.numeric(fraud_data$Taxable.Income)
#here we get the output TRUE.
# so convert it into a categorical form
#here i am using cut method for conversion
fraud_data$Taxable.Income<- cut(fraud_data$Taxable.Income,breaks=c(0,30000,200000),labels=c("Risky","Good"))
View(fraud_data)

##lets check the data type of Taxable.Income after conversion
is.numeric(fraud_data$Taxable.Income)
#here we get the output False, means its in categorical form


str(fraud_data)

install.packages("C50",dependencies = T)
library(C50)

#data partition for model building and testing
install.packages("caret")
library(caret)

datas<-createDataPartition(fraud_data$Taxable.Income,p=.75,list = F)
View(datas)

#create training and testing data set
training<-fraud_data[datas,]
testing<-fraud_data[-datas,]

View(training)
head(training)
View(testing)
head(testing)


####MODEL BUILDING############

model<-C5.0(Taxable.Income~. ,data=training)
summary(model)
plot(model)


###PREDICTION##
train_pred<-predict.C5.0(model,training[,-3])
train_pred

test_pred<-predict.C5.0(model,testing[,-3])
test_pred

#using table function compare orwith predicted original data with predicted data
a<-table(training$Taxable.Income,train_pred)
b<-table(testing$Taxable.Income,test_pred)

#check the testing accuracy
sum(diag(b)/sum(b))
