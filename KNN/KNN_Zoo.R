zoo<-read.csv(file.choose(),header = T)
View(zoo)

#shows the structure of the data set
str(zoo)

#table of our target variable 'type
table(zoo$type)

#here we can see that all variable are in numerical format
#convert  all those variable into text format for better understanding

# recode 'Type' as a factor
#following are the type of glasses
#1 -- Mammal
#2 --Bird
#3 --Reptiles
#4 --Fish
#5 --Frog
#6 --Insects
#7 --Marine

zoo$type <- factor(zoo$type, levels = c(1,2,3,4,5,6,7),
                     labels = c("Mammal", "Bird","Reptiles","Fish","Frog","Insects","Marine"))

table(zoo$type)
View(zoo)
head(zoo)
str(zoo)

#normalize the data using customized normalized function

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
zoo_n<-as.data.frame(lapply(zoo[2:17],normalize))
View(zoo_n)
head(zoo_n)
summary(zoo_n)

#SPLITTING THE DATA SET
#here randomly select testing and training data set
data<-sample(1:nrow(zoo_n),size=nrow(zoo_n)*0.7,replace = FALSE)

#Training and testing the dataset

zoo_train<-zoo_n[data,]
zoo_test<-zoo_n[-data,]

#creating training and testing labels

zoo_train_labels<-zoo[data,18]
zoo_test_labels<-zoo[-data,18]

#display first few rows of the traing and testing data set
head(zoo_train,10)
head(zoo_test,10)
head(zoo_train_labels,10)
head(zoo_test_labels,10)

#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

knn_zoo_pred1<-knn(train = zoo_train,test = zoo_test,cl = zoo_train_labels, k=1)
knn_zoo_pred1
zoo_test_labels
#calculate the proportion of correct classification for k=1
zoo_accur<-100*sum(zoo_test_labels==knn_zoo_pred1)/NROW(zoo_test_labels)
zoo_accur

#table representation of predicted and actual data
table(knn_zoo_pred,zoo_test_labels)

##--------Evaluating model performance ----

# load the "gmodels" library

library(gmodels)

CrossTable(x = zoo_test_labels, y = knn_zoo_pred)

#confusion matrix
install.packages("caret")
library(caret)

confusionMatrix(table(knn_zoo_pred1,zoo_test_labels))


#using a simple loop  we can find best k value
i=1
k_value=1
for(i in 1:25){
  knn.model<-knn(train = zoo_train, test = zoo_test,cl = zoo_train_labels, k=i)
  k_value[i]<-100*sum(zoo_test_labels == knn.model)/NROW(zoo_test_labels)
  k=i
  cat(k,"=",k_value[i],'\n')
 
}

#  plot the accuracy of k-value
plot(k_value, type="b", xlab="K- Value",ylab="Accuracy level") 

# create a dataframe contains, animal,test_label, predicted data
zoo_animals<-zoo[-data,1]
test_data<- -[data]
View(data.frame( zoo_animals,zoo_test_labels,knn_zoo_pred1)) 

# take k=25
knn_zoo_pred25<-knn(train = zoo_train,test = zoo_test,cl = zoo_train_labels, k=25)
knn_zoo_pred25
zoo_test_labels
#calculate the proportion of correct classification for k=1
zoo_accur<-100*sum(zoo_test_labels==knn_zoo_pred25)/NROW(zoo_test_labels)
zoo_accur

View(data.frame( zoo_animals,zoo_test_labels,knn_zoo_pred25)) 


