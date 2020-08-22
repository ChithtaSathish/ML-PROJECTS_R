
#importing data set
glass<-read.csv(file.choose(),header = T)
View(glass)

#display top 10 records
head(glass,10)

#display the column name
colnames(glass)

#shows the structure of the data set
str(glass)

#check whether any null values present in the data set
sum(is.na(glass))

#here we have to Prepare a model for glass classification using KNN
# so 'Type' is our target or label or dependent variable
#using available information we have to classify whether new glass lies which type

#table of our target variable 'Type
table(glass$Type)

#here we can see that all variable are in numerical format
#convert  all those variable into text format for better understanding

# recode 'Type' as a factor
#following are the type of glasses
#1 -- building_windows_float_processed  <- T1
#2 --building_windows_non_float_processed  <-  T2
#3 --vehicle_windows_float_processed <-  T3
#4 --vehicle_windows_non_float_processed <-T4
#5 --containers  <-T5
#6 --tableware  <- T6
#7 --headlamps  <- T7


glass$Type <- factor(glass$Type, levels = c(1,2,3,4,5,6,7),
                           labels = c("T1", "T2","T3","T4","T5","T6","T7"))

table(glass$Type)
View(glass)
str(glass)

# use the scale() function to z-score standardize a data frame
glass_z <- as.data.frame(scale(glass[-10]))
View(glass_z)
head(glass_z)
#SPLITTING THE DATA SET

set.seed(123)
datas<-sample(1:nrow(glass_z),size=nrow(glass_z)*0.7,replace = FALSE)

#Creating training and testing data set
train_glass<-glass_z[datas,]
test_glass<-glass_z[-datas,]

head(train_glass)
head(test_glass)

#creating training and testing labels
train_glass_label<-glass[datas,10]
test_glass_label<-glass[-datas,10]
head(train_glass_label)
head(test_glass_label)

#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

knn_glass_pred1 <- knn(train = train_glass, test = test_glass,
                   cl = train_glass_label, k=1)
knn_glass_pred1

#calculate the proportion of correct classification for k=1
accur<-100*sum(test_glass_label==knn_glass_pred1)/NROW(test_glass_label)
accur

table(knn_glass_pred2,test_glass_label)

##--------Evaluating model performance ----

# load the "gmodels" library

library(gmodels)

CrossTable(x = test_glass_label, y = knn_glass_pred2)

#confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(table(knn_glass_pred3,test_glass_label))

#using a simple loop we can find best k value
i=1
k_value=1
for(i in 1:25){
  knn.models<-knn(train = train_glass, test = test_glass,cl = train_glass_label, k=i)
  k_value[i]<-100*sum(test_glass_label == knn.models)/NROW(test_glass_label)
  k=i
  cat(k,"=",k_value[i],'\n')
  
}
#  plot the accuracy of k-value
plot(k_value, type="b", xlab="K- Value",ylab="Accuracy level")  

#final dataframe of k=1
View(data.frame(test_glass_label,knn_glass_pred1))

# take k=5
knn_glass_pred5 <- knn(train = train_glass, test = test_glass,
                       cl = train_glass_label, k=5)
knn_glass_pred5

#calculate the proportion of correct classification for k=1
accur<-100*sum(test_glass_label==knn_glass_pred5)/NROW(test_glass_label)
accur

table(knn_glass_pred5,test_glass_label)

#create data frame using knn_glass_pred5
head(data.frame(test_glass_label,knn_glass_pred5))

#conclusion:
#here we are plotting the the accuracy of different k value
# k=1 will give the better accuracy
