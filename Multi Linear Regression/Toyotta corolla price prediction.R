install.packages("psych")
library(psych)

#importing dataset
Toyotta_corolla <- read.csv(file.choose(),header = T)
View(Toyotta_corolla)
nrow(Toyotta_corolla)
str(Toyotta_corolla)
# in our data set contain 38 bariables and 1436 records

#here we are taking few variable for price prediction
Corolla<-Toyotta_corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
head(Corolla)
# here price is our target or dependant variable, others are our independant variable

#seeing the summary and structure and checking the aspects of data set
str(Corolla)
nrow(Corolla)
summary(Corolla)

#Checking number of na values per each columns in the corolla data frame
colSums(is.na(Corolla),na.rm = FALSE)  # all columns contain zero null values

attach(Corolla)

########VISULIZATION########
#Here we are using two types of visualization methods
#UNIVARIATE AND BIVARIATE ANALYSIS

#UNIVARIATE ANALYSIS

#Univariate analysis means one variable analysis.'Uni' means 'one' and 'variate'
#means'variable'. Univariate analysis is to analyse one variable or 
#one features Univariate basically tells us how data in
#each feature is distributed and also tells us about central tendencies 
#like mean, median, and mode.

#To do univariate data analysis we use following ploting mechanisms:
  
#Histograms

#Boxplot

install.packages("ggplot2")
library(ggplot2)

#histogram of price
ggplot(Corolla, aes(x = Price)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "Price",
       y = "Count of Values")

# here we can see that most of the price is in the range of 10000 to 20000  euros

#histgram of Age_08_04 
ggplot(Corolla, aes(x = Age_08_04)) +                           
  geom_histogram(col = "red") +
  
  labs(title = "My ggplot2 Histogram",
       x = "age",
       y = "Count of Values")

#histogram of KM
ggplot(Corolla, aes(x = KM)) +                           
  geom_histogram(col = "blue") +
  
  labs(title = "My ggplot2 Histogram",
       x = "KM",
       y = "Count of Values")   ##Accumulated Kilometers on odometer(KM) all in the range of 50000 to 150000

#histogram of HP
ggplot(Corolla, aes(x = HP)) +                           
  geom_histogram(bins=10,col = "blue") +
  
  labs(title = "My ggplot2 Histogram",
       x = "HP",
       y = "Count of Values")  ##we can see that most of the carhorse power is 100 to 110 range

#histogram of cc
ggplot(Corolla, aes(x = cc)) +                           
  geom_histogram(bins=5,col = "blue") +
  scale_x_continuous(breaks = seq(0,max(Corolla),2000))+
  
  labs(title = "My ggplot2 Histogram",
       x = "CC",
       y = "Count of Values")  ## Cylinder Volume in cubic centimeters (cc) 
#here most of the car cc is in the range of 1000 to 2000 


#histogram of Doors
 
hist(Corolla$Doors) # here most of the cars doors 3 or 5

hist(Corolla$Gears) #  Number of gear positions is 5

#histogram of Quarterly_Tax
ggplot(Corolla, aes(x = Quarterly_Tax)) +                           
  geom_histogram(bins=5,col = "blue") +
  labs(title = "My ggplot2 Histogram",
       x = "Quarterly_Tax",
       y = "Count of Values") # Quarterly_Tax is in the range of 150 to 250 euros

#histogram of Weight
ggplot(Corolla, aes(x = Weight)) +                           
  geom_histogram(bins=5,col = "blue") +
  labs(title = "My ggplot2 Histogram",
       x = "Weight",
       y = "Count of Values")  ##most of the car weight is in the range of 1000 to 1200

#Boxplot
#Boxplot is used for numerical data representations.in Boxplot data representation is in quartile basis. a boxplot contains five parts:Minimum, First quartile(25% of data),
#second quartile(50% of data, also called median),Third quartile(75% of data) 
#and maximum.If data below the minimum boundaries and above the maximum boundaries 
#considered as Outliers. Main task of boxplot is to detect outliers.

boxplot(Corolla$Price)  # we can see that  price variable has many outliers 

boxplot(Corolla$Age_08_04) # few outliers detected

boxplot(Corolla$KM) #many outliers

boxplot(Corolla$HP)  ##one outliers

boxplot(Corolla$cc) #one ouliers

boxplot(Corolla$Quarterly_Tax) # many outliers are detected

boxplot(Corolla$Weight)  # many outliers are detected

# here we  see that boxplot representation of varible, and also detects many outliers
# for better prediction we have to remove those outliers from our data set

##BIVARIATE ANALYSIS###

#scatter plot



 


#pair plot

#here i am dividing data sets in to two parts for more convenience to visualize pair plot
pairs(Corolla[c("Price","Age_08_04","KM","HP","cc")]) # in this plot we can see that 
# KM and Age_08_04 has some linear relation ship between price, but other variables such as
# HP,cc has not make any impact on price

pairs(Corolla[c("Price","Doors","Gears","Quarterly_Tax","Weight")])
#here only Weight has form a linear relationship 
#with respect to price

#using pairs.panels to show the correlation between variables
pairs.panels(Corolla[c("Price","Age_08_04","KM","HP","cc")])
pairs.panels(Corolla[c("Price","Doors","Gears","Quarterly_Tax","Weight")])

# here price is our target  variable, lets check the correlation between price and other variable
cor(Corolla)

# check partial correlation
###Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla))

## create training and test data
install.packages("caTools")
library(caTools)

##use caTools function to split, SplitRatio for 70%:30% splitting

data= sample.split(Corolla,SplitRatio = 0.3)

## here I am using 70% of data for training and 30% data for testing
#subsetting into Test data
test =subset(Corolla,data==TRUE)

#subsetting into Train data
train_datas =subset(Corolla,data==FALSE)
## check number of records present  in the data set
nrow(test)
nrow(train_datas)
head(train_datas)
head(test)

# create first model
Corolla_model1<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=train_datas)
summary(Corolla_model1)    ##Multiple R-squared:  0.867,	Adjusted R-squared:  0.866

#here we can see that  gear,cc,Doors  Quarterly_Tax   variables are not significant
#lets check individually those variables  significant or not

Corolla_model1_gear<-lm(Price~Gears,data=train_datas)
summary(Corolla_model1_gear)

Corolla_model1_Tax<-lm(Price~Quarterly_Tax,data=train_datas)
summary(Corolla_model1_Tax)

# check individually we can see that gear is a significant variable

# lets perform another analysis to improve the model performance

# use some diagnostic plot to detect outliers
install.packages("car")
library(car)
plot(Corolla_model1)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

# residual vs fitted plot showing records 602,222,148 are outliers
# normal Q-Q plot showing records 602,222,81 are outliers
#Std. Residuals vs Fitted showing records 602,81,222 are outliers
#Cook's distance showing records 602,222,81 are outliers

# in all plots 602,81,222 are considered outliers, so remove all those records
#from our training data set to improve model performance

#create second model,remove the otliers
Corolla_model2<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=train_datas[-c(222,81,602),])
summary(Corolla_model2)    ##Multiple R-squared:  0.866,	Adjusted R-squared:  0.8656 

##check multicollinearity present in a data set
# here using Variance Inflation Factors(VIF) technique helps to identify the multicollinearity
vif(Corolla_model2)  # VIF is > 10 => collinearity
# here we get VIF values of all variable less than 10, so we can say that there is no multicollinearity present in our data set

#The Akaike information criterion (AIC) is a mathematical method for evaluating 
#how well a model fits the data it was generated from. 
#In statistics, AIC is used to compare different possible models and determine 
#which one is the best fit for the data.

install.packages("MASS")
library("MASS")
stepAIC(Corolla_model2)


## Lower the AIC (Akaike Information Criterion) value better is the model.
#AIC is used only if you build multiple models

#here we get four AIC model, if choose lowesr AIC model  value for prediction
> 
  
  #following are the AIC model, we can choose lowest AIC model
  
  #AIC=16061.89
#Price ~ Age_08_04 + KM + HP + cc + Doors + Gears + Quarterly_Tax + Weight


#AIC=16059.9
#Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight
  
  # here AIC=16059.9 is the lowest value, so choose this model for prediction  

Corolla_model3<-lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight,data=train_datas[-c(222,81,602),] )
summary(Corolla_model3) #Multiple R-squared:  0.8666,	Adjusted R-squared:  0.8657



# perform other transformmation methods to improve r-squared values

#logerithamic transformation



Corolla_model4<-lm(Price ~ log(Age_08_04) +log( KM) + log(HP) + log(cc)+ log(Gears)+log(Quarterly_Tax) + log(Weight),data=train_datas[-c(222,81,602),]) 
summary(Corolla_model4)  ##Multiple R-squared:  0.8424,	Adjusted R-squared:  0.8414 

#exponential transformation
Corolla_model5<-lm(log(Price) ~ Age_08_04 + KM + HP + cc+ Gears +Quarterly_Tax+ Weight,data=train_datas[-c(222,81,602),]) 
summary(Corolla_model5)    ##Multiple R-squared:   0.8484,	Adjusted R-squared:  0.8475

#square root transformation
Corolla_model6<-lm(sqrt(Price) ~ Age_08_04 + KM + HP +cc+ Gears +Quarterly_Tax+ Weight,data=train_datas[-c(222,81,602),]) 
summary(Corolla_model6) ##Multiple R-squared:  0.8667,	Adjusted R-squared:  0.8659

#quadratic model
Corolla_model7<-lm(Price ~ Age_08_04+I(Age_08_04)^2 + KM+I(KM)^2 + HP+I(HP)^2 +cc+I(cc)^2 + Gears+I(Gears)^2 +Quarterly_Tax+I(Quarterly_Tax)^2+ Weight+I(Weight)^2,data=train_datas[-c(222,81,602),]) 
summary(Corolla_model7) ##Multiple R-squared:  0.8666,	Adjusted R-squared:  0.8657 

#polynomial model
Corolla_model8<-lm(Price ~ Age_08_04+I(Age_08_04)^2+I(Age_08_04)^3 + KM+I(KM)^2+I(KM)^3 + HP+I(HP)^2+I(HP)^3 +cc+I(cc)^2+I(cc)^3 + Gears+I(Gears)^2+I(Gears)^3 +Quarterly_Tax+I(Quarterly_Tax)^2+ I(Quarterly_Tax)^3+Weight+I(Weight)^2+I(Weight)^3,data=train_datas[-c(222,81,602),]) 
summary(Corolla_model8)  ##Multiple R-squared:  0.8666,	Adjusted R-squared:  0.8657 

#Corolla_model1:Multiple R-squared:  0.867,	Adjusted R-squared:  0.866
#Corolla_model2:Multiple R-squared:  0.866,	Adjusted R-squared:  0.8656 
#Corolla_model3:Multiple R-squared:  0.8666,	Adjusted R-squared:  0.8657 
#Corolla_model4:Multiple R-squared:  0.8424,	Adjusted R-squared:  0.8414
#Corolla_model5:Multiple R-squared:  0.8484,	Adjusted R-squared:  0.8475
#Corolla_model6:Multiple R-squared:  0.8667,	Adjusted R-squared:  0.8659
#Corolla_model7:Multiple R-squared:  0.8666,	Adjusted R-squared:  0.8657
#Corolla_model8:Multiple R-squared:  0.8666,	Adjusted R-squared:  0.8657

# here we can see that  model1  has highest r squared value
# lets take model1
#here i am taking Corolla_model1 for prediction

#prediction

price_predn<-predict(Corolla_model1,test)
head(price_predn)

# compare the predicted value in test_data
head(test_data1)

test_data1$predict_price<-price_predn
head(test_data1)
head(test_data1$predict_price)

# check the correlation of actual price and predicted price
r<-cor(test_data1$Price,test_data1$predict_price)
r_squared<-cor(test_data1$Price,test_data1$predict_price)^2

View(r_squared)  

# using ggplot, grapgically represent the actual price and predicted price
 


ggplot(data=test_data1,aes(x=Price,color="actual price"))+
  geom_density(aes(x=Price,color="actual price"))+
  geom_density(aes(x=predict_price,color="predicted price"))+
  scale_color_manual(values=c("predicted price"="blue","actual price"="red"))+
  labs(title="density plt between the actual price and predicted price",
       x="price",y= "")
# in this plot showing there are lot of variation between actual price and predicted price

ggplot(Corolla, aes(x = Age_08_04, y = Price)) +
  geom_point(aes(color =factor(Gears) ))

ggplot(Corolla, aes(x=Weight, y=Price, color="green")) +
  geom_point()
             
 
             