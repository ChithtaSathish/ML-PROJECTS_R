install.packages("psych")
library(psych)

#importing dataset
comp_datas <- read.csv(file.choose(),header = T)
View(comp_datas)

head(comp_datas)
#drop first column
comp_datas<-comp_datas[-1]
View(comp_datas)
head(comp_datas)
str(comp_datas)
#here we can see that three categorical variable in the data set. convert that variable into a numerical format
# here i am using label encoding
install.packages("plyr")
library(plyr)

# check the frequency of item in a categorical variable
count(comp_datas,"cd" )
count(comp_datas,"multi" )
count(comp_datas,"premium" )


colnames(comp_datas)
#variable "cd", "multi" , "premium" are categorical variable, convert them into numerical variable
comp_datas$cd<-revalue(comp_datas$cd,c("yes"="1", "no"="0"))
comp_datas$multi<-revalue(comp_datas$multi,c("yes"="1", "no"="0"))
comp_datas$premium<-revalue(comp_datas$premium,c("yes"="1", "no"="0"))
View(comp_datas)
head(comp_datas)

#change the character variable datatypes to numerical data
comp_datas$cd<- as.numeric(comp_datas$cd)
comp_datas$multi<- as.numeric(comp_datas$multi)
comp_datas$premium<- as.numeric(comp_datas$premium)
str(comp_datas)                      
                      
#check whether the data set contain null values
sum(is.na(comp_datas))  # data set contain zero null values

# summary of dataset 
summary(comp_datas)
attach(comp_datas)

# to number of rows
nrow(comp_datas)   #6259 rows
     
#Visualization
#Here we are using three types of analysis techniques:
  
#Univariate analysis

#Bivariate analysis

#Multivariate analys

#Univariate analysis
#Univariate analysis means one variable analysis.'Uni' means 'one' and 'variate' means'variable'. Univariate analysis is to analyse one variable or one features Univariate basically tells us how data in each feature is distributed and also tells us about central tendencies like mean, median, and mode.

#To do univariate data analysis we use following ploting mechanisms:
  
#Histograms

#Boxplot

#Histograms:
#following code shows histogram reprepresentation of variable in a dataset
hist(price)
hist(speed)
hist(hd)
hist(ram)
hist(screen)
hist(cd)
hist(ads)
hist(trend)

#boxplot
#box plot main used to identify the outliers
boxplot(trend)
boxplot(ads)
boxplot(price) # many outliers
boxplot(speed)
boxplot(ram)
boxplot(screen)

#Bivariate representation
#scatter plot
plot(trend,price)
plot(ads,price)
plot(speed,price)
plot(screen,price)

# in the scatter pot representation we can see that there is no linearity between dependent variable price and independent variable
#pair plot
pairs(comp_datas)

#using pairs.panels to show the correlation between variables
pairs.panels(comp_datas)

# here price is our target  variable, lets check the correlation between price and other variable
cor(comp_datas)

## create training and test data
install.packages("caTools")
library(caTools)

##use caTools function to split, SplitRatio for 70%:30% splitting

data= sample.split(comp_datas,SplitRatio = 0.3)

## here I am using 70% of data for training and 30% data for testing
#subsetting into Test data
test_data =subset(comp_datas,data==TRUE)

#subsetting into Train data
train_data =subset(comp_datas,data==FALSE)
## check number of records present  in the data set
nrow(test_data)
nrow(train_data)
head(train_data)
head(test_data)

# create first model

model1=lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=train_data)
summary(model1)     #Multiple R-squared:  0.7755,	Adjusted R-squared:  0.775 
# we can see that all variables are significant
#lets try to improve r squared value using different transformation methods

# check partial correlation
###Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(train_data))

# use some diagnostic plot to detect outliers
install.packages("car")
library(car)
plot(model1)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

# Deletion Diagnostics for identifying influential variable
influence.measures(model1)
influenceIndexPlot(model1) # Index Plots of the influence measures
influencePlot(model1)
#records 5961,4478 considered as outliers, so remove these records from dataset and create new model 

#create second  model
comp_model2=lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=train_data[-c(5961,4478),])
summary(comp_model2)   #Multiple R-squared:  0.7755,	Adjusted R-squared:  0.775
#there is no difference between model 1 and model 2, lets try another methods

#check multicollinearity present in a data set
# here using Variance Inflation Factors(VIF) technique helps to identify the multicollinearity
vif(model2)  # VIF is > 10 => collinearity
# here we get VIF values of all variable less than 10, so we can say that there is no multicollinearity present in our data set

#The Akaike information criterion (AIC) is a mathematical method for evaluating how well a model fits the data it was generated from. In statistics, AIC is used to compare different possible models and determine which one is the best fit for the data.
install.packages("MASS")
library("MASS")
stepAIC(model2)
## Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models

# here we can see that only one AIC value, so we can  take lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=train_data)

# to improve the R-squared values, here I am using different transformation techniques 

#exponential transformation


comp_model3<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=train_data[-c(5961,4478),])
summary(comp_model3)   #Multiple R-squared:  0.7836,	Adjusted R-squared:  0.7831

#square root transformation
comp_model4<-lm(sqrt(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=train_data[-c(5961,4478),])
summary(comp_model4)  #Multiple R-squared:  0.7856,	Adjusted R-squared:  0.7852 

#quadratic  model
comp_model5<-lm((price)~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)+ads+I(ads^2)+trend+I(trend^2),data=train_data[-c(5961,4478),])
summary(comp_model5)  #Multiple R-squared:  0.8045,	Adjusted R-squared:  0.8039

#polynomial model
comp_model6<lm((price)~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^2)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3),data=train_data[-c(5961,4478),])
summary(comp_model6)   #Multiple R-squared:  0.8077,	Adjusted R-squared:  0.8069

# here comp_model6 has highest R-squared	Adjusted R-squared values. so take comp_model6 for prediction
#prediction of price

comp_price_prediction<-predict(comp_model6,test_data)
head(comp_price_prediction)

# compare the predicted value in test_data
head(test_data)

comp_price_prediction<-predict(comp_model1,test_data)
head(comp_price_prediction)
head(test_data)

#
test_data$predicted_price<-comp_price_prediction
head(test_data)
head(test_data$predicted_price)

# check the correlation of actual price and predicted price
r<-cor(test_data$price,test_data$predicted_price)
r_squared<-cor(test_data$price,test_data$predicted_price)^2

# using ggplot, grapgically represent the actual price and predicted price
install.packages("ggplot2")
library(ggplot2)

                                      
ggplot(data=test_data,aes(x=price,color="actual price"))+
  geom_density(aes(x=price,color="actual price"))+
  geom_density(aes(x=predicted_price,color="predicted price"))+
  scale_color_manual(values=c("predicted price"="blue","actual price"="red"))+
  labs(title="density plt between the actual price and predicted price",
       x="price",y= "")
  
  
View(comp_datas)

