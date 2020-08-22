install.packages("psych")
library(psych)
Startups_data <- read.csv(file.choose(),header = T)
View(Startups_data)

#get column name of dataframe startup_data
colnames(Startups_data)

# get the structure of startup_data
str(Startups_data)

#here we have one categoriacal variable 'state', se we convert it into a numerical variable
install.packages("plyr")
library(plyr)
Startups_data$State<-revalue(Startups_data$State,c("New York"="0", "California"="1", "Florida"="2"))
View(Startups_data)
head(Startups_data)
# here state variable is in character data types, we have to convert it into a numerical form
Startups_data$State = as.numeric(Startups_data$State)
str(Startups_data)
View(Startups_data)

#check whether the data set contain null values
sum(is.na(Startups_data)) # data set contain zero null values

# summary of dataset 
summary(Startups_data)
attach(Startups_data)

# check the data is normally distributed or not
# using qqplot and shapiro test
qqnorm(R.D.Spend)
qqline(R.D.Spend)
shapiro.test(R.D.Spend)

qqnorm(Administration)
qqline(Administration)
shapiro.test(Administration) # pvalue=0.1801, if the p value>0.05, we can say that data is normally distributed

qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
shapiro.test(Marketing.Spend) #p value=0.3 so data is normally distributed


qqnorm(Profit)
qqline(Profit)
shapiro.test(Profit) #p value=0.7, so data is normally distributed

# Explore the data
pairs(Startups_data)# Scatter plot for all pairs of variables

#using pairs.panels to show the correlation between variables
pairs.panels(Startups_data)
 
# check correlation between target variable profit and independant variable using cor()
cor(Profit,R.D.Spend) #get 0.9729, highly correlated, so this variable important to get better model
cor(Profit,Marketing.Spend)  # get 0.74, better correlation
cor(Profit,Administration) # get 0.20, less correlation, the variable administration is not making any impact on model

# create first linear model using 
model1<-lm(Profit~R.D.Spend+Administration+State+Marketing.Spend,data=Startups_data ) 
summary(model1) #Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9464 
#Here only one variable R.D.Spend is significant, so we would individually check other variable significant or not

#calculate RMSE value
sqrt(sum(model1$residuals^2)/nrow(Startups_data))

model1_Adm<-lm(Profit~Administration,data=Startups_data)
summary(model1_Adm) # administration variable is not significant, so we can remove this variable from data set

model1_markng<-lm(Profit~Marketing.Spend,data=Startups_data)
summary(model1_markng) # we have checked individually , to get the marketing.spend variable is significant

# check partial correlation
###Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups_data))

# use some diagnostic plot to detect outliers
install.packages("car")
library(car)
plot(model1)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

# Deletion Diagnostics for identifying influential variable
influence.measures(model1)
influenceIndexPlot(model1) # Index Plots of the influence measures
influencePlot(model1)# A user friendly representation of the above
#records 49,50 considered as outliers, so remove these records from dataset and create new model 

#create second model
model2<-lm(Profit~R.D.Spend+Administration+State+Marketing.Spend,data=Startups_data[-c(49,50),])
summary(model2) #Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9592 

#check multicollinearity present in a data set
# here using Variance Inflation Factors(VIF) technique helps to identify the multicollinearity
vif(model2)  # VIF is > 10 => collinearity
# here we get VIF values of all variable less than 10, so we can say that there is no multicollinearity present in our data set

# another method to check the multicollinearity use added variable plot(AV plots) 
#### Added Variable Plots ######
avPlots(model2, id.n=5, id.cex=100, col="red")

#The Akaike information criterion (AIC) is a mathematical method for evaluating how well a model fits the data it was generated from. In statistics, AIC is used to compare different possible models and determine which one is the best fit for the data.
install.packages("MASS")
library("MASS")
stepAIC(model1) # backward
# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.
#Profit ~ R.D.Spend + Marketing.Spend, this combination got lowest AIC value, so choose
#this combination for prediction


model3<-lm( Profit ~ R.D.Spend + Marketing.Spend, data = Startups_data[-c(49,50), ]) 
summary(model3)   #Multiple R-squared:  0.9614,	Adjusted R-squared:  0.9596 
vif(model3)
# to improve the R-squared values, here I am using different transformation techniques 
#logarithmic transformation
model4<-lm( Profit ~ log(R.D.Spend) + log(Marketing.Spend), data = Startups_data[-c(49,50), ]) 
summary(model4) #Multiple R-squared:  0.9609,	Adjusted R-squared:  0.9592

#exponential transformation
model5<-lm( log(Profit) ~ R.D.Spend + Marketing.Spend, data = Startups_data[-c(49,50), ]) 
summary(model5) #Multiple R-squared:  0.9221,	Adjusted R-squared:  0.9187 

#square root transformation
model6<-lm( sqrt(Profit) ~ R.D.Spend + Marketing.Spend, data = Startups_data[-c(49,50), ]) 
summary(model6)     #Multiple R-squared:  0.9541,	Adjusted R-squared:  0.952 

# polynomial model
model7<-lm( Profit ~ R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3) + Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3), data = Startups_data[-c(49,50), ]) 
summary(model7)   #Multiple R-squared:  0.9652,	Adjusted R-squared:  0.9601

#quadratic  model
model8<-lm( Profit ~ R.D.Spend+I(R.D.Spend^2) + Marketing.Spend+I(Marketing.Spend^2), data = Startups_data[-c(49,50), ]) 
summary(model8) #Multiple R-squared:  0.962,	Adjusted R-squared:  0.9585 

# now we choose best model for prediction

# model1:Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9464
# model2:Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9592 
# model3:Multiple R-squared:  0.9614,	Adjusted R-squared:  0.9596
# model4:Multiple R-squared:  0.9609,	Adjusted R-squared:  0.9592
# model5:Multiple R-squared:  0.9221,	Adjusted R-squared:  0.9187
# model6:Multiple R-squared:  0.9541,	Adjusted R-squared:  0.952 
# model7:Multiple R-squared:  0.9652,	Adjusted R-squared:  0.9601
# model8:Multiple R-squared:  0.962,	Adjusted R-squared:  0.9585

# here we can see that model7 has higher adjusted R-squared value. so we can choose model as our final model 7 for predict the profit

final_model<-model7
summary(final_model)

#prediction
confint(final_model,level = 0.95)
profit_pred<-predict(final_model,interval = "predict")
profit_pred

Final_data <- cbind(Startups_data$R.D.Spend,Startups_data$Administration,Startups_data$Marketing.Spend,Startups_data$State,Startups_data$Profit,profit_pred)
View(Final_data)
 


 