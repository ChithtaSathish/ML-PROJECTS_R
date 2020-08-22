#import csv files
BuyerRatios<-read.csv(file.choose(),header = T)
View(BuyerRatios)
head(BuyerRatio)
attach(BuyerRatio)

# here x= regions and y= male or female
# so x and y data are discrete
# in our data set we have four population-East,west,south,north
#so we have to perform chi-squared test

# we have to setup hypothesis
#H0: all proportions are equal
#H1: not all proportions are equal

#perform chi square test
chisq.test(BuyerRatio[,-1])

#here we got p-value=0.6603 =>   p-value >0.05
# accept null hypothesis
#so we have to conclude that male-female buyer rations are similar across regions