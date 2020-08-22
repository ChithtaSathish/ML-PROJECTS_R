#import the data set
LabTAT<-read.csv(file.choose(),header = T)
View(LabTAT)

#check weather the dat set contains null values
sum(is.na(LabTAT))  #zero null values

# shows first six datas
head(LabTAT)

#in our data sets y is continous and x is discrete
#x=  Laboratory.1 Laboratory.2 Laboratory.3 Laboratory.4
#Y= Values of X 
# in our data set gives Four population

# compare four population with each other
#next step is to  check weather data is normalized or not using shapiro test
attach(LabTAT)

# we have to set hypothesis
#H0: Data is normally distributed(p>0.05)
#H1: not normally distributed (p<0.05)

shapiro.test(Laboratory.1) # p- value=0.5508
shapiro.test(Laboratory.2) #p- value=0.8637
shapiro.test(Laboratory.3)  #p-value=0.4205
shapiro.test(Laboratory.4)  #p-value =0.6619

# in all case p- values>0.05, so accept null hypothesis, 
#that means all populations are normally distributed

# next step is to check the variance are equal
# here we have more than two population, so we have to perform Bartlett test 
#for for finding equal variance

#for doing bartlettest we have to perform some data manipulations
# we have to convert four column data sets into two column set using stack operations

#convert data set into a stacked data
stacked_data<-stack(LabTAT)
View(stacked_data)
head(stacked_data,10)
attach(stacked_data)

#check variance of all sample equal using bartlett.test
# we have to set hypothesis

#H0: Variance of sample are equal    # p-value>0.05
#H1 : Variance of sample are not equal   # p-value <0.05


bartlett.test(values~ind, data=stacked_data)
# p-value=0.1069 >0.05  so p high null fly means reject alternatete hypothesis, accept
#null hypothesis, that means variance between  populations are equal

#here we gets all samples variance are equal, so next step is to perform one way ANOVA test

# we have to set hypothesis

#H0:means sample are equal    # p-value>0.05
#H1 :meanssample are not equal   # p-value <0.05


anova_test<-aov(values~ind, data=stacked_data)         
summary(anova_test)

# here we get very small p-value<0.05, so we can say that mean of all samples are different
# so we have to conclude that there is   difference in average TAT among the different laboratories at 5% significance level.
