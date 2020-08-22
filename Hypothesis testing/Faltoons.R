#import csv files

faltoons<-read.csv(file.choose(),header = T)
View(faltoons)
head(faltoons)
attach(faltoons)

# in our data set both x and y are discrete
# data set contains two proportions
# so we have to perform 2 proportion test

stack_data<-stack(faltoons)
View(stack_data)
head(stack_data,10)
attach(stack_data)


#create table
faltoons_table<-table(ind,values)
View(faltoons_table)

#perform 2 proportion test
#set hypothesis

#H0: proportion of males and females equal  => p-value>0.05
#H1: proportion of males and females  are different  => p-value<0.05

prop.test(x=c(233,113),n=c(520,250),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

# performing prop.test, we got p-value=0.9184 >0.05
#so we can say that there is no difference in males versus females walking in to the store 
# on day of the week
# we accept null hypothesis