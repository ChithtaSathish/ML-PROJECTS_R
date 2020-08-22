#import csv files
Customer<-read.csv(file.choose(),header = T)
View(Customer)
head(Customer)
colnames(Customer)
 
#convert data set into a stacked data

stacked_datas<-stack(Customer)
View(stacked_datas)
head(stacked_datas,10)
attach(stacked_datas)

# in our data set both x and y are discrete
# data set contains four proportions
# so we have to perform chisquare test

cust_table<-table(stacked_datas$ind,stacked_datas$values)
View(cust_table)

#perform chi-square test

# set hypothesis
#H0: defective %  not varies by centre  => p- value>0.05
#H1: defective %  varies by centre   => p-value<0.05


chisq.test(cust_table)

# here we got p-value =0.2771 >0.005  => accept null hypothesis

# so we have conclude that defective %  not varies by centre
