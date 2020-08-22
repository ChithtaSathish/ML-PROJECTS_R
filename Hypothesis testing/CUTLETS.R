# import csv files
cutlets<-read.csv(file.choose(),header = T)
View(cutlets)
attach(cutlets)

#in our data sets y is continous and x is discrete
#x= Unit.A and Unit.B
#Y= Values of X 
# in our data set gives two population

# compare two population with each other
#next step is to  check weather data is normalized or not using shapiro test
head(cutlets)
colnames(cutlets)

# we have to set hypothesis
#H0: Data is normally distributed(p>0.05)
#H1: not normally distributed (p<0.05)

shapiro.test(Unit.A)
# p- value=0.32 >0.05,so p high null fly. it follows normal distribution

shapiro.test(Unit.B)
# p- value=0.5225 >0.05,so p high null fly. it follows normal distribution

# so our data follows normal distribution

# next step is to check variance are equal
#calculate variance between two populations

# we have to set hypothesis

#H0: Variance(Unit.A) = variance(Unit.B)  (p>0.05)
#H1 : Variance(Unit.A) are not equal to variance(Unit.B)  (p<0.05)

var.test(Unit.A,Unit.B)   ##variance test
# p-value=0.3136 >0.05 so p high null fly means reject alternatete hypothesis, accept
#null hypothesis, that means variance between two population are equal


# here variance between two populations are equal, so perform 2 sample t test

######## 2 SAMPLE T TEST#########

# first we have to set nullhypothesis
#H0: mean(Unit.A) = mean(Unit.B)  #p>0.05
#H1: mean(Unit.A) not equal to mean(Unit. B)  #p<0.05

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct=TRUE)

#p-value=0.4723 >0.05, this mean we can accept our null hypothesis
# null hypothesis is mean(Unit.A) = mean(Unit.B)
# here we get equal mean between Unit.A and Unit.B, so we can conclude that there is no significant difference 
#in the diameter of the cutlet between two units.  
