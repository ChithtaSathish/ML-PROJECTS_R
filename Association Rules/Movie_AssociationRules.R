install.packages("arules") # install the package
library("arules") # invoke the package
 
#Visualizing Association Rules Package arulesViz supports visualization of 
#association rules with scatter plot, 
#graph, parallel coordinates plot, etc

install.packages("arulesViz")
library("arulesViz")

#importing data set 
Movie<-read.csv(file.choose(),header=T)
View(Movie)

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# here i am taking 6 to 15 column for creating rules, it is in binary format
# convert binary data int a matrix format using as. matrix
Movie_rules1 <-  apriori(as.matrix(Movie[,6:15]),parameter=list(support=0.01,confidence=0.7)) 
Movie_rules1

#here creating 79 rules

#display the rules using inspect command
inspect(Movie_rules1[1:10])

# check the presence of redundant rules
redundant_rule1<-is.redundant(Movie_rules1)
redundant_rule1
summary(redundant_rule1)

#remove the redundant rules
Movie_rules1<- Movie_rules1[!redundant_rule1]
Movie_rules1

# we can see that there we have 30 rules remains

#display first 10 rules by highest lift ratios
inspect(sort(Movie_rules1[1:10],by="lift"))


#visualize the rules
plotly_arules(Movie_rules1)

##grouped matrix for association rules
plot(Movie_rules1,method="grouped")

################################################################
#change support value and confident value

Movie_rules2 <-  apriori(as.matrix(Movie[,6:15]),parameter=list(support=0.2,confidence=0.8)) 
Movie_rules2

#here creating 8 rules

#display the rules using inspect command
inspect(Movie_rules2)

# check the presence of redundant rules
redundant_rule2<-is.redundant(Movie_rules2)
redundant_rule2
summary(redundant_rule2)

#remove the redundant rules
Movie_rules2<- Movie_rules2[!redundant_rule2]
Movie_rules2

# we can see that there we have 30 rules remains

#display  rules by highest lift ratios
inspect(sort(Movie_rules2,by="lift"))


#visualize the rules
plotly_arules(Movie_rules2)

##grouped matrix for association rules
plot(Movie_rules2,method="grouped")

##############################################################33

#change support value and confident value

Movie_rules3 <-  apriori(as.matrix(Movie[,6:15]),parameter=list(support=0.03,confidence=0.85)) 
Movie_rules3

#here creating 75 rules

#display the rules using inspect command
inspect(Movie_rules3[1:10])

# check the presence of redundant rules
redundant_rule3<-is.redundant(Movie_rules3)
redundant_rule3
summary(redundant_rule3)

#remove the redundant rules
Movie_rules3<- Movie_rules3[!redundant_rule3]
Movie_rules3

# we can see that there we have 30 rules remains

#display  rules by highest lift ratios
inspect(sort(Movie_rules3[1:10],by="lift"))


#visualize the rules
plotly_arules(Movie_rules3)

##grouped matrix for association rules
plot(Movie_rules3,method="grouped")

##############################################################################
#change support value and confident value

Movie_rules4 <-  apriori(as.matrix(Movie[,6:15]),parameter=list(support=0.02,confidence=0.90)) 
Movie_rules4

#here creating 75 rules

#display the rules using inspect command
inspect(Movie_rules4[1:10])

# check the presence of redundant rules
redundant_rule4<-is.redundant(Movie_rules4)
redundant_rule4
summary(redundant_rule4)

#remove the redundant rules
Movie_rules4<- Movie_rules4[!redundant_rule4]
Movie_rules4

# we can see that there we have 30 rules remains

#display  rules by highest lift ratios
inspect(sort(Movie_rules4[1:10],by="lift"))


#visualize the rules
plotly_arules(Movie_rules4)

##grouped matrix for association rules
plot(Movie_rules4,method="grouped")
####################################################################

#write the top 10 rules as csv files
write(sort(Movie_rules4[1:10],by="lift"), file="movies_rules.csv",sep=",")
