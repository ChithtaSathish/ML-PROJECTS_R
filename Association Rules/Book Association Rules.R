install.packages("arules") # install the package
library("arules") # invoke the package
 
#Visualizing Association Rules Package arulesViz supports visualization of 
#association rules with scatter plot, 
#graph, parallel coordinates plot, etc

install.packages("arulesViz")
library("arulesViz") # for visualizing rules

 
#importing data set
Books<-read.csv(file.choose(),header = T)
View(Books)
head(Books)
str(Groceries)
#check how many columns presents in data set
colnames(Books)

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

Books_rules <-  apriori(as.matrix(Books,parameter=list(support=0.002,confidence=0.7)))
inspect(Books_rules)                        

# here we got 7 rules

#sorting the rules by lift ratio
inspect(sort(Books_rules,by="lift"))

# here we consider top 5 rules based on highest lift ratio

#visualize the rules
plotly_arules(Books_rules)

##grouped matrix for association rules
plot(Books_rules,method="grouped")

#graph
plot(Books_rules , method="graph", control=list(type="items"))

#parallel coordinates plot
plot(Books_rules, method="paracoord", control=list(reorder=TRUE))
###########################################################################

#next we can try support=0.11, confidence=0.85

Books_rules2 <-  apriori(as.matrix(Books,parameter=list(support=0.11,confidence=0.85)))
#sorting the rules by lift ratio
inspect(sort(Books_rules2,by="lift"))

# here we consider top 5 rules based on highest lift ratio

#visualize the rules
plotly_arules(Books_rules2)

##grouped matrix for association rules
plot(Books_rules2,method="grouped")

#graph
plot(Books_rules2 , method="graph", control=list(type="items"))

#parallel coordinates plot
plot(Books_rules2, method="paracoord", control=list(reorder=TRUE)) 

###########################################################################

#next we can try support=0.12, confidence=0.95
Books_rules3 <-  apriori(as.matrix(Books,parameter=list(support=0.12,confidence=0.95)))
inspect(Books_rules3)
inspect(sort(Books_rules3,by="lift"))

#visualize the rules
plotly_arules(Books_rules3)

# Different Ways of Visualizing Rules
plot(Books_rules3,method="grouped")

plot(Books_rules3, method="paracoord", control=list(reorder=TRUE))

plot(Books_rules3, method="graph", control=list(type="items"))

#write the rules as csv files
write(Books_rules3, file="Book_rules.csv",sep=",")

