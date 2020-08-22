install.packages("arules") # install the package
library("arules") # invoke the package

#Visualizing Association Rules Package arulesViz supports visualization of 
#association rules with scatter plot, 
#graph, parallel coordinates plot, etc

install.packages("arulesViz")
library("arulesViz") # for visualizing rules


#importing data set
Groceryy<-read.transactions(file.choose())
View(Groceryy)
head(Groceryy)
# convert the data sets into discreate form, other we will get errors
#Grocery_df = as(data.frame(lapply(Grocery, as.character), stringsAsFactors=T), "transactions")
inspect(Groceryy[1:10])

## calculates support for frequent items
frequentItems <- eclat (Groceryy, parameter = list(supp = 0.07, maxlen = 15)) 
inspect(frequentItems)

# plot frequent items
itemFrequencyPlot(Groceryy, topN=10, type="absolute", main="Item Frequency") 

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# Building rules using apriori algorithm
Grocery_Rule <- apriori(Groceryy, parameter = list(support=0.01,confidence=0.7))
Grocery_Rule
# to view the rules we use inspect
inspect(head(sort(Grocery_Rule,by="lift")))
# here we got 7 rules, we have to increase the support value to get more rules

# to view the rules we use inspect
inspect(head(sort(Grocery_Rule,by="lift")))

#visualize the rules
plotly_arules(Grocery_Rule)

##grouped matrix for association rules
plot(Grocery_Rule,method="grouped")

#graph
plot(Grocery_Rule , method="graph", control=list(type="items"))

#parallel coordinates plot
plot(Grocery_Rule, method="paracoord", control=list(reorder=TRUE))

#####################################################################

#decrease support=0.001, confidence=0.8
Grocery_Rule1 <- apriori(Groceryy, parameter = list(support=0.001,confidence=0.8))
Grocery_Rule1
#display first few rules by highest lift ratios
inspect(head(sort(Grocery_Rule1,by="lift")))

#display fisr 10 rules with highest lift ratio
inspect(sort(Grocery_Rule1[1:10],by="lift"))

#lets check presence of redundant rules
Grocery_Rule1

redundant_rules<-is.redundant(Grocery_Rule1)
redundant_rules
summary(redundant_rules)
# here we can see that TRUE vale 120, means that 120 values are redundant
# we have to remove all redundant values
Grocery_Rule1<-Grocery_Rule1[!redundant_rules]
Grocery_Rule1
# we can see that there are 76 rules remains

inspect(sort(Grocery_Rule1[1:10],by="lift"))
# lhs(left hand side) means the product that is customer going to buy,
#rhs(right hand side) indicates the product to reccommend to customer

#visualize the rules
plotly_arules(Grocery_Rule1)

##grouped matrix for association rules
plot(Grocery_Rule1,method="grouped")

#graph
plot(Grocery_Rule1 , method="graph", control=list(type="items"))

#parallel coordinates plot
plot(Grocery_Rule1, method="paracoord", control=list(reorder=TRUE))

###################################################################

#lets change support value .0001, and confidence=.85
Grocery_Rule2 <- apriori(Groceryy, parameter = list(support=0.003,confidence=0.85))
Grocery_Rule2
#display the rules
inspect(Grocery_Rule2)

#display fisr 10 rules with highest lift ratio
inspect(sort(Grocery_Rule2[1:10],by="lift"))

#lets check presence of redundant rules
Grocery_Rule2

redundant_rules2<-is.redundant(Grocery_Rule2)
redundant_rules2
summary(redundant_rules2)
# here we can see that TRUE vale 10, means that 10 values are redundant
# we have to remove all redundant values
Grocery_Rule2<-Grocery_Rule2[!redundant_rules2]
Grocery_Rule2
# we can see that there we have 76 rules reamins

inspect(sort(Grocery_Rule2[1:10],by="lift"))
# lhs(left hand side) means the product that is customer going to buy,
#rhs(right hand side) indicates the product to reccommend to customer

#visualize the rules
plotly_arules(Grocery_Rule2)

##grouped matrix for association rules
plot(Grocery_Rule2,method="grouped")

#graph
plot(Grocery_Rule2 , method="graph", control=list(type="items"))

#parallel coordinates plot
plot(Grocery_Rule2, method="paracoord", control=list(reorder=TRUE))

#######################################################################
#write the top 10 rules as csv files, here i am taking Grocery_Rule1
write(sort(Grocery_Rule1[1:10],by="lift"), file="Grocery_rules.csv",sep=",")
