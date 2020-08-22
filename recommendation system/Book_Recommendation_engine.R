#installing and loding the libraries
install.packages("recommender lab", dependencies = TRUE)
library(recommenderlab)
library(caTools)


#import books data set
books_data<- read.csv(file.choose(),header = T)
View(books_data)

book_data1<-books_data[-1]
View(book_data1)

#display the structure of the data set
str(books_data)

#check the null values present in the data set
#no null values present in the data set

#visualize rating distribution using histogram
hist(books_data$Book.Rating)
#in the histogram representation we can see that 8 is the first highest rate given by the reders
# second highest rate is 7
# very few people rateted less than 2

#the datatype should be realRatingMatrix inorder to build recommendation engine
#here i am removing first column in a data set
book_rate_data_matrix<- as(book_data1,'realRatingMatrix')

#next I am going to create recommendation system based on population criteria
books_recom_model1<-Recommender(book_rate_data_matrix,method="POPULAR")

#next is going to predict list movies using books_recom_model1
recommend_items1<-predict(books_recom_model1,book_rate_data_matrix[1],n=10)
as(recommend_items1,"list")

## Popularity model recommends the same movies for all users , 
#we need to improve our model using  Collaborative Filtering
book_recomm_model2 <- Recommender(book_rate_data_matrix, method="UBCF")

#Predictions 
recommended_items6 = predict(book_recomm_model2, book_rate_data_matrix[], n=10)
as(recommended_items6, "list")




