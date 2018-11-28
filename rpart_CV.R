library(wordnet)
library(rpart)
library(randomForest)
library(NLP)
library(tm) #load text mining library
library(SnowballC)# used for stemming the words
library(caTools)# used for splitting the data, training and testing
library(kernlab)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)
library(plotly)
library(plot3D)


n=5##number of cusines
l=180## number of recipies in each
t=n*l##total number of recipies
setwd("D:/Academic/MSU/SPRING 2017/CSE842/Final project/data")
corpus<-Corpus(DirSource("verbnouns",mode = "text"), readerControl = list(language="en") )
##Building a Corpus

data1<-unlist(corpus[[1]][1]) # unlisting the fist cusine and storing in data1
##using for loop to concaticate all the remainag cusine recipies into vector data1
for(i in 2:n) data1<-c(data1,unlist(corpus[[i]][1]))
names(data1)<-c(1:t)
corpus<-Corpus(VectorSource(data1))
# Pre-process data
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords("english"))
#corpus = tm_map(corpus, stemDocument)

# create response vector
response_RR <-rep(1:n,each=l)
response_RR<-as.data.frame(response_RR)
rnames<-rownames(response_RR) ### vector of numbers to be used as rownames for data frame

# Create matrix
dtm = DocumentTermMatrix(corpus)

#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#dtm <- DocumentTermMatrix(corpus, control = list(tokenize=BigramTokenizer))
#dim(dtm)
#dtm = removeSparseTerms(dtm, 0.99)
## creatin gindecis for cross validation
address<-c(1:900)
add<-split(address, ceiling(seq_along(address)/30))
cd_test<-list()
for (i in 1:6){
  cd1<-add[seq(i,30,by=6)]
  cd1<- Reduce(c,cd1)
  cd_test[[i]]<-cd1
}
#dtm = removeSparseTerms(dtm, 0.99)
dim(dtm)
# Create data frame with row names and add the response vectors with class code from 1 to n
labeledTerms = as.data.frame(as.matrix(dtm))     # turning the dtm to data frame
rownames(labeledTerms)<-rnames                   #change the row names of dtm from "1" to "t"
labeledTerms_new<-cbind(labeledTerms,response_RR)# adding the response column to the dtm

accuracy_table <-list()
accuracy<-c()
for (i in 1:6){
  train=labeledTerms_new[-(cd_test[[i]]),]
  test = labeledTerms_new[(cd_test[[i]]),]
  test1 = test
  test1[,ncol(test1)]<-NULL
  reviewCART = rpart(response_RR~., data=train, method="class")
  # Make predictions on the test set
  pred = predict(reviewCART, newdata=test1)
  pred.prob<-c()
  for (k in 1:nrow(pred))  pred.prob[k]=which.is.max(pred[k,1:5])
  # Compute accuracy
  accuracy_table[[i]] <-table(test$response_RR, pred.prob)
  accuracy[i]<-sum(diag(accuracy_table[[i]]))/sum(accuracy_table[[i]])
}

mean(accuracy)
ind_acc<-list()
for (i in 1:6){
  ind_acc[[i]]<-diag(accuracy_table[[i]])/30
}
avg_indi<-(ind_acc[[1]]+ind_acc[[2]]+ind_acc[[3]]+ind_acc[[4]]+ind_acc[[5]]+ind_acc[[6]])/6
overall<-mean(accuracy)
print(avg_indi)
print(overall)

prp(reviewCART,type=1,extra=1,split.prefix="is ", split.suffix="?", split.box.col="lightgray",
    split.border.col="darkgray",type=)
prp(reviewCART,type=1,extra=1)

printcp(reviewCART)
plot(reviewCART)
text(reviewCART)


