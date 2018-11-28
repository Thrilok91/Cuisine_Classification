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
getwd()
##Building a Corpus
#data<- read.table(DirSource("data",mode = "text"))
setwd("D:/Academic/MSU/SPRING 2017/CSE842/Final project/data")
corpus<-Corpus(DirSource("fulltext",mode = "text"), readerControl = list(language="en") )
#corpus2<-Corpus(DirSource("pos",mode = "text"), readerControl = list(language="en") )
#corpus<-c(corpus1,corpus2, recursive = FALSE)
#length(corpus)
data1<-unlist(corpus[[1]][1]) # unlisting the fist cusine and storing in data1

##using for loop to concaticate all the remainag cusine recipies into vector data1
for(i in 2:n) data1<-c(data1,unlist(corpus[[i]][1]))
names(data1)<-c(1:t)
corpus<-Corpus(VectorSource(data1))

#data1<-paste0(data,collapse =" ")
# Pre-process data
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
#corpus = tm_map(corpus, removeNumbers)
#corpus = tm_map(corpus, removeWords, stopwords("english"))
#corpus = tm_map(corpus, stemDocument)
#to.remove2 <- c("tabascoT")
#corpus<-tm_map(corpus,removeWords,to.remove2)

# create response vector
response_RR <-rep(1:n,each=l)
response_RR<-as.data.frame(response_RR)
rnames<-rownames(response_RR) ### vector of numbers to be used as rownames for data frame

# Create matrix
dtm = DocumentTermMatrix(corpus)

nword<-c()
sparcity<-seq(0.8,0.99,0.01)
split<-seq(0.7,0.92,0.02)
accuracy_NB<-matrix(NA,nrow=length(sparcity),ncol=length(split))
d=0
for (i in sparcity){
  # Remove sparse terms
  dtm1 = removeSparseTerms(dtm, i)
  
  d=d+1
  nword[d]<-ncol(dtm1)    
  
  # Create data frame with row names and add the response vectors with class code from 1 to n
  labeledTerms = as.data.frame(as.matrix(dtm1))     # turning the dtm to data frame
  rownames(labeledTerms)<-rnames                   #change the row names of dtm from "1" to "t"
  labeledTerms_new<-cbind(labeledTerms,response_RR)# adding the response column to the dtm
  # Split the data
  set.seed(100)
  p=0
  for(k in split)  { 
    p=p+1
    spl = sample.split(labeledTerms_new$response_RR, k)
    train = subset(labeledTerms_new, spl == TRUE)
    test = subset(labeledTerms_new, spl == FALSE)
    test1 = test
    test1[,ncol(test1)]<-NULL
    
    
    NB <- naiveBayes(as.factor(response_RR)~., data = train)
    
    # Make predictions on the test set
    pred_NB = predict(NB, newdata=test1,laplase=2)
   
     # Compute accuracy
    accuracy_table <- table(test$response_RR,pred_NB)
    accuracy_NB[d,p]=sum(diag(accuracy_table))/sum(accuracy_table) 
  }
}

rownames(accuracy_NB)<-sparcity
colnames(accuracy_NB)<-split
z=accuracy_NB
x<-seq(0.8,0.99,length.out = nrow(z))
y<-seq(0.7,0.92,length.out = ncol(z))
persp3D(x,y,z = accuracy_NB,shade = 0.5,cex.lab=1,cex.axis=0.75,
        theta =50, phi=45,ticktype = "detailed",expand = 0.5,
        lphi=45,ltheta = 90,xlab="Sparcity",ylab="Split",zlab="Accuracy",
        main="Accuracy with all verbatim in NB")
inds=which(accuracy == max(accuracy), arr.ind = TRUE)
max(accuracy)
sparcity(accuracy)[inds[,row()]]
split(accuracy)[inds[,2]]
inds <- arrayInd(which.max(accuracy), dim(accuracy))
