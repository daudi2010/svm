require(kernlab)
library(ggplot2)
library(tidyverse)
library(readxl)

setwd("C:\\Users\\hp\\Downloads\\thesis")
print("Reading in xls file")
loan_clean <- read_excel(paste(getwd(),"loan_clean.xlsx",sep = "/"), 
                         col_types = c("numeric", "numeric", "numeric","numeric",
                                       "numeric", "numeric", "numeric","numeric",
                                       "numeric", "numeric", "numeric","numeric",
                                       "numeric", "numeric", "numeric","numeric"))
#View(loan_clean)
print("Clean data")
#remove nulls
data<- na.omit(loan_clean)


# define mahalnobis  distance function based kernel
#mahalanobis(data, colMeans(data), cov(data))

mahal<-function(data){
  k<-function(data,...)
  {
    #data<-as.data.frame(data)
    exp(-sum(mahalanobis(as.data.frame(data),colMeans(as.data.frame(data)),cov(as.data.frame(data))))) 
  }
  class(k)<-"kernel"
  k
}
  
# dependent variable
y<-data$loan_status

# move dependednt var to colum 1
data_ <- data[ , c("loan_status",    # Reorder data frame
                   names(data)[names(data) != "loan_status"])]

#divide data into training and test
print("creating tesst and training data")
samp_size <- floor(0.7 * nrow(data_))  #testing should be .7

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_)), size = samp_size)
#training set
train <- data_[train_ind, ]
#testing set
test <- data_[-train_ind, ]

print("Running model")
cred <- ksvm(loan_status~.,data=train,kernel=mahal,kpar=list(train), #type="C-svc",
             C=10,cross=10,)
 #perform predictions
print("perform predictions")
pred<-predict(cred,test)


