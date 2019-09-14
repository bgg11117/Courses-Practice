#rm(list=ls(all=TRUE))
library(e1071)
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lattice)
library(stringi)
library(quantreg)
library(SparseM)
library(caret)
library(partykit)
library(grid)
library(C50)
library(rpart)
library(ipred)
library(MASS)
library(kernlab)
AAPL = read.csv('Youtubelist.csv')




#movie predict - svm

#data processing
AAPL.svm = AAPL[,-c(1,2,3,10)]#Range to process

for(i in 1:7)
{AAPL.svm = AAPL.svm[!is.na(AAPL.svm[,i]),]}
AAPL.svm$Y = c(rep(0,length(AAPL.svm$Box)))
ranklist = c(-1,7e6L,3e7L,7e7L,1e8L)
ranklist = as.numeric(ranklist)
for(i in 2:length(ranklist))
{
  AAPL.svm$Y[AAPL.svm$Box<=ranklist[i] & AAPL.svm$Box>ranklist[i-1]] = i-1
}
AAPL.svm$Y[AAPL.svm$Box>ranklist[length(ranklist)]] = length(ranklist)

AAPL.svm = AAPL.svm[,-1]
AAPL.svm= as.data.frame(AAPL.svm)
AAPL.svm$Y = as.factor(AAPL.svm$Y)
AAPL.svm$Youtube.Views = scale(AAPL.svm$Youtube.Views)
AAPL.svm$Runtime = scale(AAPL.svm$Runtime)

set.seed(123)
train_sample <- sample(nrow(AAPL.svm),nrow(AAPL.svm)*0.8)
AAPL.svm_train <- AAPL.svm[train_sample,]
AAPL.svm_test <- AAPL.svm[-train_sample,]

#evaluating model performance

#tuned = tune.svm(Y ~ ., data = AAPL.svm_train, gamma = 2^(-7:-5), cost = 2^(2:4))
#summary(tuned)
svm.model = svm(Y ~ ., data = AAPL.svm_train, kernal='radial', type = 'C-classification'
                , cost = 16, gamma = 0.03125)

AAPL.svm_pred = predict(svm.model, AAPL.svm_test)
table(AAPL.svm_pred, AAPL.svm_test$Y)

correction <- AAPL.svm_pred == AAPL.svm_test$Y
prop.table(table(correction)) #accuracy 76%

#bonus
control <- trainControl(method = 'repeatedcv',number=10,repeats = 3)

#Cart
set.seed(300)
fit.cart <- train(Y~., data = AAPL.svm , method = 'rpart',trControl = control)

#LDA
set.seed(300)
fit.lda <- train(Y~., data = AAPL.svm , method = 'lda',trControl = control)

#Ksvm
set.seed(300)
fit.ksvm <- train(Y~., data = AAPL.svm , method = 'svmRadial',trControl = control)

#KNN
set.seed(300)
fit.knn <- train(Y~., data = AAPL.svm , method = 'knn',trControl = control)

#RandomForest
set.seed(300)
fit.rf <- train(Y~., data = AAPL.svm , method = 'rf',trControl = control)

#collect resamples
results <- resamples(list(CART = fit.cart , LDA = fit.lda , KSVM= fit.ksvm,
                          KNN = fit.knn , RF = fit.rf))

summary(results)





#simple tuned method (C5.0)
set.seed(123)
AAPL.c5_train <- train(Y~., data = AAPL.svm_train , method = 'C5.0')
AAPL




#ggplot analysis

#直方圖(1個x則對應的y是count)----------------------------------------------
ggplot(AAPL.svm,aes(x=factor(MPAA)))+geom_bar(aes(fill=factor(MPAA)))+
  xlab("MPAA")+ylab("# of MPAAs")+ggtitle("No. of MPAA from 1980-2016")+
  theme(axis.text.x = element_text(angle=45,vjust=0.8,face= 'italic',size =12))
#點圖---------------------------------------------
ggplot(AAPL.svm,aes(x=Runtime,y=Youtube.Views,col=Y,size=Y))+geom_point()+
  xlab("runtime")+ylab("youtubeviews")  
#facet wrap直接分類，Y contains how much MPAA----------------------
ggplot(AAPL.svm,aes(x=factor(MPAA)))+geom_bar(aes(fill=MPAA))+
  facet_wrap(~Y)+xlab("MPAA")+ylab("No. of Y")+ggtitle("Types of MPAA and Y")+
  theme(axis.text.x = element_text(angle=45,vjust=0.8,face= 'italic',size =12))
#MPAA、Genre造成Y的結果，coord好處是看得清楚座標--------------
ggplot(data = AAPL.svm, aes(x = factor(Genre), fill=Y)) +
  geom_bar(aes(fill=factor(Y)), width=1) +
  facet_wrap(~ MPAA) +
  ggtitle("MPAA and Genres") + coord_flip()+
  xlab("MPAA") + ylab("No of Y's")
#------------Which MPAA causes how much Y----------------
ggplot(AAPL.svm,aes(x=Y))+geom_bar(aes(fill=factor(Y)))+
  facet_wrap(~MPAA)

scatter3d(x = AAPL$Release.Date, y = AAPL$Runtime, z = AAPL$MPAA,
          groups =AAPL$Y,surface=FALSE,ellipsoid = TRUE)


