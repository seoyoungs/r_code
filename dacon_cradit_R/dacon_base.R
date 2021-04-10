options(warn=-1)
# =========== library
#R version 4.0.2
# install.packages(c("tidyverse","dplyr","caret","Metrics","ggplot2","xgboost","data.table","mltools"))
library(tidyverse,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(dplyr,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(caret,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(Metrics,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(xgboost,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(data.table,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE) # fread()
library(mltools,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE) # one_hot()

# ============= Data
train<-fread('C:/data/cradit/train.csv',data.table=T,stringsAsFactors = T) # na.strings='' : NA->''
train$credit<-as.integer(train$credit)
print(sum(is.na(train))) 
str(train)

test<-fread('C:/data/cradit/test.csv',data.table=T,stringsAsFactors = T)
print(sum(is.na(test)))
str(test)

sample_sub<-fread('C:/data/cradit/sample_submission.csv',data.table=F)
head(sample_sub)

# =============== Data Preprocessing
# factor형 one_hot coding
oh_train<-one_hot(train)
str(oh_train)
oh_test<-one_hot(test)
str(oh_test)

# ========== Xgboost 5-fold Cross Validation
########  cv 검증 ##################################################
set.seed(200)
train_data<-oh_train[,2:(ncol(oh_train)-1)]
x_train_cv<-data.matrix(train_data)
y_train_cv<-data.matrix(oh_train$credit)
xgb_cv<-xgb.cv(data=x_train_cv,label=y_train_cv,
               nfold=5,nrounds=1000,early_stopping_rounds = 10,
               objective='multi:softprob',metrics='mlogloss',num_class=3,prediction = T,print_every_n = 10,
               params=list(eta=0.05,max_depth=8,subsample=0.8,colsample_bytree=0.8,stratified=T)) 
#xgb_cv$evaluation_log
#xgb_cv

# ============== Xgboost Training
train_x<-data.matrix(train_data)
train_y<-data.matrix(oh_train$credit)
xgb_train<-xgb.DMatrix(data=train_x,label=train_y)
watchlist <- list(train=xgb_train)
xgb_fit <- xgb.train(data = xgb_train, 
                     eta=0.05, 
                     max_depth=8, subsample=0.8,colsample_bytree=0.8,
                     nrounds= 545,  # xgb_cv 결과, Best iteration
                     objective= "multi:softprob",  
                     eval_metric= "mlogloss", num_class=3,             
                     watchlist=watchlist,
                     print_every_n = 10
)

# ========== Test
xgb_test<-data.matrix(oh_test[,-1])
xgb_pred_test<-predict(xgb_fit,xgb_test)
xgb_pred_test_total<-matrix(xgb_pred_test,nrow=3) %>% t() %>% data.frame()
xgb_pred_test_total

# =========== Submission
sample_sub[,2:4]<-xgb_pred_test_total
print(sum(is.na(sample_sub)))
sample_sub

write.csv(sample_sub,file="C:/data/cradit/sub1.csv",row.names=F) 









