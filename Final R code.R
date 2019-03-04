# assignment 8 clustering
# the given dataset requires customer segmentation with 18 varaibles which explains the bheaviour of the customer, the dataset also have almost 90000 observations which explains the 9000 bheavioural pattern of 9000 credit card holders.

#reading the dataset in R
#install.packages("data.table")
install.packages("babar")
library(babar)
library(data.table)
library(caret)
library(ggplot2)
library(magrittr)
library(dplyr)
library(mlbench)
library(magrittr)
library(ggplot2)
library(tidyr)
library(PerformanceAnalytics)
library(MASS) # for transformations
library(moments)
library(corrplot)
library(tidyverse)
library(car)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dbscan)
library(ade4)
library(dummies)
install.packages('lars')
library(lars)
library(glmnet)
#install.packages('rcompanion')
library(rcompanion)
cc<- read.csv("/Users/chanukya/Desktop/IDA/Project/train.csv")
View(cc)
x <- as.factor(cc$Hazard)

levels(x)
test <- read.csv("/Users/chanukya/Desktop/IDA/Project/test.csv")
ID1 <- test$Id
test$Id <- NULL
str(test)
colnames(test)
ncol(test)
str(cc)
summary(cc)
dat <- cc
Hazard <- dat$Hazard
ncol(dat)



dat$Hazard <- NULL
dat$Id <- NULL
colnames(dat)
final <- rbind(dat,test)

#first lets see how the Varaibles are distributed.
dat %>%gather() %>%ggplot(aes(value)) +facet_wrap(~ key, scales = "free") + geom_density()



#converting all the factor variables to dummies
#library(dummies)
fact <- sapply((dat), is.factor)
fact <- dat[, fact]
cname1 <- colnames(fact)
colnames(dat)
(cname1)
for (f in cname1){
  df_all_dummy = acm.disjonctif(final[f])
  final[f] = NULL
  final = cbind(final, df_all_dummy)
}
colnames(final)
str(dat)
write.csv("/Users/chanukya/Desktop/IDA/Project/dummy.csv")
#converting all the variables as numeric:

dat <- as.data.frame(sapply( final, as.numeric ))
str(dat)



final_train <-cbind(final_train,Hazard)
summary(final_train)
colnames(final_train)
write.csv(final_train,"/Users/chanukya/Desktop/IDA/Project/dummy_train.csv")

final_test <- final[51000:101999,]
write.csv(final_test, "/Users/chanukya/Desktop/IDA/Project/dummy_test.csv")
dim(final)


#modelling.


#Ridge Regression
final_train1 <-as.data.frame( scale(final_train[,-1]))
model.ridge <- lm.ridge( Hazard~ ., data=final_train1)  #ridge regression is run for several values of lambda
qqnorm(model.ridge$coef)
str(model.ridge)

# The optimal lambda is given by

qplot(model.ridge$lambda,model.ridge$GCV )        # plots generalized cross validated error (GCV) vs the tuning parameter  
which.min(model.ridge$GCV)   # identifies the value for lambda with the smallest associated GCV
lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]   #save the value of optimal lambda for future use



#Lasso Regression:

model.lasso <- lars(as.matrix(final_train1),Hazard, type="lasso")
plot(model.lasso)
View(final_train1)
predict()

#bayesian regression

set.seed(1234)  ## reproducible sim
lmfit <- lm(Hazard~ ., data=final_train1)
bf<-Bayesfit(lmfit,10000)
t(apply(bf,2,Bayes.sum))



#lasso, Ridge,Elastic net
final_train1$Hazard <- NULL
fit
fit.lasso <- glmnet(as.matrix(final_train1,Hazard, family="gaussian", alpha=1)
                    fit.ridge <- glmnet(as.matrix(final_train1),Hazard, family="gaussian", alpha=0)
                    fit.elnet <- glmnet(as.matrix(final_train1),Hazard, family="gaussian", alpha=.5)
                    
                    
                    # 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
                    # (For plots on Right)
                    for (i in 0:10) {
                      assign(paste("fit", i, sep=""), cv.glmnet(as.matrix(final_train1),Hazard, type.measure="mse", 
                                                                alpha=i/10,family="gaussian"))
                    }
                    
                    
                    train1 <- final_train
                    nrow(final_train)
                    Hazard <- as.data.frame(Hazard)
                    train1 <- train1[0:39000,]
                    
                    test1 <- final_train[39001:50999,]
                    test1$Hazard <- NULL
                    fit0 <- glmnet(as.matrix(train1),Hazard[0:39000,], family="gaussian", alpha=0)
                    
                    yhat0 <- predict(fit0, s=fit0$lambda.1se, as.matrix(test1))
                    mse0 <- mean((Hazard[39001:50999,] - yhat0)^2)
                    RMSE <- sqrt(mse0)
                    RMSE
                    
                    
                    nrow(final_train1)*.75
                    
                    
                    cc$  <- cut(train1$Hazard, breaks=c(0,10,20,30,70), labels=c("1","2","3","4"))
                    
                    table(train1$bins)
                    
                    
                    # Plot solution paths:
                    par(mfrow=c(3,2))
                    # For plotting options, type '?plot.glmnet' in R console
                    plot(fit.lasso, xvar="lambda")
                    plot(fit10, main="LASSO")
                    
                    plot(fit.ridge, xvar="lambda")
                    plot(fit0, main="Ridge")
                    
                    plot(fit.elnet, xvar="lambda")
                    plot(fit5, main="Elastic Net")
                    
                    
                    class(final_test)
                    dim(final_test)
                    dim(final_train)
                    #imputation 
                    library(mice)
                    # Deterministic regression imputation via mice
                    
                    cc <- mice(out, method = "norm.predict", m = 1)
                    cc1 <- read.csv("/Users/chanukya/Desktop/IDA/out2.csv")
                    yhat0 <- predict(fit0, s=fit0$lambda.1se, as.matrix(final_test))
                    write.csv(yhat0,"/Users/chanukya/Desktop/IDA/Ridge.csv")
                    yhat1 <- predict(fit1, s=fit1$lambda.1se, as.matrix(final_test))
                    yhat2 <- predict(fit2, s=fit2$lambda.1se, as.matrix(final_test))
                    yhat3 <- predict(fit3, s=fit3$lambda.1se, as.matrix(final_test))
                    yhat4 <- predict(fit4, s=fit4$lambda.1se, as.matrix(final_test))
                    yhat5 <- predict(fit5, s=fit5$lambda.1se, as.matrix(final_test))
                    yhat6 <- predict(fit6, s=fit6$lambda.1se, as.matrix(final_test))
                    yhat7 <- predict(fit7, s=fit7$lambda.1se, as.matrix(final_test))
                    yhat8 <- predict(fit8, s=fit8$lambda.1se, as.matrix(final_test))
                    yhat9 <- predict(fit9, s=fit9$lambda.1se, as.matrix(final_test))
                    yhat10 <- predict(fit10, s=fit10$lambda.1se, as.matrix(final_test))
                    write.csv(yhat10,"/Users/chanukya/Desktop/IDA/lasso.csv")
                    
                    #Randome forest regression
                    set.seed(123)
                    valid_split <- initial_split(final_train, .8)
                    
                    # training data
                    ames_train_v2 <- analysis(final_train)
                    
                    # validation data
                    a
                    
                    rf_oob_comp <- randomForest(
                      formula = Hazard ~ .,
                      data    = final[0:50999],
                      xtest   = final_train,
                      ytest   = Hazard$Hazard[]
                    )
                    
                    # extract OOB & validation errors
                    oob <- sqrt(rf_oob_comp$mse)
                    validation <- sqrt(rf_oob_comp$test$mse)
                    
                    # compare error rates
                    tibble::tibble(
                      `Out of Bag Error` = oob,
                      `Test error` = validation,
                      ntrees = 1:rf_oob_comp$ntree
                    ) %>%
                      gather(Metric, RMSE, -ntrees) %>%
                      ggplot(aes(ntrees, RMSE, color = Metric)) +
                      geom_line() +
                      scale_y_continuous(labels = scales::dollar) +
                      xlab("Number of trees")
                    
                    #View(cc1)
                    Hazard$Hazard <- as.factor(Hazard$Hazard)
                    levels(Hazard$Hazard)
                    hazard1 <- Hazard$Hazard
                    hazard1<- as.numeric(hazard1)
                    
                    breaks <- c(10,20,40,50,60,70)
                    labels <- c("First", "Second", "Third", "Fourth", "Fifth", "Sixth")
                    bins <- cut(hazard1,breaks, include.lowest = T, right=FALSE, labels=labels)
                    hazard1 <- as.numeric(hazard1)
                    # classification 
                    length(hazard1)
                    for(i in 1:length(hazard1))
                    {
                      if(0<hazard1[i]<10){
                        hazard1[i] <- "First"
                      }
                      else if(10<hazard1[i]<20){
                        hazard1[i] <- "Second"
                      }
                      else if(30< hazard1[i] <40){
                        hazard1[i] <- "Third"
                      }
                      else if(40<hazard1[i]<50){
                        hazard1[i] <- "Fourth"
                      }
                      else (hazard1[i]>50){
                        hazard1[i] <- "Fifth"
                      }
                      
                      
                    }
                    Hazard$Hazard <- as.numeric(Hazard$Hazard)
                    Hazard$Hazard[ Hazard$Hazard < 10 & Hazard$Hazard >=0] <- "FIRST"
                    Hazard$Hazard[Hazard$Hazard < 20 & Hazard$Hazard >=10] <-"Second"
                    Hazard$Hazard[Hazard$Hazard < 30 & Hazard$Hazard >=20] <-"Third"
                    Hazard$Hazard[Hazard$Hazard < 40 & Hazard$Hazard >= 50] <-"Fourth"
                    Hazard$Hazard[ Hazard$Hazard >=50] <-"Fifth"
                    # w
                    
                    #classification
                    cc$Hazard <- cut(cc$Hazard, breaks=c(0,10,20,30,70), labels=c("1","2","3","4"))
                    final_train$Hazard <- cc$Hazard
                    
                    ncol(final_train)
                    write.csv(final_train,"/Users/chanukya/Desktop/IDA/Project/dummy_train.csv")
                    ncol(final_test)
                    
                    set.seed(101)
                    model.rf=randomForest(Hazard ~ . , data = cc , subset = final_train[0:39001,])
                    model.rf
                    plot(model.rf)
                    pred<-predict(model.rf,tr)
                    test.err= with(final_test, mean( (Hazard[39001:50999] - pred)^2))
                    confusionMatrix(pred,Hazard[39001:50999] )
                    pred1 <- predict(model.rf,final_test)
                    table(pred1)
                    
                    
                    ###################################XGboost################3
                    
                    library(readr)
                    library(reshape2)
                    library(xgboost)
                    library(DiagrammeR) #for xgb.plot.multi.trees 
                    library(corrplot)
                    library(caret)
                    
                    # Read data files:
                    train <- read.csv("train.csv")
                    test <- read.csv("test.csv")
                    
                    cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
                    cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))
                    
                    # extract id
                    id_test <- test$Id
                    test$Id <- NULL
                    train$Id <- NULL
                    n <- nrow(X)
                    
                    #No missing values
                    length(train[is.na(train)])
                    length(test[is.na(test)])
                    
                    # extarct target
                    target <- train$Hazard
                    train$Hazard <- NULL
                    boxplot(target) # Has a lot of outliers
                    
                    # summarize the Hazard or y variable, note that not every number is used (50 Levels only)
                    summary(target)
                    length(levels(as.factor(target)))
                    
                    total=rbind(train,test) #
                    
                    # Find near zero values
                    nzv <- nearZeroVar(total, saveMetrics= FALSE)
                    filteredtotal <- total[, -nzv]
                    dim(filteredtotal)  #Removal of 5 variables \\ Need to cross check with xgboost importance of var.
                    names(filteredtotal)
                    
                    # Checking the names of the predictors that are factors
                    factcol_total=names(total[,sapply(total, is.factor)] )
                    
                    # replace factors with level mean or(median) hazard
                    for (i in 1:ncol(total))
                    {
                      if (class(total[,i])=="factor") 
                      {
                        mm <- aggregate(target~train[,i], data=train, mean)
                        levels(total[,i]) <- mm[,2] 
                        total[,i] <- as.numeric(as.character(total[,i]))
                      }
                    }
                    
                    train_trans <- total[1:50999,]
                    test_trans <- total[51000:101999,]
                    train_trans2=cbind(train_trans,target)
                    write.csv(train_trans2,"train_numeric.csv",row.names=F, quote=FALSE)
                    train <- as.matrix(train_trans)
                    test <- as.matrix(test_trans)
                    
                    #Correlation plot
                    cormat=cor(train_trans)
                    corrplot(cormat,type="upper")
                    
                    logfile <- data.frame(shrinkage=c(0.04, 0.03, 0.03, 0.03, 0.02),
                                          rounds = c(140, 160, 170, 140, 180),
                                          depth = c(8, 7, 9, 10, 10),
                                          gamma = c(0, 0, 0, 0, 0),
                                          min.child = c(5, 5, 5, 5, 5),
                                          colsample.bytree = c(0.7, 0.6, 0.65, 0.6, 0.85),
                                          subsample = c(1, 0.9, 0.95, 1, 0.6))
                    
                    
                    # Prediction using 50 models through baging
                    models <- 5
                    repeats <- 10
                    targethat.test  <- rep(0,nrow(test))
                    for (j in 1:repeats) {
                      for (i in 1:models){
                        set.seed(i*2+j*2)
                        xgboost.mod <- xgboost(data = train, label = target, max.depth = logfile$depth[i], 
                                               eta = logfile$shrinkage[i], nround = logfile$rounds[i], 
                                               nthread = 4, objective = "reg:linear", subsample=logfile$subsample[i],
                                               colsample_bytree=logfile$colsample.bytree[i], gamma=logfile$gamma[i],
                                               min.child.weight=logfile$min.child[i])
                        targethat.test  <- targethat.test + predict(xgboost.mod, test)  
                      }
                    }
                    targethat.test <-  targethat.test/(models*repeats)
                    
                    #xgboost.mod2=xgboost(data=train,label=target,nrounds=10)
                    write.csv(data.frame(Id=id_test, Hazard=targethat.test),"xgboost_output_1bag.csv",row.names=F, quote=FALSE)
                    
                    imp=xgb.importance(model=xgboost.mod)
                    order(imp)
                    rank(imp)
                    xgb.plot.importance(imp) #shows the importance of variables w.r.t the following:
                    "The meaning of the importance data table is as follows:
                    
                    The Gain implies the relative contribution of the corresponding feature to the model calculated by taking each feature's contribution for each tree in the model. A higher value of this metric when compared to another feature implies it is more important for generating a prediction.
                    The Cover metric means the relative number of observations related to this feature. For example, if you have 100 observations, 4 features and 3 trees, and suppose feature1 is used to decide the leaf node for 10, 5, and 2 observations in tree1, tree2 and tree3 respectively; then the metric will count cover for this feature as 10+5+2 = 17 observations. This will be calculated for all the 4 features and the cover will be 17 expressed as a percentage for all features' cover metrics.
                    The Frequence (frequency) is the percentage representing the relative number of times a particular feature occurs in the trees of the model. In the above example, if feature1 occurred in 2 splits, 1 split and 3 splits in each of tree1, tree2 and tree3; then the weightage for feature1 will be 2+1+3 = 6. The frequency for feature1 is calculated as its percentage weight over weights of all features.
                    
                    The Gain is the most relevant attribute to interpret the relative importance of each feature."
                    xgb.plot.deepness(model = xgboost.mod)
                    "There is more than one way to understand the structure of the trees, besides plotting them all. Since there are all binary trees, we can have a clear figure in mind if we get to know the depth of each leaf. The function xgb.plot.deepness is inspired by this blogpost: http://aysent.github.io/2015/11/08/random-forest-leaf-visualization.html.
                    
                    From the function xgb.plot.deepness, we can get two plots summarizing the distribution of leaves according to the change of depth in the tree."
                    "The upper plot shows the number of leaves per level of deepness. The lower plot shows noramlized weighted cover per leaf (weighted sum of instances). From this information, we can see that for the 5-th and 6-th level, there are actually not many leaves. To avoid overfitting, we can restrict the depth of trees to be a small number."
                    
                    