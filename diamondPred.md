## Two-months-Salary
##This project has the objective of building a model that predicts the price od a diamong ring based on the two month's Salary
data set by Pope, B. (2006)

## two_months_salary.r
## Sample R program to begin work on the Two Month's Salary case study
## to run the entire source code program type:  source("two_months_salary.r")  
## read in data from comma-delimited text file
getwd()
setwd("C:/Users/Uduak/Dropbox/MSPA/454/Assignments/Week4/Two Months")
diamonds <- read.csv("two_months_salary.csv")
attach(diamonds)

cat("\n","----- Initial Structure of diamonds data frame -----","\n")
## examine the initial data frame
##=======================================
dim(diamonds)
str(diamonds)
anyNA(diamonds)
summary(diamonds)
table(color)
table(clarity)

##data quality check using a Custom Summary Function for numeric variables

num.diamonds <- diamonds[,-c(2:6)]

my.summary <- function(in.df){
  
  VAR.NAME <- colnames(in.df);
  
## Here we will embed the functions not.na() and count.na()
## directly into the function my.summary().
  not.na <- function(x){
    out <- sum(-1*(is.na(x)-1));
    return(out);
  }
  NOT.NA <- apply(X=in.df,MARGIN=2,FUN='not.na');
  
  count.na <- function(x){
    out <- sum(is.na(x));
    return(out);
  }
  
  IS.NA <- apply(X=in.df,MARGIN=2,FUN='count.na');
  PCT.NA <- IS.NA/(NOT.NA+IS.NA);
  ## Note that we need to account for computations with NA values;
  MIN <- apply(X=in.df,MARGIN=2,FUN='min',na.rm=TRUE);
  p <- c(0.01,0.05,0.25,0.50,0.75,0.95,0.99);
  Q <- apply(X=in.df,MARGIN=2,FUN='quantile',p,na.rm=TRUE);
  tQ.df <- as.data.frame(t(Q));
  colnames(tQ.df) <- c('Q01','Q05','Q25','Q50','Q75','Q95','Q99');
  MAX <- apply(X=in.df,MARGIN=2,FUN='max',na.rm=TRUE);
  MEAN <- apply(X=num.diamonds,MARGIN=2,FUN='mean',na.rm=TRUE)
  SD <- apply(X=num.diamonds,MARGIN=2,FUN='sd',na.rm=TRUE)
  
  wide.df <- cbind(VAR.NAME,NOT.NA,IS.NA,PCT.NA,MIN,tQ.df,MAX,MEAN,SD);
  rownames(wide.df) <- seq(1,dim(wide.df)[1],1);
  
  return(wide.df);
}

my.summary(in.df = num.diamonds)
diamondSummary <- write.csv(my.summary(in.df = num.diamonds),"diamondSummary.csv")

## Check distribution of target variable: Price
require(lattice)
histogram(~price, diamonds)
densityplot(~price, diamonds)
qqmath(~ price, data = diamonds)
## Transform the response variable using log function 
##to see if it becomes normlaized
diamonds$logprice <- log(diamonds$price)
## Check distribution of target variable:logprice
histogram(~logprice, diamonds)
densityplot(~logprice, diamonds)
bwplot(~logprice, diamonds)
qqmath(~ logprice, data = diamonds)

## Check distribution of numerical predictors
histogram(~carat, diamonds)
histogram(~log(carat), diamonds)
densityplot(~carat, diamonds)
densityplot(~log(carat), diamonds)
bwplot(~carat, diamonds)
bwplot(~log(carat), diamonds)
qqmath(~ carat, data = diamonds)

## Check on categorical predictors
table(color)
table(clarity)
table(cut)
table(store)

## ## ## we can create a new channel factor called internet as a binary indicator   
## ## ## the ifelse() function is a good way to do this type of variable definition
## ## diamonds$internet <- ifelse((diamonds$channel == "Internet"),2,1)
## ## diamonds$internet <- factor(diamonds$internet,levels=c(1,2),labels=c("No","Yes"))
## 
## cat("\n","----- Checking the Definition of the internet factor -----","\n")
## ## check the definition of the internet factor
## table(diamonds$channel,diamonds$internet)
 

cat("\n","----- Revised Structure of diamonds data frame -----","\n")
## we check the structure of the diamonds data frame again
str(diamonds)

##Exploratory Data Analysis (EDA)

## Create data matrix to dichotomize all factor variables
diamond.matrix <- model.matrix (~.-1,data=diamonds)
diamond.frame <- data.frame(diamond.matrix)
names(diamond.frame)
str(diamond.frame)
attach(diamond.frame)
##Exploratory decision Tree (Regression)
require(tree)
?tree
summary(price)
str(price)
tree.diamonds <- tree(price~.-logprice,data = diamond.frame)
summary(tree.diamonds)
plot(tree.diamonds)
text(tree.diamonds,pretty = 0)
tree.diamonds

## Examining boxplots of decision tree variables
color.f <- factor(diamonds$color)
clarity.f <- factor(diamonds$clarity)
storeBlue.Nilef <- factor(storeBlue.Nile)
bwplot(price~color.f)
bwplot(price~clarity.f)
bwplot(price~storeBlue.Nilef)

##Bin price for EDA purposes

bins <- 4
cutpoints <- quantile(price,(0:bins)/bins)
priceBin <- cut(price,cutpoints,include.lowest = TRUE)
table(priceBin)


table(priceBin, color.f)
table(priceBin, clarity.f)
table(priceBin, storeBlue.Nilef)

##Compare priceBin with Color.F, Clarity.F, Store - chi-square test

chisq.test(priceBin,color.f)
chisq.test(priceBin,clarity.f)
chisq.test(priceBin,storeBlue.Nilef)
                                                                      
## Other Plots:

xyplot(price~carat, diamonds)
abline
bwplot(logprice~color.f, diamonds,xlab = "color.f")
bwplot(logprice~clarity.f, diamonds,xlab = "clarity.f")
bwplot(logprice~cut, diamonds,xlab = "cut")
bwplot(logprice~channel, diamonds,xlab = "channel")
bwplot(logprice~store, diamonds,xlab = "store")


## let's prepare a graphical summary of the diamonds data
## we note that price and carat are numeric variables with a strong relationship
## also cut and channel are factor variables related to price
## showing the relationship between price and carat, while conditioning
## on cut and channel provides a convenient view of the diamonds data
## in addition, we jitter to show all points in the data frame

xyplot(jitter(price) ~ jitter(carat) | color.f, 
       data = diamond.frame,
        aspect = 1, 
        strip=function(...) strip.default(..., style=1),
        col.line = "darkgrey", col.symbol = "black",
        xlab = "Size or Weight of Diamond (carats)", 
        ylab = "Price")

xyplot(jitter(price) ~ jitter(carat) | clarity.f, 
       data = diamond.frame,
       aspect = 1, 
       strip=function(...) strip.default(..., style=1),
       col.line = "darkgrey", col.symbol = "black",
       xlab = "Size or Weight of Diamond (carats)", 
       ylab = "Price")
 

xyplot(jitter(price) ~ jitter(carat) | storeBlue.Nilef, 
       data = diamond.frame,
       aspect = 1, 
       strip=function(...) strip.default(..., style=1),
       col.line = "darkgrey", col.symbol = "black",
       xlab = "Size or Weight of Diamond (carats)", 
       ylab = "Price")
?xyplot
 ##Correlations
library(corrplot)
M <- cor(diamond.frame)
corrplot(M,method = "circle")


##backward variable selection with price
require(leaps)
regfit.bwd <- regsubsets(price~.-logprice,data=diamond.frame,nvmax=18,method="backward")
  bwd.summary <- summary(regfit.bwd)
  names(bwd.summary)
  par(mfrow =c(2,2))
  plot(bwd.summary$rss,xlab="Number of Variables",ylab=" RSS", type="l")
  plot(bwd.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")
  which.max(bwd.summary$adjr2)
  points(12,bwd.summary$adjr2[12],col ="red",cex =2, pch =20)
  plot(bwd.summary$cp ,xlab ="Number of Variables ",ylab="Cp",type="l")
  which.min (bwd.summary$cp )
  points (11,bwd.summary$cp[11], col ="red",cex =2, pch =20)
  which.min(bwd.summary$bic )
  plot(bwd.summary$bic,xlab="Number of Variables",ylab=" BIC",type="l")
  points (8,bwd.summary$bic[8], col ="red",cex =2, pch =20)
  
  par(mfrow =c(1,1))

##Displaying selected variables for the best based on optimal values of Cp, bic and AdjRsq
  
  plot(regfit.bwd,scale="Cp")
  plot(regfit.bwd,scale="bic")
  plot(regfit.bwd,scale="adjr2")
  coef(regfit.bwd,12)

## Naive Regression Model
par(mfrow=c(2,2))
fit <- lm(price~carat+color+clarity+cutIdeal+channelMall
          +storeAusmans+storeChalmers+storeDanford+storeGoodmans
          +storeKay+storeRiddles+cutNot.Ideal,data = diamond.frame)
summary(fit)
plot(fit)

fit1 <- lm(logprice~carat+color+clarity+cutIdeal+channelMall
           +storeAusmans+storeChalmers+storeDanford+storeGoodmans
           +storeKay+storeRiddles = diamond.frame)
summary(fit1)
plot(fit1)

##log tranform of carat
diamond.frame$logcarat <- log(diamond.frame$carat)

fit2 <- lm(logprice~logcarat+color+clarity+cutIdeal+channelMall
           +storeAusmans+storeChalmers+storeDanford+storeGoodmans
           +storeKay+storeRiddles,data = diamond.frame)
summary(fit2)
plot(fit2)

par(mfrow=c(1,1))

## ##Make predictions and calculate validation errors. Regsubsets has no predict method so we have to do some work to get what we need
## val.errors <- rep(NA,18)
## ##options(error=NULL)
## ##options(error=recover)
## x.test <- model.matrix(price~.-logprice,data = diamond.frame[-train,])
## x.test
## for (i in 1:18) {
##   coefi <- coef(regfit.bwd,id=i)
##   pred=x.test[,names(coefi)]%*%coefi
##   val.errors[i]=mean((diamond.frame$price[-train]-pred)^2)
## }
## plot(sqrt(val.errors),ylab="Root MSE",pch=19,type="b")
## points(sqrt(regfit.bwd$rss[-1]/128),col="blue",pch=19,type="b")
## legend("topright",legend = c("Training","Validation"),col = c("blue","black"),pch = 19)
## 
## ##Creating predict method for Regsubsets function
## predict.regsubsets <- function(object,newdata,id,...){
##   form=as.formula(object$call[[2]])
##   mat=model.matrix(form,newdata)
##   coefi=coef(object,id=id)
##   mat[,names(coefi)]%*%coefi
## }

##Other exploratory models
## Forward stepwise variable selection
library(ISLR)
library(leaps)
regfit.fwd <- regsubsets(logprice~.-price-carat,data=diamond.frame,nvmax=18,method="forward")
fwd.summary <- summary(regfit.fwd)
par(mfrow =c(2,2))
plot(fwd.summary$rss,xlab="Number of Variables",ylab=" RSS", type="l")
plot(fwd.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")
which.max(fwd.summary$adjr2)
points(12,fwd.summary$adjr2[12],col ="red",cex =2, pch =20)
plot(fwd.summary$cp ,xlab ="Number of Variables ",ylab="Cp",type="l")
which.min (fwd.summary$cp )
points (12,fwd.summary$cp[12], col ="red",cex =2, pch =20)
which.min(fwd.summary$bic )
plot(fwd.summary$bic,xlab="Number of Variables",ylab=" BIC",type="l")
points (8,fwd.summary$bic[8], col ="red",cex =2, pch =20)

par(mfrow =c(1,1))

##Displaying selected variables for the best based on optimal values of Cp, bic and AdjRsq

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")
plot(regfit.fwd,scale="bic")
coef(regfit.fwd,12)
coef(regfit.fwd,8)

##Regression Model using variables obtained from Fwd variable selection
fit.fwd1 <- lm(logprice~logcarat+color+clarity+cutIdeal+storeAusmans
               +storeBlue.Nile+storeFred.Meyer+storeGoodmans+storeKay
               +storeR..Holland+storeZales,
               data = diamond.frame)
summary(fit.fwd1)
par(mfrow=c(2,2))
plot(fit.fwd1)


par(mfrow=c(1,1))

##Stepwise Variable Selection using AIC:
library(MASS)
fit <- lm(logprice~.-price-carat,data=diamond.frame)
step <- stepAIC(fit, direction="both")
step$anova ## display results
step.summary <- summary(step)
step.summary$adj.r.squared

##Fitting linear regression models using LASSO variables
fit.step <- lm(logprice~color+clarity+cutIdeal+channelInternet
                +storeAusmans+storeDanford+storeFred.Meyer
                +storeGoodmans+storeKay+storeRiddles
                +logcarat,data = diamond.frame)
summary(fit.step)
par(mfrow=c(2,2))
plot(fit.step)

par(mfrow=c(1,1))
##All subsets - Best Subsets
regfit.full <- regsubsets(logprice~.-price-carat,data = diamond.frame)
summary(regfit.full)
regfit.full <- regsubsets(logprice~.-price-carat,data = diamond.frame,nvmax = 18)
reg.summary <- summary(regfit.full)
par(mfrow=c(1,1))
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(12,reg.summary$cp[12],pch=20,col="red")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
which.max(reg.summary$adjr2)
points(12,reg.summary$adjr2[12],pch=20,col="red")
plot(reg.summary$bic,xlab="Number of Variables",ylab="bic")
which.min(reg.summary$bic)
points(8,reg.summary$bic[8],pch=20,col="red")
##Plots using regsubsets
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="bic")
coef(regfit.full,12)
coef(regfit.full,8)

##Fitting linear regression models to all subsets
fit.all <- lm(logprice~logcarat+color+clarity+cutIdeal+channelMall
              +storeAusmans+storeBlue.Nile+storeFred.Meyer
              +storeGoodmans+storeKay+storeR..Holland
              +storeZales,data = diamond.frame)
summary(fit.all)
par(mfrow=c(2,2))
plot(fit.all)


par(mfrow=c(1,1))

## LASSO Variable Selection
library(glmnet)
x <- model.matrix (logprice~.-price-carat,data=diamond.frame )
write.csv(x,"x.csv")
y <- diamond.frame$logprice
##Lasso Model
fit.lasso <- glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso <- cv.glmnet(x,y)
##plot(fit.lasso,xvar="dev",label=TRUE)
plot(cv.lasso)
coef(cv.lasso)

##Fitting linear regression models using LASSO variables
fit.lasso <- lm(logprice~color+clarity+cutIdeal+channelInternet
                +channelMall+storeFred.Meyer+storeGoodmans+storeKay
                +storeRiddles+logcarat,data = diamond.frame)
summary(fit.lasso)
par(mfrow=c(2,2))
plot(fit.lasso)


##Modeling Suite
##=======================
##Linear regression Model no interractions
fit.Linear <- lm(logprice~logcarat+color+clarity+cutIdeal+channelMall
                 +storeFred.Meyer+storeGoodmans+storeKay,data = diamond.frame)
summary(fit.Linear)
par(mfrow=c(2,2))
plot(fit.Linear)
library(car)
vif(fit.Linear)
sqrt(vif(fit.Linear))>2

##create training and test set (~70/30 split)
set.seed(100)
train <- sample(1:nrow(diamond.frame),297)
trainset <- diamond.frame[train,]
testset <- diamond.frame[-train,]

##Prediction accuracy of linear model using training/test split:
linear.model <- lm(logprice~logcarat+color+clarity+cutIdeal+channelMall
                 +storeFred.Meyer+storeGoodmans
                 +storeKay,data = trainset)
linear.model
##Plots to check prediction quality
trainset$pred.train.linear <- predict(linear.model,trainset)
ggplot(data=trainset,aes(x=pred.train.linear,
                         y=pred.train.linear-logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.train.linear,y=pred.train.linear-logprice),color="black")

testset$pred.test.linear <- predict(linear.model,testset)
ggplot(data=testset,aes(x=pred.test.linear,
                         y=pred.test.linear-logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.test.linear,y=pred.test.linear-logprice),color="black")

##computing rsq of linear model
rsq <- function(y,f){1-sum((y-f)^2)/sum((y-mean(y))^2)}
rsq(trainset$logprice,predict(linear.model,newdata = trainset))
rsq(testset$logprice,predict(linear.model,newdata = testset))

##computing rmse of linear model
rmse <- function(y,f){sqrt(mean(y-f)^2)}
rmse(trainset$logprice,predict(linear.model,newdata = trainset))
rmse(testset$logprice,predict(linear.model,newdata = testset))


##Fitting non-linear terms and Interactions
fit.inter <- lm(logprice~logcarat*color+logcarat*clarity+cutIdeal+channelMall
                 +storeFred.Meyer+storeGoodmans+storeKay,data = diamond.frame)
summary(fit.inter)
plot(fit.inter)
vif(fit.inter)
sqrt(vif(fit.inter))>2

##Prediction accuracy of linear model with interraction using training/test split:
fit.inter <- lm(logprice~logcarat*color+logcarat*clarity+cutIdeal+channelMall
                +storeFred.Meyer+storeGoodmans+storeKay,data = trainset)
fit.inter
##Plots to check prediction quality
trainset$pred.inter.train <- predict(fit.inter,trainset)
ggplot(data=trainset,aes(x=pred.inter.train,
                         y=pred.inter.train-logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.inter.train,y=pred.inter.train-logprice),color="black")

testset$pred.inter.test <- predict(fit.inter,testset)
ggplot(data=testset,aes(x=pred.inter.test,
                        y=pred.inter.test-logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.inter.test,y=pred.inter.test-logprice),color="black")

##computing rsq of interraction model
rsq <- function(y,f){1-sum((y-f)^2)/sum((y-mean(y))^2)}
rsq(trainset$logprice,predict(fit.inter,newdata = trainset))
rsq(testset$logprice,predict(fit.inter,newdata = testset))

##computing rmse of interraction model
rmse <- function(y,f){sqrt(mean(y-f)^2)}
rmse(trainset$logprice,predict(fit.inter,newdata = trainset))
rmse(testset$logprice,predict(fit.inter,newdata = testset))


##Tree Model
require(tree)
tree.diamonds <- tree(logprice~logcarat+color+clarity+cutIdeal
                      +channelMall+storeFred.Meyer+storeGoodmans
                      +storeKay,data = diamond.frame)
summary(tree.diamonds)
par(mfrow=c(1,1))
plot(tree.diamonds)
text(tree.diamonds,pretty = 0)
tree.diamonds

##Prediction accuracy of tree model using training/test split:
tree.diamonds <- tree(logprice~logcarat+color+clarity+cutIdeal
                      +channelMall+storeFred.Meyer+storeGoodmans
                      +storeKay,data = trainset)
tree.diamonds

##Plots to check prediction quality
trainset$pred.tree.train <- predict(tree.diamonds,trainset)
ggplot(data=trainset,aes(x=pred.tree.train,y=logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.tree.train,y=logprice),color="black")+
  geom_line(aes(x=logprice,y=logprice),color="blue",linetype=2)

testset$pred.tree.test <- predict(tree.diamonds,testset)
ggplot(data=testset,aes(x=pred.tree.test,y=logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.tree.test,y=logprice),color="black")+
  geom_line(aes(x=logprice,y=logprice),color="blue",linetype=2)

##computing rsq for tree model
rsq <- function(y,f){1-sum((y-f)^2)/sum((y-mean(y))^2)}
rsq(trainset$logprice,predict(tree.diamonds,newdata = trainset))
rsq(testset$logprice,predict(tree.diamonds,newdata = testset))

##computing rmse for tree model
rmse <- function(y,f){sqrt(mean(y-f)^2)}
rmse(trainset$logprice,predict(tree.diamonds,newdata = trainset))
rmse(testset$logprice,predict(tree.diamonds,newdata = testset))

##Random Forest
require(randomForest)
set.seed(101)
rf.diamond=randomForest(logprice~logcarat+color+clarity+cutIdeal
                       +channelMall+storeFred.Meyer+storeGoodmans
                       +storeKay,data = diamond.frame,importance=T)
rf.diamond
rf.diamond$importance
varImpPlot(rf.diamond)
par(mfrow=c(1,1))

##Prediction accuracy of random forest model using training/test split:
rf.diamond=randomForest(logprice~logcarat+color+clarity+cutIdeal
                        +channelMall+storeFred.Meyer+storeGoodmans
                        +storeKay,data = trainset)

rf.diamond
##Plots to check prediction quality
trainset$pred.rf.train <- predict(rf.diamond,trainset)
ggplot(data=trainset,aes(x=pred.rf.train,y=logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.rf.train,y=logprice),color="black")+
  geom_line(aes(x=logprice,y=logprice),color="blue",linetype=2)

testset$pred.rf.test <- predict(rf.diamond,testset)
ggplot(data=testset,aes(x=pred.rf.test,y=logprice))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred.rf.test,y=logprice),color="black")+
  geom_line(aes(x=logprice,y=logprice),color="blue",linetype=2)

##computing rsq for the  Random Forest model
rsq <- function(y,f){1-sum((y-f)^2)/sum((y-mean(y))^2)}
rsq(trainset$logprice,predict(rf.diamond,newdata = trainset))
rsq(testset$logprice,predict(rf.diamond,newdata = testset))

##computing rmse for the Random Forest  model
rmse <- function(y,f){sqrt(mean(y-f)^2)}
rmse(trainset$logprice,predict(rf.diamond,newdata = trainset))
rmse(testset$logprice,predict(rf.diamond,newdata = testset))

