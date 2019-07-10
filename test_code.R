## loss function
lossDMC <- function(true, pred){
  loss <- sum( (true==1) & (pred==0) ) * 5 +
    sum( (true==0) & (pred==1) ) * 25 + 
    sum( (true==1) & (pred==1) ) * (-5)
  return(loss)
}


## training set
Train0 <- read.csv(file = "train.csv",sep = "|")

Train0$trustLevel <- as.factor(Train0$trustLevel)
TotalItem <- Train0$totalScanTimeInSeconds * Train0$scannedLineItemsPerSecond
Train <- data.frame(Train0, TotalItem = TotalItem)
Train <- Train[,-c(3,6,7,9)]


## testing set
Test0 <- read.csv(file = "test.csv",sep = "|")

Test0$trustLevel <- as.factor(Test0$trustLevel)
TotalItem <- Test0$totalScanTimeInSeconds * Test0$scannedLineItemsPerSecond
Test <- data.frame(Test0, TotalItem = TotalItem)
Test <- Test[,-c(3,6,7,9)]

## true value
real_pred <- read.csv(file = "DMC-2019-realclass.csv")



formula1 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds "

formula2 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds + I( totalScanTimeInSeconds * valuePerSecond ) +
I( totalScanTimeInSeconds * valuePerSecond^2 )"

formula3 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds + I( totalScanTimeInSeconds * valuePerSecond ) +
I( totalScanTimeInSeconds * valuePerSecond^(3.5) )"

fit1 <- glm(formula=formula1,data=Train,family=binomial(link=logit))
fit2 <- glm(formula=formula2,data=Train,family=binomial(link=logit))
fit3 <- glm(formula=formula3,data=Train,family=binomial(link=logit))
py1 <- predict(fit1,newdata=Test, type = "response")
py2 <- predict(fit2,newdata=Test, type = "response")
py3 <- predict(fit3,newdata=Test, type = "response")

## model 1:2
pred1_2 <- ifelse((0.4*py1+0.6*py2) > 5/7, 1, 0)
lossDMC(real_pred$fraud, pred1_2)
##### -51800


## model 1:3
pred1_3 <- ifelse((0.4*py1+0.6*py3) > 5/7, 1, 0)
lossDMC(real_pred$fraud, pred1_3)
##### -51945

## model 1:2:3
pred1_2_3 <- ifelse((0.3*py1+0.1*py2+0.6*py3) > 5/7, 1, 0)
lossDMC(real_pred$fraud, pred1_2_3)
##### -50375

## majority vote (our solution)
pred_mv <- pred1_2_3
pred_mv[pred1_2_3!=pred1_3] <- pred1_2[pred1_2_3!=pred1_3]
lossDMC(real_pred$fraud, pred_mv)
##### -51355




