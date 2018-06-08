#library used for the project:
library(randomForest)
library(caret)
library(e1071)
library(rpart)
library(forecast)
library(dplyr)
library(plyr)
library(rpart)
library(pROC)
library(Hmisc)
library(ROCR)
library(rpart.plot)

#Reading the test and train data:
train.data <- read.csv("TrainData.csv")
nrow(train.data)
test.data <- read.csv("TestData.csv")


#Summary of testdata
summary(test.data)


#Visualization:
# 1) histogram of the independent values:

par(mfrow=c(1,1))
hist(train.data$Months.since.Last.Donation,main = "Months since last donation",
     xlab = "Months")
hist(train.data$Number.of.Donations,main = "Number of donations",
     xlab = "Months")
hist(train.data$Total.Volume.Donated..c.c..,main = "Total volume donated",
     xlab = "Volume")
hist(train.data$Months.since.First.Donation,main = "Months since first donation",
     xlab = "Months")

# 2) density plot:
plot(density(train.data$Months.since.Last.Donation),main = "Months since last donation")
plot(density(train.data$Number.of.Donations),main = "Number of donations")
plot(density(train.data$Total.Volume.Donated..c.c..),main = "Total volume donated")
plot(density(train.data$Months.since.First.Donation),main = "Months since first donation")


# Correlation between the independent varibales:
par(mfrow=c(1,1))
cor(test.data[,2:5])
  #We can see that "Number.of.Donations" and "Total.Volume.Donated..c.c.." are highly correlated.


pairs(train.data[,2:5])


#################################################################################################
#Regression model:

#logistics regression:

log.model1 <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                    Number.of.Donations + 
                    Months.since.First.Donation , data = train.data,family = "binomial")
log.model1
summary(log.model1)

# evaluate model fit in comparison with null model
modelChi <- log.model1$null.deviance - log.model1$deviance
chidf <- log.model1$df.null - log.model1$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.prob <- 1-pchisq(modelChi, chidf)
chisq.prob

AIC(log.model1)

# compute BIC
model1.BIC <- log.model1$deviance +
  2*length(log.model1$coefficients)*log(length(log.model1$fitted.values) )
model1.BIC  #607.4584


#Computing the accuracy of the model by creating confusion matrix:

train.predict1 <- predict(log.model1,train.data , type = "response")
p1 <- ifelse(train.predict1 > 0.5,1,0)
tab1 <- table(predicted = p1 , actual = train.data$Made.Donation.in.March.2007)
tab1
confusionMatrix(tab1)  #76.22%


#ROC curve:
plot(roc(train.data$Made.Donation.in.March.2007,train.predict1))
somers2(fitted(log.model1),train.data$Made.Donation.in.March.2007) #0.7547978 ROC value


#kappa value:
kappa(log.model1) #28.33947

#R square value:
rsq(log.model1)

###########logestic regression - taking log transformation:


log.model2 <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                    log(Number.of.Donations) + 
                    log(Months.since.First.Donation) , data = train.data,family = "binomial")
log.model2
summary(log.model2)

tree_final <- predict(log.model2, test.data,type = "response")
dataa1 <- cbind(test.data$X,tree_predict)
write.csv(dataa1,file="tree.csv")

# evaluate model fit in comparison with null model
modelChi2 <- log.model2$null.deviance - log.model2$deviance
chidf2 <- log.model2$df.null - log.model2$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.prob2 <- 1-pchisq(modelChi2, chidf2)
chisq.prob2

#compute AIC
AIC(log.model2)

# compute BIC
model2.BIC <- log.model2$deviance +
  2*length(log.model2$coefficients)*log(length(log.model2$fitted.values) )
model2.BIC  #600.627


#Computing the accuracy of the model by creating confusion matrix:

train.predict2 <- predict(log.model2,train.data , type = "response")
p2 <- ifelse(train.predict2 > 0.5,1,0)
tab2 <- table(predicted = p2 , actual = train.data$Made.Donation.in.March.2007)
tab2
confusionMatrix(tab2)  #77.08%


#ROC curve:
plot(roc(train.data$Made.Donation.in.March.2007,train.predict2))
somers2(fitted(log.model2),train.data$Made.Donation.in.March.2007) #0.7547978 ROC value


#kappa value:
kappa(log.model2) #28.33947


#R square value:
rsq(log.model2)

##########################
plot(log.model2)
par(mfrow=c(1,1))
plot(train.data$Months.since.First.Donation,ylab = "Months since first donation",main = "Normal plot")
plot(log(train.data$Months.since.First.Donation),ylab = "Months since first donation",
     main = "after log transformation")
plot(train.data$Number.of.Donations,ylab = "Number of donation",main = "Normal plot")
plot(log(train.data$Number.of.Donations),ylab = "Number of donation",
     main = "after log transformation")
#############################################################################################

# Decision tree:
library(tree)
tree_model1 <- rpart(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                        Number.of.Donations + 
                        Months.since.First.Donation , data = train.data)
summary(tree_model1)
rpart.plot(tree_model1,type =3,extra = 101)



#Computing the accuracy by creating the confusion matrix:
ctree_predict <- predict(tree_model1,train.data)
p3 <- ifelse(ctree_predict > 0.5,1,0)
tab3 <- table(predicted = p3 , actual = train.data$Made.Donation.in.March.2007)
tab3
confusionMatrix(tab3)
nrow(train.data)
?rpart.plot

#ROC curve:
attach(train.data)
plot(roc(train.data$Made.Donation.in.March.2007,ctree_predict))
somers2(fitted(ctree_model1),train.data$Made.Donation.in.March.2007) 

#predict the test data:
Made.Donation.in.March.2007 <- predict(ctree_model1, test.data)
final_data <- cbind(test.data$X,Made.Donation.in.March.2007)
write.csv(final_data,file="Blood.Donation.csv",row.names = FALSE)


