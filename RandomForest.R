#------------------------CART:Applying Decision Tree Regression--------------------------------------

#Divide dataset into train and test
ames<-read.csv("G:/DATA SCIENCE/Group project/R/Workspace/one-hotencoding/ames.csv")
str(ames)
ames<-ames[,-1]
str(ames)
library(splitstackshape)
library(dplyr)
set.seed(93)

stratified_sample <- ames %>%
  sample_frac(0.7)
#ungroup


View(stratified_sample)
class(stratified_sample)
str(stratified_sample)


train<-ames[stratified_sample$X,]
test<-ames[-stratified_sample$X,]
View(train)
View(test)
ncol(test)

test1<-test[,-20]
View(test1)
#_________________________________________________________________________________________
#----------------------------Random Forest-----------------------------
library(randomForest)

regressor = randomForest(formula =SalePrices ~ .-X,data = (train), ntree = 200,mtry=4)

#, ntree = 50, mtry=3
regressor
summary(regressor)

pred <- predict(regressor,newdata=test1)

# Plotting the tree
plot(regressor)
#text(regressor)
regressor$predicted

str(pred)
View(pred)

View(ames$SalePrice)

results <- cbind(pred,test$SalePrices) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
View(results)
write.csv(results,"G:/DATA SCIENCE/Group project/R/Workspace/RF_predict_HP_value.csv")

#--------------------------Calculate Accuracy-------------------------------------------------

#There's lots of ways to evaluate the prediction values, for example the MSE (mean squared error):
mse <- mean((results$real-results$pred)^2)
print(mse)

#Or the root mean squared error:
RMSE<-sqrt(mse)
RMSE
#Or just the R-Squared Value for our model (just for the predictions)

SSE = sum((results$real-results$pred)^2)
SST = sum( (results$real - mean(ames$SalePrice))^2)
SSR = sum((results$pred-mean(ames$SalePrice))^2)

R2= SSR/SST 
print(R2)

#Adjusted R2 value
oneminus.R2=1-R2

Adj.R2= 1-((oneminus.R2*(438-1))/(438-200-1))
print(Adj.R2)
#----------------------------Error rates-----------------------------------------------

finalvalid.err_rate <- ((abs((results$pred) - (results$real))))
#print(finalvalid.err_rate)

finalvalid.pcterr <- abs((results$real - results$pred )/results$real)*100
#print(finalvalid.pcterr)

finalvalid<-cbind(finalvalid.err_rate,finalvalid.pcterr) 
colnames(finalvalid)<- c('err_rate','pcterr')
finalvalid<-as.data.frame(finalvalid)
class(finalvalid)
View(finalvalid)

#median
median(finalvalid$err_rate) # 22795.4 $
median(finalvalid$pcterr)  #12.85075 %

#mean---just for checking
mean(finalvalid$err_rate) # 78916.37 $
mean(finalvalid$pcterr)  #47.8787 %

paste(" median prediction error in $ ",median(finalvalid.err_rate))

paste(" median prediction error as % difference from correct value ",median(finalvalid.pcterr))

#--------------------------------------------------------------------------------------------


