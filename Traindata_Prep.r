#Prepration of Train data...

#setwd("C:/R/workspace")
#------------------------------------------------------------------
##             1.Read Train and Test data and check Summary
#------------------------------------------------------------------
#read Train Data
setwd("G:/DATA SCIENCE/Group project/R/Workspace")
AMES_Train=read.csv("Train_data.csv")
View(AMES_Train)
str(AMES_Train) 
#Summary of Train data set
summary(AMES_Train)  
summary(AMES_Train[,sapply(AMES_Train[,1:81], class) == "integer"])


#Read Test Data
AMES_Test=read.csv("Test_Data.csv")
View(AMES_Test)
str(AMES_Test)
summary(AMES_Test)  

#------------------------------------------------------------------
#Count the number of columns that consists of text data and numerical data
data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
}
data_types(AMES_Train)

table(unlist(lapply(AMES_Train, class)))
#------------------------------------------------------------------
#Count the number of columns that consists of text data

sum(sapply(AMES_Train[,1:81], class) == "factor")

#Count the number of columns that consists of numerical data

sum(sapply(AMES_Train[,1:81], class) == "integer")
#------------------------------------------------------------------

#Check The Dimension Of data

cat('Train has', dim(AMES_Train)[1], 'rows and', dim(AMES_Train)[2], 'columns.')
## Train has 1460 rows and 81 columns.
cat('Test has', dim(AMES_Test)[1], 'rows and', dim(AMES_Test)[2], ' columns.')
## Test has 1459 rows and 80  columns.
#------------------------------------------------------------------


# Looking at the distribution and summary of the target variable
summary(AMES_Train$SalePrice)

quantile(AMES_Train$SalePrice)
# Conclusion: From summary, it was observed that minimum price is greater than 0

## Histogram for target variable
library(ggplot2)
ggplot(AMES_Train, aes(x=AMES_Train$SalePrice))+geom_histogram()
#hist(AMES_Train$SalePrice)
## Conclusion: From Histogram, we could see that it deviates from normal distribution and has positive skewness.

#------------------------------------------------------------------
##               2.Combine Train And test data
#------------------------------------------------------------------
library('dplyr') 
AMES_Test$SalePrice<-rep(NA,1459)

AMES_Train$isTrain <- 1
AMES_Test$isTrain <- 0
 house<-bind_rows(AMES_Train,AMES_Test)


 str(house)
 head(house)
 tail(house)
 
 ##check if there is any NA's are there in data
 colSums(is.na(house[,1:80]))  
 house<-as.data.frame(house)
 missingValue<- (colSums(is.na(house[,1:80]))/nrow(house))*100
 print(missingValue)
 
 ##Eliminate columns having more than 40% NA value
 house<-house[, -which(colMeans(is.na(house[,1:80])) > 0.4)]
 
 str(house)
 View(house)

 #------------------------------------------------------------------ 
 #--------3.Treatment of missing value by Imputing Variable--------
 #------------------------------------------------------------------ 
 ##used for character variables to find mode
 Mode=function(x){
   ta=table(x)
   tam=max(ta)
   if(all(ta==tam))
     mod = NA
   else if(is.numeric(x))
     mod=as.numeric(names(ta))[ta==tam]
   else
     mod=names(ta)[ta==tam]
   return(mod)
 }
 
 ## impute categorical varible by Mode
 house$BsmtQual[is.na(house$BsmtQual)]=Mode(house$BsmtQual)
 house$BsmtCond[is.na(house$BsmtCond)]=Mode(house$BsmtCond)
 house$BsmtExposure[is.na(house$BsmtExposure)]=Mode(house$BsmtExposure)
 house$BsmtFinType1[is.na(house$BsmtFinType1)]=Mode(house$BsmtFinType1)
 house$BsmtFinType2[is.na(house$BsmtFinType2)]=Mode(house$BsmtFinType2)
 house$Electrical[is.na(house$Electrical)]=Mode(house$Electrical)
 house$GarageType[is.na(house$GarageType)]=Mode(house$GarageType)
 house$GarageYrBlt[is.na(house$GarageYrBlt)]=Mode(house$GarageYrBlt)
 house$GarageQual[is.na(house$GarageQual)]=Mode(house$GarageQual)
 house$GarageCond[is.na(house$GarageCond)]=Mode(house$GarageCond)
 house$MasVnrType[is.na(house$MasVnrType)]=Mode(house$MasVnrType)
 house$GarageFinish[is.na(house$GarageFinish)]=Mode(house$GarageFinish)
 house$GarageType[is.na(house$GarageType)]=Mode(house$GarageType)
 house$GarageYrBlt[is.na(house$GarageYrBlt)]=Mode(house$GarageYrBlt)
 house$GarageQual[is.na(house$GarageQual)]=Mode(house$GarageQual)
 house$MSZoning[is.na(house$MSZoning)]=Mode(house$MSZoning)
 house$Utilities[is.na(house$Utilities)]="AllPub"
 house$Exterior1st[is.na(house$Exterior1st)]=Mode(house$Exterior1st)
 house$Exterior2nd[is.na(house$Exterior2nd)]=Mode(house$Exterior2nd)
 house$KitchenQual[is.na(house$KitchenQual)]=Mode(house$KitchenQual)
 house$Functional[is.na(house$Functional)]=Mode(house$Functional)
 house$SaleType[is.na(house$SaleType)]=Mode(house$SaleType)
 house$BsmtQual[is.na(house$BsmtQual)]=Mode(house$BsmtQual)
 house$BsmtCond[is.na(house$BsmtCond)]=Mode(house$BsmtCond)
 house$BsmtExposure[is.na(house$BsmtExposure)]=Mode(house$BsmtExposure)
 house$BsmtHalfBath[is.na(house$BsmtHalfBath)]=Mode(house$BsmtHalfBath)
 house$BsmtFullBath[is.na(house$BsmtFullBath)]=Mode(house$BsmtFullBath)
 
 colSums(is.na(house))
 #summary(house)

 res <- colSums(house==0)/nrow(house)*100
 
 #imputing NA in contnious data set by mean or median
 library(ggplot2)
 #LotFrontage
 ggplot(house, aes(x=house$LotFrontage))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$LotFrontage))+geom_boxplot()
 
 house$LotFrontage[is.na(house$LotFrontage)]=median(house$LotFrontage,na.rm=TRUE)
 
 #MasVnrArea
 ggplot(house, aes(x=house$MasVnrArea))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$MasVnrArea))+geom_boxplot()
 
 house$MasVnrArea[is.na(house$MasVnrArea)]=median(house$MasVnrArea,na.rm=TRUE)
 
 #GarageArea
 ggplot(house, aes(x=house$GarageArea))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$GarageArea))+geom_boxplot()
 
 house$GarageArea[is.na(house$GarageArea)]=median(house$GarageArea,na.rm=TRUE)
 
 #GarageCars
 ggplot(house, aes(x=house$GarageCars))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$GarageCars))+geom_boxplot()
 
 house$GarageCars[is.na(house$GarageCars)]=median(house$GarageCars,na.rm=TRUE)
 
 
 #TotalBsmtSF
 ggplot(house, aes(x=house$TotalBsmtSF))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$TotalBsmtSF))+geom_boxplot()
 
 house$TotalBsmtSF[is.na(house$TotalBsmtSF)]=median(house$TotalBsmtSF,na.rm=TRUE)
 
 #BsmtFinSF2
 ggplot(house, aes(x=house$BsmtFinSF2))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$BsmtFinSF2))+geom_boxplot()
 
 house$BsmtFinSF2[is.na(house$BsmtFinSF2)]=median(house$TotalBsmtSF,na.rm=TRUE)
 
 #BsmtFinSF1
 ggplot(house, aes(x=house$BsmtFinSF1))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$BsmtFinSF1))+geom_boxplot()
 
 house$BsmtFinSF1[is.na(house$BsmtFinSF1)]=median(house$TotalBsmtSF,na.rm=TRUE)
 
 #BsmtUnfSF
 ggplot(house, aes(x=house$BsmtUnfSF))+geom_histogram()
 ggplot(house, aes(x=house$SalePrice,y=house$BsmtUnfSF))+geom_boxplot()
 
 house$BsmtUnfSF[is.na(house$BsmtUnfSF)]=median(house$TotalBsmtSF,na.rm=TRUE)
 
 #write.csv(house,"C:/R/workspace/clean_data.csv")
 #house<-read.csv("clean_data.csv")
 #View(house)
 #str(house)
 colSums(is.na(house))
 
 train <- house[house$isTrain==1,]
 test <- house[house$isTrain==0,]
 
 str(train)
 str(test)
 train<-train[,79]
 test<-test[,-c(78,79)]
 
 write.csv(train,"G:/DATA SCIENCE/Group project/R/Workspace/train.csv")
 write.csv(test,"G:/DATA SCIENCE/Group project/R/Workspace/test.csv")


# barplot(ames$SalePrice,xlab="Neighborhood",ylab="SalesPrice",main="House sale price by Neighborhood", names.arg=ames$Neighborhood,col=green,border="red")
# 
# data_types <- function(frame) {
#   res <- lapply(frame, class)
#   res_frame <- data.frame(unlist(res))
#   barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
# }
# data_types(ames)
# 
# table(unlist(lapply(ames, class)))

