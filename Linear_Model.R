#__________________________________________________________________________________________

#-------------------------4.Applying Feature Engineering------------------------------------

#-------------------------Reading Data--------------------------------------------
setwd("G:/DATA SCIENCE/Group project/R/Workspace")
AMES=read.csv("G:/DATA SCIENCE/Group project/R/Workspace/train.csv")
#View(AMES)
str(AMES)
AMES<-AMES[,-c(1,2,78)]
#View(AMES)
str(AMES)
AMES[,75]
#---------------------------------------------------------------------------------

## Apply corrplot on Numeric variable
library(corrplot)
numeric_data <- AMES[sapply(AMES, is.numeric)]
descrCor <- cor(numeric_data)
#View(descrCor)
print(descrCor)
corrplot(descrCor)

relationdf=data.frame()
relationdf<-unlist(lapply(AMES, class))
relationdf<-as.data.frame(relationdf)
relationdf$pval = 0

class(relationdf)
View(relationdf)
str(relationdf)


#just for checking..
relationdf[45,1]=="integer"
AMES[,2]
AMES$SalePrice


#---------Apply Correlation and Chi-sqaure  and filter significant columns--------------

#Check reation between independent variable and target variable.
for(i in 1:length(AMES)){
  print(i)
  if(relationdf[i,1] == "integer"){
    relationdf$pval[i] = cor(AMES[,i], AMES$SalePrice)
    relationdf$pval[i]
    relationdf$testname[i]<-"correlation"
  }
  else{
    anova.test <- aov(AMES$SalePrice ~ AMES[,i] )
    sum_test = unlist(summary(anova.test))
    #names(sum_test)
    relationdf$pval[i] =  sum_test["Pr(>F)1"]
     relationdf$testname[i]<-"ANOVA"
  }
}

print(relationdf)
View(relationdf)

#sum(relationdf$testname == "ANOVA" & relationdf$pval <= 0.05)



relationdf$matchexpr = "mismatch"
relationdf$matchexpr[relationdf$testname == "ANOVA" & relationdf$pval <= 0.05] = "match"
relationdf$matchexpr[relationdf$testname == "correlation" & 
                       (relationdf$pval>=0.30  & relationdf$pval <=0.80 )]  = "match"
relationdf$matchexpr[relationdf$testname == "correlation" & 
                       (relationdf$pval<=-0.30  & relationdf$pval >=-0.80 )]  = "match"


#backup<-relationdf
# relationdf<-backup
# relationdf<-relationdf[-1,]

str(relationdf)
View(relationdf)

  finalcols<-relationdf[(which(relationdf$matchexpr=="match")),]  
  
View(finalcols)
class(finalcols)

print(rownames(finalcols))
FinalCol<-rownames(finalcols)

final_data<-AMES[rownames(finalcols)]
View(final_data)

imp<-final_data
SalePrice<-AMES[,75]
SalePrice<-as.data.frame(SalePrice)
final_data<-cbind(imp,SalePrice)

write.csv(final_data,"G:/DATA SCIENCE/Group project/R/Workspace/significant_data.csv")

#------------check multi- colinearity [Numeric data]----------------

numeric_data<-relationdf[which(relationdf$testname == "correlation"),]
numeric_data<-numeric_data[which(numeric_data$matchexpr == "match"),]
View(numeric_data)

print(rownames(numeric_data))
View(AMES[rownames(numeric_data)])

numeric <- AMES[rownames(numeric_data)]
#descrCor <- cor(numeric)
#library(corrplot)
# View(descrCor)
# print(descrCor)
#corrplot(descrCor)



multi_co=data.frame()
multi_co<-unlist(lapply(numeric, class))
multi_co<-as.data.frame(multi_co)
multi_co$correl = 0

class(multi_co)
View(multi_co)
str(multi_co)


#just for checking..
multi_co[18,1]=="integer"
numeric[,2]
numeric$OpenPorchSF

for(i in 1:length(numeric)){
  print(i)
  if(multi_co[i,1] == "integer"){
    multi_co$correl = cor(numeric)
    multi_co$correl
  }
}

#print(multi_co$correl)
View(multi_co$correl)

#Apply criteria for multi co linerarity >=90%
View( multi_co$correl>=0.90)
#Note-There is no Multi-c olinearity within Independent variable..


#--------------------------Apply chi square----------------------------

factor_data<-relationdf[which(relationdf$testname == "ANOVA"),]
factor_data<-factor_data[which(factor_data$matchexpr == "match"),]
View(factor_data)

print(rownames(factor_data))
View(AMES[rownames(factor_data)])

categorical_data <- AMES[rownames(factor_data)]

#just for checking..
class(categorical_data)
View(categorical_data)
str(categorical_data)

categorical_data[,35]
categorical_data$SaleCondition

library(rlist)
df <- categorical_data    #this supposed to be a data frame
x<-list()       # return var as a list
for (i in 1:length(df))
{
  if(i < length(df)){ k= i+1 }
  if((class(df[,i])== class(df[,k])) & class(df[,i])=='factor' )
  {
    y<-  chisq.test(table(df[,i], df[,k]))
    vec<-c(Var=paste(colnames(df[i]),"-",colnames(df[k])), y$statistic, pval=y$p.value, significant = (if(y$p.value<0.05 & y$p.value!='NaN') "yes" else "no")) 
    x<<-list.append(x, vec)
  }
}
print(x)
View(x)
# library(plyr)
# 
# combos <- combn(ncol(categorical_data),2)
# 
# out<-adply(combos, 2, function(x) {
#   test <- chisq.test(categorical_data[, x[1]], categorical_data[, x[2]])
#   
#   out <- data.frame("Row" = colnames(categorical_data)[x[1]]
#                     , "Column" = colnames(categorical_data[x[2]])
#                     , "Chi.Square" = round(test$statistic,3)
#                     ,  "df"= test$parameter
#                     ,  "p.value" = round(test$p.value, 3)
#   )
#   return(out)
#   
# })  
# 
# View(out)
# print(out)
# class(out)

write.csv(out,"G:/DATA SCIENCE/Group project/R/Workspace/chiSqTest.csv")

#-----------------------------------------------------------------
#-------------------------5.EDA-Numeric data-----------------------
#-------------------------outlier treatment------------------------

ames<-read.csv("G:/DATA SCIENCE/Group project/R/Workspace/significant_data.csv")
str(ames)
library(ggplot2)

#  #to check outliers by means of quantile.
out_qua_fix = function(x,m){
  
  Q1 = quantile(x, 0.25)
  Q3 = quantile(x, 0.75)
  IQR = Q3 - Q1
  lc = Q1 - m*IQR #lower cut-off
  uc = Q3 + m*IQR #upper cut-off
  out_value_count=sum(x>uc | x<lc)
  out_value <- which(x > uc | x < lc)
  
  for (i in 1:length(out_value)){
    x[out_value[i]]<-median(x)
  }
  
  val=list(count=out_value_count,outlier=out_value,lower_cutoff=lc,upper_cutoff=uc)
  print(val)
  return(x)
}

ggplot(ames, aes(x=ames$SalePrice))+geom_histogram()
summary(ames$SalePrice)
#run 3 times
ames$SalePrice <- out_qua_fix(ames$SalePrice, 1.5)


#----------------------------


#1.OverallQual
ggplot(ames,aes(y=ames$SalePrice,x=ames$OverallQual))+geom_point()
ggplot(ames, aes(x=ames$OverallQual))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$OverallQual))+geom_boxplot()

# summary(ames$OverallQual)
# ames$OverallQual <- out_qua_fix(ames$OverallQual, 1.5)


#2.LotFrontage(2)
ggplot(ames,aes(y=ames$SalePrice,x=ames$LotFrontage))+geom_point()
ggplot(ames, aes(x=ames$LotFrontage))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$LotFrontage))+geom_boxplot()
summary(ames$LotFrontage)
ames$LotFrontage <- out_qua_fix(ames$LotFrontage, 1.5)


#3.YearBuilt
ggplot(ames,aes(y=ames$SalePrice,x=ames$YearBuilt))+geom_point()
ggplot(ames, aes(x=ames$YearBuilt))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$YearBuilt))+geom_boxplot()


#4.BsmtFinSF1(2)
ggplot(ames,aes(y=ames$SalePrice,x=ames$BsmtFinSF1))+geom_point()
ggplot(ames, aes(x=ames$BsmtFinSF1))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$BsmtFinSF1))+geom_boxplot()

summary(ames$BsmtFinSF1)
ames$BsmtFinSF1 <- out_qua_fix(ames$BsmtFinSF1, 1.5)



#5.YearRemodAdd
ggplot(ames,aes(y=ames$SalePrice,x=ames$YearRemodAdd))+geom_point()
ggplot(ames, aes(x=ames$YearRemodAdd))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$YearRemodAdd))+geom_boxplot()



#6.TotalBsmtSF(till 0)
ggplot(ames,aes(y=ames$SalePrice,x=ames$TotalBsmtSF))+geom_point()
ggplot(ames, aes(x=ames$TotalBsmtSF))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$TotalBsmtSF))+geom_boxplot()

summary(ames$TotalBsmtSF)
ames$TotalBsmtSF <- out_qua_fix(ames$TotalBsmtSF, 1.5)


#7.X1stFlrSF(till 0)
ggplot(ames,aes(y=ames$SalePrice,x=ames$X1stFlrSF))+geom_point()
ggplot(ames, aes(x=ames$X1stFlrSF))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$X1stFlrSF))+geom_boxplot()

summary(ames$X1stFlrSF)
ames$X1stFlrSF <- out_qua_fix(ames$X1stFlrSF, 1.5)

#8.X2ndFlrSF (till 0)
ggplot(ames,aes(y=ames$SalePrice,x=ames$X2ndFlrSF))+geom_point()
ggplot(ames, aes(x=ames$X2ndFlrSF))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$X2ndFlrSF))+geom_boxplot()

summary(ames$X2ndFlrSF)
ames$X2ndFlrSF <- out_qua_fix(ames$X2ndFlrSF, 1.5)


#9.GrLivArea (Till 0)
ggplot(ames,aes(y=ames$SalePrice,x=ames$GrLivArea))+geom_point()
ggplot(ames, aes(x=ames$GrLivArea))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$GrLivArea))+geom_boxplot()

summary(ames$GrLivArea)
ames$GrLivArea <- out_qua_fix(ames$GrLivArea, 1.5)

#10.MasVnrArea(No outlier treatment)
ggplot(ames,aes(y=ames$SalePrice,x=ames$MasVnrArea))+geom_point()
ggplot(ames, aes(x=ames$MasVnrArea))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$MasVnrArea))+geom_boxplot()

summary(ames$MasVnrArea)
ames$MasVnrArea <- out_qua_fix(ames$MasVnrArea, 1.5)

#11.TotRmsAbvGrd (No outlier treatment)
ggplot(ames,aes(y=ames$SalePrice,x=ames$TotRmsAbvGrd))+geom_point()
ggplot(ames, aes(x=ames$TotRmsAbvGrd))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$TotRmsAbvGrd))+geom_boxplot()

summary(ames$TotRmsAbvGrd)
ames$TotRmsAbvGrd <- out_qua_fix(ames$TotRmsAbvGrd, 1.5)


#12.Fireplaces (No outlier treatment)
ggplot(ames,aes(y=ames$SalePrice,x=ames$Fireplaces))+geom_point()
ggplot(ames, aes(x=ames$Fireplaces))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$Fireplaces))+geom_boxplot()

summary(ames$Fireplaces)

#13.GarageYrBlt
ggplot(ames,aes(y=ames$SalePrice,x=ames$GarageYrBlt))+geom_point()
ggplot(ames, aes(x=ames$GarageYrBlt))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$GarageYrBlt))+geom_boxplot()

summary(ames$GarageYrBlt)

#14.GarageCars
ggplot(ames,aes(y=ames$SalePrice,x=ames$GarageCars))+geom_point()
ggplot(ames, aes(x=ames$GarageCars))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$GarageCars))+geom_boxplot()

#15.GarageArea (till 0)
ggplot(ames,aes(y=ames$SalePrice,x=ames$GarageArea))+geom_point()
ggplot(ames, aes(x=ames$GarageArea))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$GarageArea))+geom_boxplot()

summary(ames$GarageArea)
ames$GarageArea <- out_qua_fix(ames$GarageArea, 1.5)


#16.OpenPorchSF(7)
ggplot(ames,aes(y=ames$SalePrice,x=ames$OpenPorchSF))+geom_point()
ggplot(ames, aes(x=ames$OpenPorchSF))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$OpenPorchSF))+geom_boxplot()

summary(ames$OpenPorchSF)
ames$OpenPorchSF <- out_qua_fix(ames$OpenPorchSF, 1.5)

#17.WoodDeckSF(till 0)
ggplot(ames,aes(y=ames$SalePrice,x=ames$WoodDeckSF))+geom_point()
ggplot(ames, aes(x=ames$WoodDeckSF))+geom_histogram()
ggplot(ames, aes(x=ames$SalePrice,y=ames$WoodDeckSF))+geom_boxplot()

summary(ames$WoodDeckSF)
ames$WoodDeckSF <- out_qua_fix(ames$WoodDeckSF, 1.5)

#-------------------------------EDA - Categorical--------------------------
#MSZoning
ggplot(ames, aes(x=ames$MSZoning))+geom_bar()
summary(ames$MSZoning)

#LotShape
ggplot(ames, aes(x=ames$LotShape))+geom_bar()
summary(ames$LotShape)

#LandContour
ggplot(ames, aes(x=ames$LandContour))+geom_bar()
summary(ames$LandContour)

#LotConfig
ggplot(ames, aes(x=ames$LotConfig))+geom_bar()
summary(ames$LotConfig)

#Neighborhood
ggplot(ames, aes(x=ames$Neighborhood))+geom_bar()
summary(ames$Neighborhood)

#Condition1
ggplot(ames, aes(x=ames$Condition1))+geom_bar()
summary(ames$Condition1)

#Condition2
ggplot(ames, aes(x=ames$Condition2))+geom_bar()
summary(ames$Condition2)

#BldgType
ggplot(ames, aes(x=ames$BldgType))+geom_bar()
summary(ames$BldgType)

#HouseStyle
ggplot(ames, aes(x=ames$HouseStyle))+geom_bar()
summary(ames$HouseStyle)

#RoofStyle
ggplot(ames, aes(x=ames$RoofStyle))+geom_bar()
summary(ames$RoofStyle)

#RoofMatl
ggplot(ames, aes(x=ames$RoofMatl))+geom_bar()
summary(ames$RoofMatl)

#Exterior1st
ggplot(ames, aes(x=ames$Exterior1st))+geom_bar()
summary(ames$Exterior1st)

#MasVnrType
ggplot(ames, aes(x=ames$MasVnrType))+geom_bar()
summary(ames$MasVnrType)

#ExterQual
ggplot(ames, aes(x=ames$ExterQual))+geom_bar()
summary(ames$ExterQual)

#ExterCond
ggplot(ames, aes(x=ames$ExterCond))+geom_bar()
summary(ames$ExterCond)

#Foundation
ggplot(ames, aes(x=ames$Foundation))+geom_bar()
summary(ames$Foundation)

#BsmtQual
ggplot(ames, aes(x=ames$BsmtQual))+geom_bar()
summary(ames$BsmtQual)

#BsmtCond
ggplot(ames, aes(x=ames$BsmtCond))+geom_bar()
summary(ames$BsmtCond)

#BsmtExposure
ggplot(ames, aes(x=ames$BsmtExposure))+geom_bar()

#BsmtFinType1
ggplot(ames, aes(x=ames$BsmtFinType1))+geom_bar()
summary(ames$BsmtFinType1)

#BsmtFinType2
ggplot(ames, aes(x=ames$BsmtFinType2))+geom_bar()
summary(ames$BsmtFinType2)

#Heating
ggplot(ames, aes(x=ames$Heating))+geom_bar()
summary(ames$Heating)

#HeatingQC
ggplot(ames, aes(x=ames$HeatingQC))+geom_bar()
summary(ames$HeatingQC)

#CentralAir
ggplot(ames, aes(x=ames$CentralAir))+geom_bar()
summary(ames$CentralAir)

#Electrical
ggplot(ames, aes(x=ames$Electrical))+geom_bar()
summary(ames$Electrical)

#KitchenQual
ggplot(ames, aes(x=ames$MSZoning))+geom_bar()
summary(ames$MSZoning)


#Functional
ggplot(ames, aes(x=ames$Functional))+geom_bar()
summary(ames$Functional)

#GarageType
ggplot(ames, aes(x=ames$GarageType))+geom_bar()
summary(ames$GarageType)


#GarageFinish
ggplot(ames, aes(x=ames$GarageFinish))+geom_bar()
summary(ames$GarageFinish)


#GarageQual
ggplot(ames, aes(x=ames$GarageQual))+geom_bar()
summary(ames$GarageQual)


#GarageCond
ggplot(ames, aes(x=ames$GarageCond))+geom_bar()
summary(ames$GarageCond)

#PavedDrive
ggplot(ames, aes(x=ames$PavedDrive))+geom_bar()
summary(ames$PavedDrive)

#SaleType
ggplot(ames, aes(x=ames$SaleType))+geom_bar()
summary(ames$SaleType)

#SaleCondition
ggplot(ames, aes(x=ames$SaleCondition))+geom_bar()
summary(ames$SaleCondition)


#---------------------factorizing--(categorical variable)-----------------------------
?factor
str(ames)
View(ames)
ames<-ames[,-c(1,56)]


ames$MSZoning<- as.factor(ames$MSZoning)
ames$LotShape <-as.factor(ames$LotShape )
ames$LandContour<-as.factor(ames$LandContour)
ames$LotConfig<-as.factor(ames$LotConfig)
ames$Neighborhood<-as.factor(ames$Neighborhood)
ames$Condition1<-as.factor(ames$Condition1)
ames$Condition2<-as.factor(ames$Condition2)
ames$BldgType<-as.factor(ames$BldgType)
ames$HouseStyle<-as.factor(ames$HouseStyle)
ames$RoofStyle<-as.factor(ames$RoofStyle)
ames$RoofMatl<-as.factor(ames$RoofMatl)
ames$Exterior1st<-as.factor(ames$Exterior1st)
ames$Exterior2nd<-as.factor(ames$Exterior2nd)
ames$MasVnrType<-as.factor(ames$MasVnrType)
ames$ExterQual<-as.factor(ames$ExterQual)
ames$ExterCond<-as.factor(ames$ExterCond)
ames$Foundation<-as.factor(ames$Foundation)
ames$BsmtQual<-as.factor(ames$BsmtQual)
ames$BsmtCond<-as.factor(ames$BsmtCond)
ames$BsmtExposure<-as.factor(ames$BsmtExposure)
ames$BsmtFinType1<-as.factor(ames$BsmtFinType1)
ames$BsmtFinType2<-as.factor(ames$BsmtFinType2)
ames$Heating<-as.factor(ames$Heating)
ames$HeatingQC<-as.factor(ames$HeatingQC)
ames$CentralAir<-as.factor(ames$CentralAir)
ames$Electrical<-as.factor(ames$Electrical)
ames$KitchenQual<-as.factor(ames$KitchenQual)
ames$Functional<-as.factor(ames$Functional)
ames$GarageType<-as.factor(ames$GarageType)
ames$GarageFinish<-as.factor(ames$GarageFinish)
ames$GarageQual<-as.factor(ames$GarageQual)
ames$GarageCond<-as.factor(ames$GarageCond)
ames$PavedDrive<-as.factor(ames$PavedDrive)
ames$SaleType<-as.factor(ames$SaleType)
ames$SaleCondition<-as.factor(ames$SaleCondition)

str(ames)
View(ames)
print(is.factor(ames))

#write.csv(ames,"G:/DATA SCIENCE/Group project/R/Workspace/outlierTeatedData/final_data.csv")
ames<-read.csv("G:/DATA SCIENCE/Group project/R/Workspace/outlierTeatedData/final_data.csv")
str(ames)

#----------------One hot encoding---------------------------
library(fastDummies)
library(data.table)
Dummy<-dummy_cols(ames) # it will convert your data to hot encoding
str(Dummy)
View(Dummy)
house<-Dummy

ZeroValue<-(as.data.table(Dummy)[, lapply(.SD, function(x) sum(x==0))]/nrow(Dummy))*100

class(ZeroValue)
ZeroValue<-as.data.frame(ZeroValue)

SignifCols<-ZeroValue[,-which(ZeroValue>99)]
class(SignifCols)
str(SignifCols)
colnames(SignifCols)

originalData<-(Dummy[,colnames(SignifCols)])

str(originalData)
View(originalData)

df = originalData[,!(names(originalData) %in% c(rownames(factor_data)))]

class(df)
str(df)
View(df)

#ames<-ames[,-173]
View(ames)

write.csv(ames,"G:/DATA SCIENCE/Group project/R/Workspace/one-hotencoding/ames.csv")
#-----------------Check linearity-scatterplot-------------------------

#SaleCondition
plot(x=AMES$SaleCondition,y=AMES$SalePrice,
     xlab="sale condition",
     ylab="sale Price",
     main="sale condition vs sale price")



#--------------------6.Divide Dataset into Training and testing set----------------------
library(splitstackshape)
library(dplyr)
ames<-read.csv("G:/DATA SCIENCE/Group project/R/Workspace/one-hotencoding/ames.csv")
str(ames)
ames<-ames[,-1]

set.seed(105)

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

train$SalePrices

# summary(train$RoofMatl)

#------------------------7.AMES Housing Sales price predictve model-----------------------------
#------------------------Linear Regression--------------------------------------------------
Fit=lm(data = (train),SalePrices ~ . -X)
summary(Fit)


#------ Grab residuals------------
res <- residuals(Fit)
res <- as.data.frame(res)

head(res)
library(ggplot2)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)


#validation
library(gvlma)
require(lmSupport)
lm.modelAssumptions(mod, "linear")
gvlma::gvlma(Fit)

library(DAAG)
library(lattice)
# # K-fold cross-validation - why this is not fit for this.
cv.lm(train, form.lm = formula(SalePrices ~ . -X), m=5, dots=FALSE, seed=29, plotit=TRUE, printit=TRUE)





#Model Selection
#stepwise sleection:

nullModel<- lm(SalePrices ~ 1, train)

#summary(nullModel)

fullmodel <- lm(SalePrices ~ .-X,data = (train))
#summary(fullmodel)

fit <- step(nullModel, scope=list(lower=nullModel, upper=fullmodel), direction="both")

#revel the model
fit

#anova() - pass the multiple model.
#AIC ()
#BIC () - pass the multiple modle
#---------------------------------------------------------------------------------

#---------------------------------Predict value------------------------------------------------
 #HP_Test_Data=read.csv("test.csv")
# # na.strings=c("","","NA")

#prediction.
pred <- predict(fit, test1)


View(pred)
# New <- cbind(test,pred)
# View(New)



#--------------------------Calculate Accuracy-------------------------------------------------

results <- cbind(pred,test$SalePrice) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
View(results)


#There's lots of ways to evaluate the prediction values, for example the MSE (mean squared error):
mse <- mean((results$real-results$pred)^2)
paste("MSE=",mse)

#Or the root mean squared error:
RMSE<-mse^0.5
paste("RMSE=",RMSE)
#Or just the R-Squared Value for our model (just for the predictions)

SSE = sum((results$real-results$pred)^2)
SST = sum( (results$real - mean(ames$SalePrices))^2)
SSR = sum((results$pred-mean(ames$SalePrices))^2)

R2= SSR/SST 
paste("R2=",R2)

#Adjusted R2 value
oneminus.R2=1-R2

Adj.R2= 1-((oneminus.R2*(438-1))/(438-56-1))
paste("Adjusted R2=",Adj.R2)
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
median(finalvalid$err_rate) # 12181 $
median(finalvalid$pcterr)  #7.45 %

paste(" median prediction error in $ ",median(finalvalid.err_rate))

paste(" median prediction error as % difference from correct value ",median(finalvalid.pcterr))

#----------------------------------------------------------------------------------------------------