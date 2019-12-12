data1 <- read.csv("Car Pricing and their models/car1.csv")

carDetails <- subset(data1, data1$HorsePower>0)


FuelTypeCode = carDetails$FuelType
levels(FuelTypeCode)
levels(FuelTypeCode)[levels(FuelTypeCode)=="diesel"]=0
levels(FuelTypeCode)[levels(FuelTypeCode)=="gas"]=1
levels(FuelTypeCode)
table(FuelTypeCode)


MakeCode = carDetails$Make
levels(MakeCode)
levels(MakeCode)[levels(MakeCode)=="alfa"]=1
levels(MakeCode)[levels(MakeCode)=="audi"]=2
levels(MakeCode)[levels(MakeCode)=="bmw"]=3
levels(MakeCode)[levels(MakeCode)=="chevrolet"]=4
levels(MakeCode)[levels(MakeCode)=="dodge"]=5
levels(MakeCode)[levels(MakeCode)=="honda"]=6
levels(MakeCode)[levels(MakeCode)=="isuzu"]=7
levels(MakeCode)[levels(MakeCode)=="jaguar"]=8
levels(MakeCode)[levels(MakeCode)=="mazda"]=9
levels(MakeCode)[levels(MakeCode)=="mercedes-benz"]=10
levels(MakeCode)[levels(MakeCode)=="mercury"]=11
levels(MakeCode)[levels(MakeCode)=="mitsubishi"]=12
levels(MakeCode)[levels(MakeCode)=="nissan"]=13
levels(MakeCode)[levels(MakeCode)=="peugot"]=14
levels(MakeCode)[levels(MakeCode)=="plymouth"]=15
levels(MakeCode)[levels(MakeCode)=="porsche"]=16
levels(MakeCode)[levels(MakeCode)=="renault"]=17
levels(MakeCode)[levels(MakeCode)=="saab"]=18
levels(MakeCode)[levels(MakeCode)=="subaru"]=19
levels(MakeCode)[levels(MakeCode)=="toyota"]=20
levels(MakeCode)[levels(MakeCode)=="volkswagen"]=21
levels(MakeCode)[levels(MakeCode)=="volvo"]=22

levels(MakeCode)

table(MakeCode)

cor(carDetails)
library(tibble)

head(carDetails)
carDetails <- add_column(carDetails, MakeCode,FuelTypeCode, .after = 1)
carDetails = carDetails[,-1]
carDetails = carDetails[,-3]

set.seed(9001)
train<-sample(1:nrow(carDetails),150)

head(train)
CarDetrailstrain=carDetails[train, ]
dim(CarDetrailstrain)

LinearModel = lm(Price~EngineSize, data = carDetails, subset = train)
summary(LinearModel)

plot(Price~EngineSize, data = carDetails, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Australian Dollar", main="Price Vs EngineSize", col = "red", pch = 20)
abline(a= -6.385, b=0.152, col = "green", lwd = 2)
legend(100, 45, legend=c("Observation", "Linear"),
       col=c("red", "green"), c( 2, 3),cex=0.8)

polyModel=lm(Price~poly(EngineSize,3), data = carDetails, subset = train) 
summary(polyModel)

polyModel1=lm(Price~poly(CurbWeight,3), data = carDetails, subset = train) 
summary(polyModel1)

plot(Price~EngineSize, data = CarDetrailstrain, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Austalian Dollar", main="Price Vs EngineSize", col = "red", pch = 20)
abline(a= -7.5849, b=0.1639, col = "green", lwd = 2)
lines(smooth.spline(CarDetrailstrain$EngineSize, predict(polyModel)), col= "blue",lwd=2, lty=2)
legend(100, 40, legend=c("Linear", "Polynomial"),
       col=c("green", "blue"), lty=1:2, cex=0.8)

par(mfrow=c(1,1))



plot(Price~CurbWeight, data = CarDetrailstrain, xlab="CurbWeight in grams",ylab = "Price in thousands of Austalian Dollar", main="Price Vs CurbWeight", col = "red", pch = 20)
lines(smooth.spline(CarDetrailstrain$CurbWeight, predict(polyModel)), col= "blue",lwd=2, lty=2,pch=10, type = "b")
legend(1500,40, legend=c("observation","Polynomial"), col=c("red","blue"), pch = c(20, 10),cex=0.8)

InteractionModel = lm(Price~Width+CurbWeight+Width*CurbWeight, data = carDetails, subset = train)
summary(InteractionModel)

plot(Price~CurbWeight, data = CarDetrailstrain,xlab="CurbWeight in grams",ylab = "Price in thousands of Austalian Dollar", main="Price Vs CurbWeight", col = "red", pch = 20)
lines(smooth.spline(CarDetrailstrain$CurbWeight,predict(InteractionModel)), col= "orange",lwd=3, pch=10, type = "b")
lines(smooth.spline(CarDetrailstrain$CurbWeight, predict(polyModel)), col= "blue",lwd=2, lty=2,pch=15, type = "b")
legend(1500,40, legend=c("observation","Interaction", "polynomial"), col=c("red","orange", "blue"), pch = c(20, 10, 15),cex=0.8)


MultiLinearModel = lm(Price~EngineSize+CurbWeight+Width+MPG , data = carDetails, subset = train)
summary(MultiLinearModel)
anova(MultiLinearModel)

plot(Price~EngineSize, data = CarDetrailstrain, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Australian Dollar", main="Price Vs EngineSize", col = "red", pch = 20)
abline(a= -6.385, b=0.152, col = "green", lwd = 2)
lines(smooth.spline(CarDetrailstrain$EngineSize, predict(MultiLinearModel)), col= "blue",lwd=2, lty=2,pch=15, type = "b")
legend(100, 45, legend=c("Observation", "Linear", "MultipleLinear"),
       col=c("red", "green", "blue"), c( 2, 3, 4),cex=0.8)

HybridModel = lm(Price~EngineSize+Width+CurbWeight+Width*CurbWeight+I(EngineSize*EngineSize*EngineSize) , data = carDetails, subset = train)
summary(HybridModel)

selectModel = lm(Price~EngineSize+Width+CurbWeight+Width*CurbWeight+I(CurbWeight*CurbWeight) , data = carDetails, subset = train)
summary(selectModel)

anova(selectModel)

plot(predict(selectModel),resid(selectModel), xlab = "Fitted Values", ylab = "Residuals")

hist(resid(selectModel), main = paste("Histogram of Residuals"), xlab = "Residuals", col= "deeppink4")



par(mfrow=c(2,2))
plot(selectmodel)

plot(MultiLinearModel, scale. = TRUE)

plot(selectmodel, scale. = TRUE)

par(mfrow=c(2,2))
plot(MultiLinearModel)


library(DAAG)
par(mfrow=c(1,1))

dim(carDetails[-train,])

carDetailstest <- carDetails[-train,]
carDetailstest <- rbind(carDetailstest, carDetails[-train,])
carDetailstest <- rbind(carDetailstest, carDetails[-train,])

dim(test)

cv.lm(data = carDetailstest, selectmodel, m= 10)

head(carDetails)




#end Regression

HighPrice=ifelse(carDetails$Price<=13.17,"No","Yes")

HighPrice <- factor(HighPrice)

head(carDetails)
View(carDetails)

set.seed(9003)
trainClass<-sample(1:nrow(carDetailsClass),100)
testClass<-Carseats_New[-train,]

carDetailsClasstrain=carDetails[trainClass, ]
dim(carDetailsClasstrain)

carDetailsClasstest = sample(carDetails[-trainClass, ], 100)
dim(carDetailsClasstest)



carDetailsClass <- carDetails
library(tibble)
carDetailsClass <- add_column(carDetailsClass, HighPrice, .after = 15)
carDetailsClasstrain = carDetailsClasstrain[,-17]
head(carDetailsClass)

carDetailsClass <- add_column(carDetailsClass, HighPrice, .after = 15)
carDetailsClass = carDetailsClass[,-17]
head(carDetailsClass)

dim(carDetailsClass)




HighPriceCode = carDetailsClass$HighPrice
levels(HighPriceCode)
table(HighPriceCode)
table(HighPrice)
levels(HighPriceCode)[levels(HighPriceCode)=="No"]=0
levels(HighPriceCode)[levels(HighPriceCode)=="Yes"]=1
levels(HighPriceCode)
HighPriceCode <- as.numeric(HighPriceCode)

HighPriceCode <- as.numeric(levels(HighPriceCode))[HighPriceCode]
table(HighPriceCode)

carDetailsClass <- add_column(carDetailsClass, HighPriceCode, .after = 15)
carDetailsClass = carDetailsClass[,-16]

plot(EngineSize~HighPriceCode, data = carDetailsClass)



LinearLogistic = glm(HighPriceCode~EngineSize, data = carDetailsClasstrain)
summary(LinearLogistic)

cv.glm(carDetailsClass,LinearLogistic, K = 10)$delta[1]

glm_prob<-predict(LinearLogistic, type="response")

glm_pred<- rep("No", 203)
glm_pred[glm_prob>0.5]="Yes"

table(glm_pred,HighPrice)

glm_prob


plot(carDetailsClass$EngineSize, HighPriceCode1, data = carDetailsClass, ylab = "Probability of Default", xlab="Balance")
abline(a= -0.396161, b=0.007084)

model2=glm(HighPriceCode1~EngineSize, data = carDetailsClass, family=binomial)
summary.glm(model2)
plot(carDetailsClass$EngineSize, model2$fitted.values, ylab = "Fitted_probabilities", xlab="EngineSize")


tree1<-tree(HighPriceCode1~.,carDetailsClass)
plot(tree1)
text(tree1, pretty=0)

cv.glm(carDetailsClass$EngineSize, model2$fitted.values, )


plot(predict(MultiLinearModel),resid(MultiLinearModel), xlab = "Fitted Values", ylab = "Residuals")

set.seed(700)
trainData<-sample(1:nrow(carDetails),101)
reg_tree_train<-tree(Price~.,carDetails,subset = trainData)
plot(reg_tree_train)
text(reg_tree_train, pretty = 0)

view(carDetails)

summary(reg_tree_train)

cv_regtree_tr<-cv.tree(reg_tree_train)

plot(cv_regtree$size, cv_regtree$dev, type = "b", 
     col= "red", xlab = "Size", ylab = "Dev", main = "Mean Residual Error")

pruned_regtree_tr<-prune.tree(reg_tree_train, best = 4)

plot(pruned_regtree_tr)
text(pruned_regtree_tr, pretty=0)


Pricehat<-predict(pruned_regtree_tr, newdata = carDetails[-trainData,])
carDetails_test<-carDetails[-trainData,"Price"]

MSE<-mean((Pricehat-carDetails_test)^2)
sqrt(MSE)

tree_model<-tree(Price~.,data = carDetails)
plot(tree_model)
text(tree_model,pretty=0, cex = 0.7)

cv_regtree<-cv.tree(tree_model, FUN=prune.misclass) 

plot(cv_regtree$size, cv_regtree$dev, type = "b")

InteractionModel1 = lm(Price~Width+CurbWeight+Width*CurbWeight, data = carDetails)
summary(InteractionModel1)

pairs(carDetails, panel = panel.smooth)

polyModel=lm(Price~poly(EngineSize,3), data = carDetails) 
summary(polyModel)

selectmodel = lm(Price~Width+CurbWeight+Width*CurbWeight+EngineSize+I(EngineSize*EngineSize*EngineSize), data = carDetails)
summary(selectmodel)


HighPrice=ifelse(carDetails$Price<=10.295,"No","Yes")
carDetails=data.frame(carDetails,HighPrice)
head(carDetails)




HighPriceCode = carDetails$HighPrice
levels(HighPriceCode)
levels(HighPriceCode)[levels(HighPriceCode)=="No"]=0
levels(HighPriceCode)[levels(HighPriceCode)=="Yes"]=1
levels(HighPriceCode)
HighPriceCode = as.numeric(HighPriceCode)
class(HighPriceCode)
table(HighPriceCode)


carDetails <- carDetails[,-17]
View(carDetails)
library(tibble)
carDetails <- add_column(carDetails, HighPriceCode, .after = 16)

set.seed(7001)
trainData1<-sample(1:nrow(carDetails),154)
model4=glm(HighPriceCode~Width+CurbWeight+Width*CurbWeight+EngineSize+I(EngineSize*EngineSize*EngineSize), data=carDetails,subset = trainData1, family=binomial)
summary(model4)

plot(HighPriceCode~Width+CurbWeight, data = carDetails, subset = trainData1, col= carDetails$HighPriceCode, pch = 20 )




#End Classification


ToyotaCarDetails <- subset(carDetails, carDetails$MakeCode==20)
dim(ToyotaCarDetails)

ToyotaselectModel = lm(Price~EngineSize+Width+CurbWeight+Width*CurbWeight+I(CurbWeight*CurbWeight) , data = ToyotaCarDetails)
summary(ToyotaselectModel)
anova(ToyotaselectModel)

plot(Price~EngineSize, data = ToyotaCarDetails, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Austalian Dollar", main="Price Vs EngineSize", col = "blue", pch = 20)
lines(smooth.spline(ToyotaCarDetails$EngineSize,predict(ToyotaselectModel)), col= "blue",lwd=2, lty=1)
par(new = TRUE)
plot(Price~EngineSize, data = NissanCarDetails, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Austalian Dollar", main="Price Vs EngineSize", col = "green", pch = 20)
lines(smooth.spline(NissanCarDetails$EngineSize,predict(NissanselectModel)), col= "green",lwd=2, lty=1)
par(new = TRUE)
plot(Price~EngineSize, data = MitsubishiCarDetails, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Austalian Dollar", main="Price Vs EngineSize", col = "red", pch = 20)
lines(smooth.spline(MitsubishiCarDetails$EngineSize,predict(MitsubishiselectModel)), col= "red",lwd=2, lty=1)
legend(100, 14, legend=c("Totota", "Nissan", "Mitsubishi"),col=c("green", "blue", "red"), c( 3, 4, 2), cex=0.8)

NissanCarDetails <- subset(carDetails, carDetails$MakeCode==13)
dim(NissanCarDetails)

NissanselectModel = lm(Price~EngineSize+Width+CurbWeight+Width*CurbWeight+I(CurbWeight*CurbWeight) , data = NissanCarDetails)
summary(NissanselectModel)
anova(NissanselectModel)

#subaru, 21, 22, 12-96%, p-bad, 6- 94%,p-good
MitsubishiCarDetails <- subset(carDetails, carDetails$MakeCode==12)
dim(HondaCarDetails)

MitsubishiselectModel = lm(Price~EngineSize+Width+CurbWeight+Width*CurbWeight+I(CurbWeight*CurbWeight) , data = MitsubishiCarDetails)
summary(MitsubishiselectModel)
anova(MitsubishiselectModel)

par(mfrow=c(1,3))

plot(Price~EngineSize, data = ToyotaCarDetails, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Austalian Dollar", main="Price Vs EngineSize", col = "blue", pch = 20)
lines(smooth.spline(ToyotaCarDetails$EngineSize,predict(ToyotaselectModel)), col= "blue",lwd=2, lty=1)
par(new = TRUE)
plot(Price~EngineSize, data = NissanCarDetails, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Austalian Dollar", main="Price Vs EngineSize", col = "green", pch = 20)
lines(smooth.spline(NissanCarDetails$EngineSize,predict(NissanselectModel)), col= "green",lwd=2, lty=1)
par(new = TRUE)
plot(Price~EngineSize, data = MitsubishiCarDetails, xlab="EngineSize in Cubic Inches",ylab = "Price in thousands of Austalian Dollar", main="Price Vs EngineSize", col = "red", pch = 20)
lines(smooth.spline(MitsubishiCarDetails$EngineSize,predict(MitsubishiselectModel)), col= "red",lwd=2, lty=1)
legend(100, 14, legend=c("Totota", "Nissan", "Mitsubishi"),col=c("green", "blue", "red"), c( 3, 4, 2), cex=0.8)



plot(Price~CurbWeight, data = ToyotaCarDetails, xlab="CurbWeight in grams",ylab = "Price in thousands of Austalian Dollar", main="Price Vs CurbWeight", col = "blue", pch = 20)
lines(smooth.spline(ToyotaCarDetails$CurbWeight,predict(ToyotaselectModel)), col= "blue",lwd=2, lty=1)
par(new = TRUE)
plot(Price~CurbWeight, data = NissanCarDetails, xlab="CurbWeight in gramss",ylab = "Price in thousands of Austalian Dollar", main="Price Vs CurbWeight", col = "green", pch = 20)
lines(smooth.spline(NissanCarDetails$CurbWeight,predict(NissanselectModel)), col= "green",lwd=2, lty=1)
par(new = TRUE)
plot(Price~CurbWeight, data = MitsubishiCarDetails, xlab="CurbWeight in grams",ylab = "Price in thousands of Austalian Dollar", main="Price Vs CurbWeight", col = "red", pch = 20)
lines(smooth.spline(MitsubishiCarDetails$CurbWeight,predict(MitsubishiselectModel)), col= "red",lwd=2, lty=1)
legend(2000, 14, legend=c("Totota", "Nissan", "Mitsubishi"),col=c("green", "blue", "red"), c( 3, 4, 2), cex=0.5)

plot(Price~Width, data = ToyotaCarDetails, xlab="Width in centimeter",ylab = "Price in thousands of Austalian Dollar", main="Price Vs Width", col = "blue", pch = 20)
lines(smooth.spline(ToyotaCarDetails$Width,predict(ToyotaselectModel)), col= "blue",lwd=2, lty=1)
par(new = TRUE)
plot(Price~Width, data = NissanCarDetails, xlab="Width in centimeter",ylab = "Price in thousands of Austalian Dollar", main="Price Vs Width", col = "green", pch = 20)
lines(smooth.spline(NissanCarDetails$Width,predict(NissanselectModel)), col= "green",lwd=2, lty=1)
par(new = TRUE)
plot(Price~Width, data = MitsubishiCarDetails, xlab="Width in centimeter",ylab = "Price in thousands of Austalian Dollar", main="Price Vs Width", col = "red", pch = 20)
lines(smooth.spline(MitsubishiCarDetails$Width,predict(MitsubishiselectModel)), col= "red",lwd=2, lty=1)
legend(65, 14, legend=c("Totota", "Nissan", "Mitsubishi"),col=c("green", "blue", "red"), c( 3, 4, 2), cex=0.8)
