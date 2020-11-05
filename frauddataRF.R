fraudcheck = read.csv(file.choose())
fraudcheck
colnames(fraudcheck)
########################################################################################
?cut
###we will create taxable.income categories of
###Risky = <= 30000  Good = >30000 

CutTaxable.Income= cut(fraudcheck$Taxable.Income,breaks =2,labels = c("Risky","Good"),right = FALSE)

fraudcheck$Taxable.Income[1:10]
CutTaxable.Income[1:10]
str(CutTaxable.Income)
summary(CutTaxable.Income)
#################################################################################################
library(dplyr)
taxable_income <- factor(if_else(fraudcheck$Taxable.Income <= 30000,"Risky","Good"))
data <- data.frame(fraudcheck[,1:2],taxable_income,fraudcheck[,4:6])
str(data)
summary(data)




# Splitting data into training and testing. 



data_Risky<-fraudcheck[data$taxable_income=="Risky",] #124
data_Good <- fraudcheck[data$taxable_income=="Good",] # 476

data_train <- rbind(data_Risky[1:62,],data_Good[1:238,])
data_test <- rbind(data_Risky[63:124,],data_Good[239:476,])

# Building a random forest model on training data 

install.packages("randomForest")
library(randomForest)

fit.forest <- randomForest(taxable_income~.,data=data_train, na.action=na.roughfix,importance=TRUE)

# Training accuracy 

mean(data_train$taxable_income==predict(fit.forest,data_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,data_train)
library(caret)


# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy = 94.6 % 


# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

