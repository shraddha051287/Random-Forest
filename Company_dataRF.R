# Using Random Forest
install.packages("randomForest")
library(randomForest)

Company_data = read.csv(file.choose())
Company_data

#####dividing the data in training and testing
#########splitting the data based on Shelveloc 

Company_data_Bad<-Company_data[Company_data$ShelveLoc=="Bad",] #96
Company_data_Good<-Company_data[Company_data$ShelveLoc=="Good",] #85
Company_data_Medium<-Company_data[Company_data$ShelveLoc=="Medium",] #219 

Company_data_train <- rbind(Company_data_Bad[1:46,],Company_data_Good[1:43,],Company_data_Medium[1:110,])
Company_data_test <- rbind(Company_data_Bad[51:96,],Company_data_Good[31:50,],Company_data_Medium[31:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(ShelveLoc~.,data=Company_data_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(Company_data_train$ShelveLoc==predict(fit.forest,Company_data_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,Company_data_train)
library(caret)


# Confusion Matrix
confusionMatrix(Company_data_train$ShelveLoc, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=Company_data_test)
mean(pred_test==Company_data_test$ShelveLoc) # Accuracy = 51.16 % 


# Confusion Marix 

confusionMatrix(Company_data_test$ShelveLoc, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

