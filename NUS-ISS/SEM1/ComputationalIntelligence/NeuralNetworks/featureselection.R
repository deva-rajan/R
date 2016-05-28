
library(caret)

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
wine_data$quality<-as.factor(wine_data$quality)

control<-rfeControl(functions=rfFuncs,method="cv",number=10)

results <- rfe(wine_data[,1:11], wine_data[,12], sizes=c(1:11), rfeControl=control)

print(results)

predictors(results)

plot(results, type=c("g", "o"))
importance<-varImp(results)

#############################################LVQ#####################################

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(quality~., data=wine_data, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
