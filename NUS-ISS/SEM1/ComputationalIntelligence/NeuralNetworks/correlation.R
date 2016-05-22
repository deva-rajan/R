
wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")

cor(wine_data)


wine_data$quality<-as.factor(wine_data$quality)
control <- rfeControl(functions=rfFuncs, method="cv", number=3)

results <- rfe(wine_data[,1:11], wine_data[,12], sizes=c(1:11), rfeControl=control)

