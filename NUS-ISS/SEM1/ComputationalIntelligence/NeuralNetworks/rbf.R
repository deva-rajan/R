library(RSNNS)

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
f<-as.formula(paste0("quality~",paste0(names(wine_data)[1:11],collapse="+")))
size=nrow(wine_data)
length=ncol(wine_data)
index <- 1:size
set.seed(1)
train_ind <- sample(index,.75*nrow(wine_data))
train<-wine_data[train_ind,]
test<-wine_data[-train_ind,]

rbf.model<-rbf(as.matrix(train[,1:11]),as.matrix(train$quality),maxit=100,learnFuncParams=c(0.4,1,1.6,2.2,0.8),inputsTest = as.matrix(test[,1:11]),targetsTest = as.matrix(test[,12]))
cat("Accuracy of rbf is:",length(test$quality[predictions==test$quality])/length(test$quality)*100)