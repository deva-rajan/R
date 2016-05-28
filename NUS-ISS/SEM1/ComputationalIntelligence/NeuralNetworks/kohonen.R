library(kohonen)

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")

wine_data.sc<-scale(wine_data[,1:11])

#splitting data as train and test
index<-1:nrow(wine_data)
set.seed(1)
train_ind <- sample(index,.75*nrow(wine_data))
train<-wine_data[train_ind,]
test<-wine_data[-train_ind,]

###############################UNSUPERVISED-SOM##################################
set.seed(7)
wine.som<-som(data=wine_data.sc[,1:11],grid= somgrid(2, 2, "hexagonal"))
plot(wine.som, main = "Wine data")

#############################SUPERVISED-XYF######################################
set.seed(8)
wine.xyf<-xyf(data = as.matrix(train[,1:11]),Y = classvec2classmat(train[,12]),xweight=0.5,grid=somgrid(3,3,"hexagonal"))
predicted.op<-predict(wine.xyf,newdata=as.matrix(test[,1:11]))$prediction
table(test$quality,predicted.op)

cat("Accuracy of xyf is:",length(test$quality[predicted.op==test$quality])/length(test$quality))
