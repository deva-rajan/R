library(nnet)
wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")

wine_data[1:11]<-wine_data[1:11]+1
wine_data$quality<-as.factor(wine_data$quality)

f<-as.formula(paste0("quality~",paste0(names(wine_data)[1:11],collapse="+")))

#splitting data as train and test
index<-1:nrow(wine_data)
set.seed(1)
train_ind <- sample(index,.75*nrow(wine_data))
train<-wine_data[train_ind,]
test<-wine_data[-train_ind,]

model.nnet<-nnet(quality~.,data=train,linout=FALSE,size=50,maxit=1000)

predicted<-predict(model.nnet,test,type='class')

cat("Accuracy of nnet is:",length(test$quality[predicted==test$quality])/length(test$quality))
