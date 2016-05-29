
buildpnn<-function(trainingset){
  library(pnn)
  trainingset$quality<-as.factor(trainingset$quality)
  pnn <- pnn::learn(set=trainingset,category.column = 6)
  pnn <- pnn::smooth(pnn, sigma = .2)
  return(pnn)
}

buildgrnn<-function(trainingset){
  library(grnn)
  grnn <- learn(trainingset, variable.column=7)
  grnn <- smooth(grnn, sigma = 1)
  return(grnn)
}

buildnnet<-function(trainingset){
  library(nnet)
  trainingset$quality<-as.factor(trainingset$quality)
  model.nnet<-nnet(quality~.,data=trainingset,linout=FALSE,size=10,maxit=2000,MaxNwts=1500,decay=5e-4)
  return(model.nnet)
}

voteForBestClass<-function(grnn.model,pnn.model,nnet.model,testset){
  votedPrediction<-c()
  for(i in 1:nrow(testset)){
    vec<-as.matrix(testset[i,])
    grnn.op<- if(!is.na(grnn::guess(grnn.model,vec))) round(grnn::guess(grnn.model,vec)) else -1
    pnn.op<- if(!is.na(pnn::guess(pnn.model,vec[,c(7:11)]))) pnn::guess(pnn.model,vec[,c(7:11)])$class else -1
    nnet.op<-predict(nnet.model,vec[,c(1,2,6,7,8,11)],type='class')
    predictions<-table(c(grnn=grnn.op,pnn=pnn.op,nnet=nnet.op))
    highestVotedClass<-max(names(predictions)[predictions == max(predictions)])
    votedPrediction<-c(votedPrediction,highestVotedClass)
  }
  return(votedPrediction)
}

printAccuracy<-function(predicted,test){
  cat("Accuracy of nnet is:",length(test$quality[predicted==test$quality])/length(test$quality)*100)
}

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
f<-as.formula(paste0("quality~",paste0(names(wine_data)[1:11],collapse="+")))
size=nrow(wine_data)
length=ncol(wine_data)
index <- 1:size
set.seed(1)
train_ind <- sample(index,.75*nrow(wine_data))
train<-wine_data[train_ind,]
test<-wine_data[-train_ind,]

# pnn_featureIndex<-c(1:5)
# grnn_featureIndex<-c(6:10)
# nnet_featureIndex<-c(1,2,5,7,9,11)
pnn.model<-buildpnn(train[,c(7:11,12)])
grnn.model<-buildgrnn(train[,c(1,2,3,4,5,6,12)])
nnet.model<-buildnnet(train[,c(1,2,6,7,8,11,12)])
predictions<-voteForBestClass(grnn.model,pnn.model,nnet.model,test[,1:11])
printAccuracy(predictions,test)
cat("Accuracy of nnet is:",length(test$quality[predicted==test$quality])/length(test$quality)*100)
