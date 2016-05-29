
buildpnn<-function(trainingset,featureindexes){
  library(pnn)
  trainingset$quality<-as.factor(trainingset$quality)
  pnn <- pnn::learn(set=trainingset,category.column = 12)
  pnn <- pnn::smooth(pnn, sigma = .2)
  return(pnn)
}

buildgrnn<-function(trainingset,featureindexes){
  library(grnn)
  grnn <- learn(trainingset, variable.column=12)
  grnn <- smooth(grnn, sigma = 1)
  return(grnn)
}

buildnnet<-function(trainingset,featureindexes){
  library(nnet)
  trainingset$quality<-as.factor(trainingset$quality)
  model.nnet<-nnet(quality~.,data=trainingset,linout=FALSE,size=10,maxit=2000,MaxNwts=1500,decay=5e-4)
  return(model.nnet)
}

calculateWeightedAverage<-function(pnn.model,nnet.model,testset){
  pnnWeight=3
  nnetWeight=1
  weightedPrediction<-c()
  for(i in 1:nrow(testset)){
    vec<-as.matrix(testset[i,])
    if(!is.na(pnn::guess(pnn.model,vec))){
    pnn.op<-pnn::guess(pnn.model,vec)
    nnet.op<-predict(nnet.model,vec,type='raw')
    weightedAvg<-c()
    for(val in 1:7){
      weightedAvg<-c(weightedAvg,(pnn.op$probabilities[val]*pnnWeight+nnet.op[1]*nnetWeight)/(pnnWeight+nnetWeight))
    }
    class<-which(weightedAvg==max(weightedAvg))
    weightedPrediction<-c(weightedPrediction,class+2)
    }
    else{
      weightedPrediction<-c(weightedPrediction,-1)
    }
  }
  return(weightedPrediction)
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

pnn.model<-buildpnn(train,c(1:12))
nnet.model<-buildnnet(train,c(1:12))
predictions<-calculateWeightedAverage(pnn.model,nnet.model,test[,1:11])
printAccuracy(predictions,test)
cat("Accuracy of nnet is:",length(test$quality[predicted==test$quality])/length(test$quality)*100)
