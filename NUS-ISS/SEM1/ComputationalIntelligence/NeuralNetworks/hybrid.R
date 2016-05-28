library(nnet)
library(grnn)
library(pnn)

buildpnn(){
  pnn <- learn(set=training, category.column=12)
  pnn <- smooth(pnn, sigma = sig)
  pnn<-smooth(pnn,sigma=sig)
}

buildgrnn(){
  
}

buildnnet(){
  
}

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
f<-as.formula(paste0("quality~",paste0(names(wine_data)[1:11],collapse="+")))
size=nrow(wine_data)
length=ncol(wine_data)
index <- 1:size



