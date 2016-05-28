library(neuralnet)

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")

wine_data[1:11]<-wine_data[1:11]+1
#wine_data$quality<-as.factor(wine_data$factor)

f<-as.formula(paste0("quality~",paste0(names(wine_data)[1:11],collapse="+")))

#neuralnet(f,data=wine_data,hidden=c(3,2),threshold=0.01,stepmax=1e+05,rep=1,algorithm='rprop+',learningrate=.1,err.fct='sse',act.fct='logistic',linear.output = TRUE)

#splitting data as train and test
index<-1:nrow(wine_data)
set.seed(1)
train_ind <- sample(index,.75*nrow(wine_data))
train<-wine_data[train_ind,]
test<-wine_data[-train_ind,]

model.neuralnet<-neuralnet(f,data=train, hidden=c(5,5,5), err.fct="sse",linear.output=TRUE,stepmax = 1e+05,threshold=.1,act.fct = 'logistic')

predicted<-round(compute(model.neuralnet,test[,1:11])$net.result)

cat("Accuracy of nnet is:",length(test$quality[predicted==test$quality])/length(test$quality))

