library(neural)

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")

wine_data$quality<-as.factor(wine_data$quality)
wine_data<-wine_data[c(12,1:11)]
size=nrow(wine_data)
length=ncol(wine_data)
index <- 1:size
positions <- sample(index, trunc(size * 0.75))

training <- wine_data[positions,]
testing <- wine_data[-positions,1:length-1]
result = wine_data[-positions,]
result$actual = result[,length]

mlp<-mlptrain(as.matrix(training[,2:12]),c(3,3),as.matrix(training$quality),visual=FALSE)
op<-mlp(as.matrix(testing[,1:11]),mlp$weight,mlp$dist,mlp$neurons,c(1,1,1))