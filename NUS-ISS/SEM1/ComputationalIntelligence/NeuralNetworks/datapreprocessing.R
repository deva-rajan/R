wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")

wine_data[1:11]<-wine_data[1:11]+1
wine_data[1:11]<-log(wine_data[1:11])
wine_data$quality<-as.factor(wine_data$quality)
