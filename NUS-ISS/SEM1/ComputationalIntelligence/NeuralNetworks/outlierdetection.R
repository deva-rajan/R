
library(DMwR)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
wine_data_new<-wine_data[,1:11]

wine_data_new<-data.frame(lapply(wine_data_new,normalize))
wine_data_new$quality<-wine_data$quality

outlier.scores <- lofactor(wine_data, k=5)

plot(density(outlier.scores,na.rm=TRUE))