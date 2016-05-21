
library(DMwR)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
wine_data<-wine_data[1:11]+1

wine_data<-data.frame(lapply(wine_data,normalize))


outlier.scores <- lofactor(wine_data, k=5)

plot(density(outlier.scores,na.rm=TRUE))