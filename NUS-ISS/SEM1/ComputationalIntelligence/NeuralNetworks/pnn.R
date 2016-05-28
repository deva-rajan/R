library(pnn)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
f<-as.formula(paste0("quality~",paste0(names(wine_data)[1:11],collapse="+")))
wine_data$quality<-as.factor(wine_data$quality)
size=nrow(wine_data)
length=ncol(wine_data)
index <- 1:size
positions <- sample(index, trunc(size * 0.75))

training <- wine_data[positions,]
testing <- wine_data[-positions,1:length-1]
result = wine_data[-positions,]
result$actual = result[,length]
result$predict = -1

for(sig in seq(.05,5,by=.01)){
  pnn <- learn(set=training, category.column=12)
  pnn <- smooth(pnn, sigma = sig)
  pnn<-smooth(pnn,sigma=sig)
  
  for(i in 1:nrow(testing))
  {	
    vec <- as.matrix(testing[i,])
    res <- guess(pnn, vec)
    if(is.na(res)){
      #result$predict[i] <- 0
    }
    else{
      result$predict[i] <- res$category
    }
  }
  
  result.size = nrow(result)
  result.correct = nrow(result[result$predict == result$actual,])
  # cat("No of test cases = ",result.size,"\n")
  # cat("Correct predictions = ", result.correct ,"\n")
  cat("Sigma Value",sig," Accuracy: ", result.correct / result.size * 100 ,"\n")
  print("##########################################################################")
}