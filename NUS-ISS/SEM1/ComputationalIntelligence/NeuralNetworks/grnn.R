library(grnn)

data = read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv", header=TRUE)

size=nrow(data)

length=ncol(data)

index <- 1:size

positions <- sample(index, trunc(size * 0.75))

training <- data[positions,]
testing <- data[-positions,1:length-1]
result = data[-positions,]
result$actual = result[,length]
result$predict = -1

for(sig in seq(.1,5,by=.1)){
nn <- learn(training, variable.column=length)
nn <- smooth(nn, sigma = sig)


for(i in 1:nrow(testing))
{	
  vec <- as.matrix(testing[i,])
  res <- guess(nn, vec)
  
  if(is.nan(res))
  {
   # cat("Entry ",i," Generated NaN result!\n")
  }
  else
  {
    result$predict[i] <- res
  }
}
result.size = nrow(result)
result.correct = nrow(result[round(result$predict) == result$actual,])
# cat("No of test cases = ",result.size,"\n")
# cat("Correct predictions = ", result.correct ,"\n")
cat("Sigma:",sig," Accuracy = ", result.correct / result.size * 100 ,"\n")
print("##########################################################################")
}