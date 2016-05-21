
wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")

wine_data<-wine_data[wine_data$fixed.acidity %in% boxplot(wine_data$fixed.acidity)$out,]
wine_data<-wine_data[wine_data$volatile.acidity %in% boxplot(wine_data$volatile.acidity)$out,]
wine_data<-wine_data[wine_data$citric.acid %in% boxplot(wine_data$citric.acid)$out,]
wine_data<-wine_data[wine_data$residual.sugar %in% boxplot(wine_data$residual.sugar)$out,]
wine_data<-wine_data[wine_data$chlorides %in% boxplot(wine_data$chlorides)$out,]
wine_data<-wine_data[wine_data$free.sulfur.dioxide %in% boxplot(wine_data$free.sulfur.dioxide)$out,]
wine_data<-wine_data[wine_data$total.sulfur.dioxide %in% boxplot(wine_data$total.sulfur.dioxide)$out,]
wine_data<-wine_data[wine_data$density %in% boxplot(wine_data$density)$out,]
wine_data<-wine_data[wine_data$pH %in% boxplot(wine_data$pH)$out,]
wine_data<-wine_data[wine_data$sulphates %in% boxplot(wine_data$sulphates)$out,]
wine_data<-wine_data[wine_data$alcohol %in% boxplot(wine_data$alcohol)$out,]

