library(nnet)
wine_data<- read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/output/wine-quality-som.csv")

wine_data_c1<-wine_data[wine_data$cluster=='cluster1',][,1:12]
wine_data_c2<-wine_data[wine_data$cluster=='cluster2',][,1:12]
wine_data_c3<-wine_data[wine_data$cluster=='cluster3',][,1:12]
wine_data_c4<-wine_data[wine_data$cluster=='cluster4',][,1:12]

################################################################################################
#splitting data as train and test
wine_data_c1$quality<-as.factor(wine_data_c1$quality)
index<-1:nrow(wine_data_c1)
set.seed(1)
train_ind_c1 <- sample(index,.75*nrow(wine_data_c1))
train_c1<-wine_data_c1[train_ind_c1,]
test_c1<-wine_data_c1[-train_ind_c1,]

# alcohol, volatile.acidity, free.sulfur.dioxide, density, chlorides
model.nnet_c1<-nnet(quality~.,data=train_c1,linout=FALSE,size=50,maxit=1000)

predicted_c1<-predict(model.nnet_c1,test_c1,type='class')

cat("Accuracy of nnet is:",length(test_c1$quality[predicted_c1==test_c1$quality])/length(test_c1$quality))

####################################################################################################
#splitting data as train and test
wine_data_c2$quality<-as.factor(wine_data_c2$quality)
index<-1:nrow(wine_data_c2)
set.seed(3)
train_ind_c2 <- sample(index,.75*nrow(wine_data_c2))
train_c2<-wine_data_c2[train_ind_c2,]
test_c2<-wine_data_c2[-train_ind_c2,]

# alcohol, volatile.acidity, free.sulfur.dioxide, density, chlorides
model.nnet_c2<-nnet(quality~.,data=train_c2,linout=FALSE,size=50,maxit=1000)

predicted_c2<-predict(model.nnet_c2,test_c2,type='class')

cat("Accuracy of nnet is:",length(test_c2$quality[predicted_c2==test_c2$quality])/length(test_c2$quality))
###############################################################################################################
#splitting data as train and test
wine_data_c3$quality<-as.factor(wine_data_c3$quality)
index<-1:nrow(wine_data_c3)
set.seed(4)
train_ind_c3 <- sample(index,.75*nrow(wine_data_c3))
train_c3<-wine_data_c3[train_ind_c3,]
test_c3<-wine_data_c3[-train_ind_c3,]

# alcohol, volatile.acidity, free.sulfur.dioxide, density, chlorides
model.nnet_c3<-nnet(quality~.,data=train_c3,linout=FALSE,size=50,maxit=1000)

predicted_c3<-predict(model.nnet_c3,test_c3,type='class')

cat("Accuracy of nnet is:",length(test_c3$quality[predicted_c3==test_c3$quality])/length(test_c3$quality))