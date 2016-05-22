library(nnet)
wine_data<-read.csv("/home/deva/NUS-ISS/SEM-1/CI1/CA/NN/winequality-white.csv")
f<-as.formula(paste0("quality~",paste0(names(wine_data)[1:11],collapse="+")))
              
wine_data_3<-wine_data[wine_data$quality==3,]
wine_data_4<-wine_data[wine_data$quality==4,]
wine_data_5<-wine_data[wine_data$quality==5,]
wine_data_6<-wine_data[wine_data$quality==6,]
wine_data_7<-wine_data[wine_data$quality==7,]
wine_data_8<-wine_data[wine_data$quality==8,]
wine_data_9<-wine_data[wine_data$quality==9,]

#df containing classes 5,6
wine_data_A<-rbind(wine_data_4,wine_data_5,wine_data_6)
wine_data_A$quality<-as.factor(wine_data_A$quality)
#splitting data as train and test
index_A<-1:nrow(wine_data_A)
set.seed(1)
train_ind_A <- sample(index_A,.75*nrow(wine_data_A))
train_A<-wine_data_A[train_ind_A,]
test_A<-wine_data_A[-train_ind_A,]

# alcohol, volatile.acidity, free.sulfur.dioxide, density, chlorides
model.nnet_A<-nnet(f,data=train_A,linout=FALSE,size=50,maxit=200)

predicted_A<-predict(model.nnet_A,test_A,type='class')

cat("Accuracy of nnet is:",length(test_A$quality[predicted_A==test_A$quality])/length(test_A$quality))
table(test_A$quality,predicted_A)

##############################################################################################
#df containing classes 7,8,3,9
wine_data_B<-rbind(wine_data_7,wine_data_8,wine_data_3,wine_data_9)

wine_data_B$quality<-as.factor(wine_data_B$quality)
#splitting data as train and test
index_B<-1:nrow(wine_data_B)
set.seed(2)
train_ind_B <- sample(index_B,.75*nrow(wine_data_B))
train_B<-wine_data_B[train_ind_B,]
test_B<-wine_data_B[-train_ind_B,]

# alcohol, volatile.acidity, free.sulfur.dioxide, density, chlorides
model.nnet_B<-nnet(f,data=train_B,linout=FALSE,size=50,maxit=200)

predicted_B<-predict(model.nnet_B,test_B,type='class')

cat("Accuracy of nnet is:",length(test_B$quality[predicted_B==test_B$quality])/length(test_B$quality))
table(test_B$quality,predicted_B)

##################################################################################################
# #df containing classes 3,9
# wine_data_C<-rbind(wine_data_3,wine_data_9)
# 
# wine_data_C$quality<-as.factor(wine_data_C$quality)
# #splitting data as train and test
# index_C<-1:nrow(wine_data_C)
# set.seed(3)
# train_ind_C <- sample(index_C,.75*nrow(wine_data_C))
# train_C<-wine_data_C[train_ind_C,]
# test_C<-wine_data_C[-train_ind_C,]
# 
# # alcohol, volatile.acidity, free.sulfur.dioxide, density, chlorides
# model.nnet_C<-nnet(f,data=train_C,linout=FALSE,size=50,maxit=200)
# 
# predicted_C<-predict(model.nnet_C,test_C,type='class')
# 
# cat("Accuracy of nnet is:",length(test_C$quality[predicted==test_C$quality])/length(test_C$quality))
# table(test_C$quality,predicted)
