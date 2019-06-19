BFT<-read.csv("C:/Users/AKHIL/Desktop/New folder/bodyfat.csv")
View(BFT)
cor(BFT)

set.seed(121)
BFT_Sam<-sample(2,nrow(BFT),replace=TRUE,prob=c(.85,.15))
BFT_Sam
BFT_Train<-BFT[BFT_Sam==1,]
BFT_Test<-BFT[BFT_Sam==2,]
BFT_Train;BFT_Test

md_lm<-lm(Bodyfat~.,data=BFT_Train)
pre_lm<-predict(md_lm,BFT_Test)
summary(md_lm)
library(dplyr)

pre_lm<-data.frame(pre_lm,BFT_Test$Bodyfat)
pre_lm100<-mutate(pre_lm,error_1=pre_lm-BFT_Test$Bodyfat,error_1sq=error_1^2)
View(pre_lm100)
mean<-sum(pre_lm100$error_1sq)/nrow(pre_lm100)    #####mean=28.86786
mean                                           ###RMSE=5.372882
RMSE<-sqrt(sum(pre_lm100$error_1sq)/nrow(pre_lm100))
RMSE



md_lm1<-lm(Bodyfat~Wrist+Abdomen+Age,data=BFT_Train)
pre_lm2<-predict(md_lm1,BFT_Test)
pre_lm2<-data.frame(pre_lm2,BFT_Test$Bodyfat)
pre_lm2
summary(md_lm1)



pre_lm2<-data.frame(pre_lm2,BFT_Test$Bodyfat)
pre_lm2<-mutate(pre_lm2,error_1=pre_lm2-BFT_Test$Bodyfat,error_1sq=error_1^2)
View(pre_lm2)
mean2<-sum(pre_lm2$error_1sq)/nrow(pre_lm2)                  ##mean=28.902
mean2                                                       ###RMSE=5.376      
RMSE2<-sqrt(sum(pre_lm2$error_1sq)/nrow(pre_lm2))
RMSE2
library(dplyr)



md_lm2<-lm(Bodyfat~Abdomen+Wrist,data=BFT_Train)
pre_lm3<-predict(md_lm2,BFT_Test)
pre_lm3<-data.frame(pre_lm3,BFT_Test$Bodyfat)
pre_lm3
summary(md_lm2)
                                                        ##mean=27.854  
                                                        ##RMSE=5.2777 
pre_lm3<-data.frame(pre_lm3,BFT_Test$Bodyfat)
pre_lm3<-mutate(pre_lm3,error_1=pre_lm3-BFT_Test$Bodyfat,error_1sq=error_1^2)
mean3<-sum(pre_lm3$error_1sq)/nrow(pre_lm3)
mean3
RMSE3<-sqrt(sum(pre_lm3$error_1sq)/nrow(pre_lm3))
RMSE3
library(dplyr)





md_lm3<-lm(Bodyfat~Abdomen+Height+Weight,data=BFT_Train)
pre_lm4<-predict(md_lm3,BFT_Test)
pre_lm4<-data.frame(pre_lm4,BFT_Test$Bodyfat)
pre_lm4                                                ##mean=24.872
summary(md_lm3)                                         ##RMSE=4.9872  


pre_lm4<-data.frame(pre_lm4,BFT_Test$Bodyfat)
pre_lm4<-mutate(pre_lm4,error_1=pre_lm4-BFT_Test$Bodyfat,error_1sq=error_1^2)
mean4<-sum(pre_lm4$error_1sq)/nrow(pre_lm4)
mean4
RMSE4<-sqrt(sum(pre_lm4$error_1sq)/nrow(pre_lm4))
RMSE4

