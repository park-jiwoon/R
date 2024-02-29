data2018<-read.csv('data_2018',header = T)
data2019<-read.csv('data_2019',header = T)
data2020<-read.csv('data_2020',header = T)
data<-read.csv('경제성장률.csv',header = T)
oecd<-read.csv('OECD.csv',header = T)
co2<-read.csv('co2.csv',header = T,encoding = 'UTF-8',fileEncoding = 'cp949')
data_co2<-read.csv('co2.csv',header = T)
data_co2
co2
data
oecd
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}    # min-max  정규화
library(ggplot2)
library(party)  #ctree
library(rpart) 
library(randomForest) 
library(caret)
library(dplyr)
library(arules)
library(rpart.plot)
library('rattle')
library(class)
library(gmodels)
# 증가율 = ((새로운 값 - 이전 값) / 이전 값) * 100

data_per<-((data_2019$X2019년.탄소배출량-data_2018$X2018년.탄소배출량)/data_2018$X2018년.탄소배출량)
data_per

data<-rbind(data_2018,data_2019)
data_2020<-data2020[,c('Country','Growth_2020','CO2_2020')]
data_2019<-data2019[,c('국가','X2019년.경제성장률','X2019년.탄소배출량')]
data_2018<-data2018[,c('국가','X2018년.경제성장률','X2018년.탄소배출량')]
data_2020<-data_2020[-27,]   
data_2019<-data_2019[-27,]
data_2018<-data_2018[-27,]

data_2020$CO2_2020<-gsub('\\(kt\\)','',data_2020$CO2_2020)
data_2019$X2019년.탄소배출량<-gsub('\\(kt\\)','',data_2019$X2019년.탄소배출량)
data_2018$X2018년.탄소배출량<-gsub('\\(kt\\)','',data_2018$X2018년.탄소배출량)

data_2020$Growth_2020 <- as.numeric(data_2020$Growth_2020)
data_2020$CO2_2020 <- as.numeric(data_2020$CO2_2020)
data_2019$X2019년.탄소배출량<- as.numeric(data_2019$X2019년.탄소배출량)
data_2019$X2019년.경제성장률<- as.numeric(data_2019$X2019년.경제성장률)
data_2018$X2018년.탄소배출량<- as.numeric(data_2018$X2018년.탄소배출량)
data_2018$X2018년.경제성장률<- as.numeric(data_2018$X2018년.경제성장률)
data_2019_c$dataCO2 <- as.numeric(data_2019_c$dataCO2)

order_indices <- order(data_2018$국가)
sorted_data <- data[order_indices, ]


data_2018 <- order(data_2018$국가)

data_2019



dataCO2<-(data_2019$X2019년.탄소배출량-data_2018$X2018년.탄소배출량)
data_2019_c<-cbind(data_2019,dataCO2)
data_2019_c
###########################################

#data_2020_c$Growth_2020 <- scale(data_2020_c$Growth_2020)
#data_2020_c$dataCO2 <- scale(data_2020_c$dataCO2)
data_2019_c
data_2019_c$X2019년.경제성장률<-scale(data_2019_c$X2019년.경제성장률)
data_2019_c$dataCO2<-scale(data_2019_c$dataCO2)


aov.out=aov(dataCO2~X2019년.경제성장률,data=data_2019_c)
summary(aov.out)  
TukeyHSD(aov.out)
# 0.0948 의미 x
####################################################


cordata<-cor(data_2019_c$X2019년.경제성장률,data_2019_c$dataCO2,)
cordata
percentage <- round(cordata * 100, 2)
output <- paste("상관 계수 (Correlation):", percentage, "%")
print(output)
###

data_2020$Growth_2020 <- scale(data_2020$Growth_2020)
data_2020$CO2_2020 <- scale(data_2020$CO2_2020)


model <- lm(dataCO2 ~ X2019년.경제성장률, data = data_2019_c)
summary(model)

