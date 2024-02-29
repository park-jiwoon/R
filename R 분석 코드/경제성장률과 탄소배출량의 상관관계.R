data2018<-read.csv('data_2018',header = T)
data2019<-read.csv('data_2019',header = T)
data2020<-read.csv('data_2020',header = T)
str(data2018)
data2019
str(data2020)   # 38x7
# 가설설정 : 경제성장률과 탄소배출량은 관계가 있다

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

str(data_2020)

data_2020<-data2020[,c('Country','Growth_2020','CO2_2020')]
data_2020<-data_2020[-27,]
str(data_2020)  # df
data_2020<-data2020[-27,c('Growth_2020','CO2_2020')]
data_2020$CO2_2020<-gsub('\\(kt\\)','',data_2020$CO2_2020)
data_2020
data_2020$Growth_2020 <- as.numeric(data_2020$Growth_2020)
data_2020$CO2_2020 <- as.numeric(data_2020$CO2_2020)



data_2020$Growth_2020 <- scale(data_2020$Growth_2020)
data_2020$CO2_2020 <- scale(data_2020$CO2_2020)
aov.out=aov(CO2_2020~Growth_2020,data=data_2020)
summary(aov.out)  
TukeyHSD(aov.out)
# F-value가 0.012이며 p-value가 0.914
# 관계가 약하다

######################################################################


data_2020$Growth_2020 <- as.numeric(data_2020$Growth_2020)
data_2020$CO2_2020 <- as.numeric(data_2020$CO2_2020)
data_2020[,] <- lapply(data_2020[,], min_max_norm)

train_size <- round(nrow(data_2020) * 0.7)
result <- sample(1:nrow(data_2020), train_size)

result<- sample(1:nrow(data_2020),nrow(data_2020)*0.7) 
table(result)
train<-data_2020[result,]
test<-data_2020[-result,]
table(train$CO2_2020)
formula<-CO2_2020~Growth_2020
model<-rpart(formula = formula,data = train)
model
str(model)   # df
rpart.plot(model)
(fancyRpartPlot(model))
안됌

##################################
data_2020
# 상관
data_2020$Growth_2020 <- as.numeric(data_2020$Growth_2020)
data_2020$CO2_2020 <- as.numeric(data_2020$CO2_2020)
data_2020$Growth_2020 <- scale(data_2020$Growth_2020)
data_2020$CO2_2020 <- scale(data_2020$CO2_2020)
cordata<-cor(data_2020$CO2_2020,data_2020$Growth_2020)
cordata
percentage <- round(cordata * 100, 2)
output <- paste("상관 계수 (Correlation):", percentage, "%")
print(output)
# 관계성이 약하다?

##################################################
# 회귀
data_2020$Growth_2020 <- scale(data_2020$Growth_2020)
data_2020$CO2_2020 <- scale(data_2020$CO2_2020)


model <- lm(CO2_2020 ~ Growth_2020, data = data_2020)
summary(model)
# p-value: 0.9137


# 모델 선택의 기분 : accuracy, 안정성(더 중요)

