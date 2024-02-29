longdata<-read.csv('long_data.csv',header = T)
con<-dbConnect(MySQL(), user="dba", password="qwer1234", dbname="homework", host="34.64.89.3", port=3306)
dbListTables(con) 
longda<-data.table(longdata)
dbWriteTable(con,"longda",longda,overwrite=TRUE)

dbWriteTable(con,"df",df,overwrite=TRUE)
(longda=dbReadTable(con,"Defaultdata"))
longda$growth<-as.numeric(longda$growth)
longda$co2<-as.numeric(longda$co2)

dbDisconnect(con)
library(randomForest)
library(rpart)
library(data.table)
library(RMySQL) 
library(DBI)
library(dbplyr)
library(ggplot2)
library(party)  #ctree
library(caret)
library(dplyr)
library(arules)
library(rpart.plot)
library('rattle')
library(class)
library(gmodels)


data<-read.csv('증감률.csv',header = T)
data
colnames(data)<-c('Country','x18CO2','x19CO2','x20CO2')


data2<-read.csv('경제성장률.csv',header = T)
data2 # 18 19 20

colnames(data2)<-c('Country','x18growth','x19growth','x20growth')



data2018<-read.csv('data_2018',header = T)
colnames(data2018)<-
data2019<-read.csv('data_2019',header = T)
data2020<-read.csv('data_2020',header = T)


data_2020<-data2020[-27,c('Growth_2020','CO2_2020')]
data_2020$CO2_2020<-gsub('\\(kt\\)','',data_2020$CO2_2020)
data_2020
data_2020$Growth_2020 <- as.numeric(data_2020$Growth_2020)
data_2020$CO2_2020 <- as.numeric(data_2020$CO2_2020)

#########
longda
cor<-cor.test(longdf$co2,longdf$growth)
cor    # p-value = 0.02036

numeric_data <- longda[, sapply(longda, is.numeric)]
corrplot(cor(numeric_data), method = "color")


percentage <- round(cordata * 100, 2)
output <- paste("상관 계수 (Correlation):", percentage, "%")
print(output)


var<-var.test(longda$co2,longda$growth)
var
plot(longda$co2,longda$growth)
info<-lm(longda$co2~longda$growth,longda)
abline(info,col="red")

with(longda,t.test(co2,growth))    


    aov.out=aov(co2~growth,data=longdf)   
    summary(aov.out)    
    plot(aov.out)       


longda<-na.omit(longda)
longda$co2 <- as.factor(longda$co2)
longda$growth <- as.factor(longda$growth)
levels(longda$co2)
levels(longda$growth)
residuals

str(longda$growth)
cut(longda$growth,breaks=4)
cut(longda$co2,breaks = 4)
confusionMatrix(longda$co2,longda$growth)

model2<-lm(co2~growth,data=longda)
plot(model2)
train_size <- round(nrow(longda) * 0.7)
result <- sample(1:nrow(longda), train_size)

result<- sample(1:nrow(longda),nrow(longda)*0.7) 
table(result)
train<-longda[result,]
test<-longda[-result,]
table(longda$co2)
formula<-co2~growth
model<-rpart(formula = formula,data = train)
model
str(model)   # df
rpart.plot(model)

predictions <- predict(model, newdata = test)
predictions  # 예측
mse <- mean((test$co2 - predictions)^2)
mse   # 오차율


choice<- sample(1:nrow(longda),10000,replace = TRUE)
choice
adult.df<-longda[choice, ]
adult.test<-longda[-choice, ]
formula=co2~growth
long_ctree<-ctree(formula,data=longda)     # party 패키지에 ,rpart
long_ctree
plot(long_ctree)

#정확도 = (TP + TN) / (전체 샘플 수)


common_levels <- union(levels(longdf$co2), levels(longdf$growth))
train <- factor(longdf$growth, levels = common_levels)
test <- factor(longdf$growth, levels = common_levels)


importance(model2)

confusionMatrix(train,test)
str(longtb)

# IQR 계산
Q1 <- quantile(longda$co2, 0.25)
Q3 <- quantile(longda$co2, 0.75)
IQR <- Q3 - Q1
q1 <- quantile(longda$growth, 0.25)
q3 <- quantile(longda$growth, 0.75)
iqr <- q3-q1
# 이상치 식별 (Q1 - 1.5 * IQR 미만 또는 Q3 + 1.5 * IQR 초과인 값)
outliers <- longda[longda$co2 < (Q1 - 1.5 * IQR) | longda$co2 > (Q3 + 1.5 * IQR), ]
outliers2 <- longda[longda$growth < (q1 - 1.5 * iqr) | longda$growth > (q3 + 1.5 * iqr), ]
# 이상치가 제거된 데이터프레임 생성
cleaned_longda <- longda[!(longda$co2 < (Q1 - 1.5 * IQR) | longda$co2 > (Q3 + 1.5 * IQR)), ]
cleaned_longda <- longda[!(longda$growth < (q1 - 1.5 * iqr) | longda$growth > (q3 + 1.5 * iqr)), ]
# 이상치 식별 결과 출력

print(outliers)

# 이상치가 제거된 데이터프레임 출력

print(cleaned_longda)

# IQR 계산
Q1 <- quantile(cleaned_longda$co2, 0.25)
Q3 <- quantile(cleaned_longda$co2, 0.75)
IQR <- Q3 - Q1
# 이상치 식별 (Q1 - 1.5 * IQR 미만 또는 Q3 + 1.5 * IQR 초과인 값)
outliers <- cleaned_longda[cleaned_longda$co2 < (Q1 - 1.5 * IQR) | cleaned_longda$co2 > (Q3 + 1.5 * IQR), ]

# 이상치가 제거된 데이터프레임 생성
cleaned_data_df <- cleaned_longda[!(cleaned_longda$co2 < (Q1 - 1.5 * IQR) | cleaned_longda$co2 > (Q3 + 1.5 * IQR)), ]
longdf<-cleaned_data_df
longdf

lm(co2~growth,longdf)
cor<-cor.test(longdf$growth,longdf$co2,method = 'pearson')


idx=sample(1:dim(longdf)[1],40)
dfSample=longdf[idx,]
country<-dfSample$Country
dfSample$Country=NULL     # 열삭제(범주형)
hc=hclust(dist(dfSample),method='ave') 
plot(hc,hang=-1 ,labels = longdf$Country[idx])
rect.hclust(hc,k=10)
(groups=cutree(hc,k=3))  # 40개의 데이터에 대한 그룹 번호 출력
table(groups,country)


set.seed(123)
data <- data.frame(
  GDP_growth = rnorm(100, mean = 3, sd = 1),
  Carbon_emission_growth = rnorm(100, mean = 2, sd = 1)
)
# 의사 결정 트리 모델 생성
model <- rpart(co2 ~ growth, data = longdf)
# 트리 모델 시각화
plot(model)
text(model)


model = randomForest(co2 ~ growth, data = longdf)  # 트리가 500, 2개의 변수


model= randomForest(co2 ~ ., data = longdf, ntree=300 , mtry=2, importance=T)
# Mean of squared residuals: 0.00932888     # 낮을 수록 잔차가 적고, 모델을 잘 설명한다
# % Var explained: -40.34                   # -이므로 예측 성능이 낮다

plot(model)
varImpPlot(model)
pred2<- predict(mode12,wdbc_test,type='class')
(res<-table(pred2,wdbc_test$wdbc.diagnosis))

longti<-as.tibble(longdf)
(result<-sapply(longti,class))
longti<-na.omit(longti)
dim(longti)
set.seed(123)
# 3. sampling (7:3) 하시오
result<-sample(1:nrow(longti),nrow(longti)*0.7)
train<-longti[result,]
test<-longti[-result,]
dim(train)
dim(test)
ran=randomForest(co2~.,data = train,ntree=200, mtry=2, importance=TRUE,na.action =na.omit)
ran
ran1<-predict(ran,newdata=test)
head(ran1)
table(ran1,test$co2)

confusionMatrix(longdf)
longdf<-longdf[,-1]
longdf<-longdf[,-2]


ntree<- c(200,300,400,500,600)
mtry<-c(6:10)
param<-data.frame(n=ntree,m=mtry)   # 3*3 9가지 경우의 수가 발생
param


    # 두 변수 간의 Pearson 상관 계수 및 p-value값 계산
    cor<-cor.test(longdf$growth,longdf$co2,method = 'pearson')
    p_value <- cor$p.value
    p_value = 1.0971e-06
    # 피어슨 상관분석에서 p-value 값이 0에 매우 가까운
    # 결과를 얻었고, 이는 두 변수 간에 매우 강력한 
    # 선형 상관 관계가 있어 실제로 매우 관련성이 있음을 나타내며,
    # 이 관계는 통계적으로 매우 유의미하다는 것을 의미합니다.

