data_new<-read.csv('증감률.csv',header = T)
# 탄소 증감률
data2017<-data_new[-c(1,2),c(1,4)]
data2018<-data_new[-c(1,2),c(1,7)]
data2019<-data_new[-c(1,2),c(1,10)]

data2017$X2017.2<-as.numeric(data2017$X2017.2)
data2018$X2018.2<-as.numeric(data2018$X2018.2)
data2019$X2019.2<-as.numeric(data2019$X2019.2)

str(data2017)
str(data2018)
str(data2019)

# 경제 성장률
data_eco<-read.csv('growth.csv',header = T)

str(data_eco)
head(data_eco)
head(data2017)
data2017_eco<-data_eco[,1:2]
data2018_eco<-data_eco[,c(1,3)]
data2019_eco<-data_eco[,c(1,4)]
data_2017_a <- merge(data2017, data2017_eco, by = "국가", all.x = T, all.y = T)
data_2017_a<-na.omit(data_2017_a)
data_2018_a <- merge(data2018, data2018_eco, by = "국가", all.x = T, all.y = T)
data_2018_a<-na.omit(data_2018_a)
data_2019_a <- merge(data2019, data2019_eco, by = "국가", all.x = T, all.y = T)
data_2019_a<-na.omit(data_2019_a)

# 연도 별 경제성장률 , 탄소배출량 증가율


data_2019_c$X2019년.경제성장률<-scale(data_2019_c$X2019년.경제성장률)
data_2019_c$dataCO2<-scale(data_2019_c$dataCO2)

data_2017_a
aov.out=aov(X2017.2~X2017,data=data_2017_a)
summary(aov.out)  
TukeyHSD(aov.out)


aov.ou2t=aov(X2018.2~X2018,data=data_2018_a)
summary(aov.ou2t)

aov.out3=aov(X2019.2~X2019,data=data_2019_a)
summary(aov.out3)


model <- lm(X2019.2~X2019, data = data_2019_a)
summary(model)

# 수가 많아서 불가가
# fisher.test(data_2019_a$X2019.2, data_2019_a$X2019, alternative = "two.sided")

# data_ch<-chisq.test(data_2017_a)

# data_2017_a$X2017.2 %*% data_2017_a$X2017

# z 점수 실패
data_2017_b<-(data_2017_a$X2017.2-mean(data_2017_a$X2017.2))/sd(data_2017_a$X2017.2)
data_2017_c<-(data_2017_a$X2017-mean(data_2017_a$X2017))/sd(data_2017_a$X2017)
data_2017_c
data_2017_b
cor(data_2017_b,data_2017_c)

library(dplyr)
result<- data_2017_a %>%
  select("X2017.2", "X2017")
sapply(result,function(x) sum(is.na(x)))
(result<-na.omit(result))

stderr<-function(x) sd(x,na.rm=T)/sqrt(length(na.omit(x)))

CrossTable(data_2018_a$X2018.2,data_2018_a$X2018, expected = T)  #  p =  0.304044 

t.test( data_2018_a$X2018.2,data_2018_a$X2018,alternative = c("two.sided"),
        mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
# p-value = 2.518e-09
var.test(data_2018_a$X2018.2,data_2018_a$X2018)  # p-value = 0.01314

with(data_2018_a,t.test(X2018.2,X2018))
#  p-value = 2.518e-09

with(data_2018_a,var.test(X2018.2,X2018)) 
 
shapiro.test(data_2018_a$X2018.2)  # 정규분포
shapiro.test(data_2018_a$X2018)

# t.test(data_2018_a$X2018.2,data_2018_a$X2018,alternative = c("greater"),
#       paired=T,  # 짝데이터인경우
#       var.equal = T ,  # 등분산인경우
#       conf.level = 0.95)

wilcox.test(data_2018_a$X2018.2,data_2018_a$X2018,    #  p-value = 2.531e-08?
            paired = T,
            var.equal=T,
            conf.level = 0.95)

cor(data_2017_a[2:3],method = 'pearson')  # 0.2912566

str(data_2018_a)
cor.test(data_2018_a$X2018.2,data_2018_a$X2018,method = "kendall",alternative = "greater")

row.names(data_2018_a) <- NULL
rownames(data_2018_a) <- NULL
print(data_2018_a)



############################################################################
data<-read.csv('GDP단위당_CO2배출량_킬로그램.csv',header = T)
data
data<-read.csv('증감률.csv',header = T)

data2<-read.csv('경제성장률.csv',header = T)
data2
# <-read.csv('growth.csv',header = T)


 # 18 19 20
data    # 배출량
data2   # 성장률
data<-data[,-1]

colnames(data)<-c('Country','x18CO2','x19CO2','x20CO2')
colnames(data2)<-c('Country','x18growth','x19growth','x20growth')

data2018<-data[,1:2]
data2019<-data[,c(1,3)]
data2020<-data[,c(1,4)]

data2018eco<-data2[,1:2]
data2019eco<-data2[,c(1,3)]
data2020eco<-data2[,c(1,4)]

data_2018 <- merge(data2018, data2018eco, by = "Country", all.x = T, all.y = T)
data_2019 <- merge(data2019, data2019eco, by = "Country", all.x = T, all.y = T)
data_2020 <- merge(data2020, data2020eco, by = "Country", all.x = T, all.y = T)
str(data_2020)
str(data_2019)
str(data_2018)
data_2020<-na.omit(data_2020)
data_2018<-na.omit(data_2018)
data_2019<-na.omit(data_2019)

data_2020$x20growth<-as.numeric(data_2020$x20growth)
data_2019$x19growth<-as.numeric(data_2019$x19growth)
data_2018$x18growth<-as.numeric(data_2018$x18growth)

data_2020$x20growth<-scale(data_2020$x20growth)
data_2020$x20CO2<-scale(data_2020$x20CO2)
data_2019$x19growth<-scale(data_2019$x19growth)
data_2019$x19CO2<-scale(data_2019$x19CO2)
data_2018$x18growth<-scale(data_2018$x18growth)
data_2018$x18CO2<-scale(data_2018$x18CO2)
###########################################################
CrossTable(data_2020$x20CO2,data_2020$x20growth, expected = T)
CrossTable(data_2019$x19CO2,data_2019$x19growth, expected = T)
CrossTable(data_2018$x18CO2,data_2018$x18growth, expected = T)
#####

cor<-cor.test(data_2018$x18CO2,data_2018$x18growth)
cor2<-cor.test(data_2019$x19CO2,data_2019$x19growth)

va<-var.test(data_2018$x18CO2,data_2018$x18growth)
#

plot(data_2020$x20CO2,data_2020$x20growth)
abline(info,col="red")
info<-lm(data_2020$x20CO2 ~ data_2020$x20growth)
fit<-summary(lm(data_2020$x20CO2~data_2020$x20growth))
plot(data_2018$x18CO2,data_2018$x18growth)
fit<-summary(lm(data_2018$x18CO2,data_2018$x18growth))


