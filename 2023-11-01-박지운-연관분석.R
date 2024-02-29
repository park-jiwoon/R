# 파일로 제출(날짜_이름_연관분석)
# 가장 중요하게 영향을 미치는 것
library(datasets)
library(caret)
library(ggplot2)

# 문제1: 가족관계 및 교육수준의 소득과의 연관성을 확인하시오
# 카이제곱
str(AdultUCI) # 'income','education','relationship'
AdultUCI2<-AdultUCI[,c('income','education')]
AdultUCI3<-AdultUCI[,c('income','relationship')]

AdultUCI3_tb<-table(AdultUCI3)
AdultUCI2_tb<-table(AdultUCI2)
Adult_ch2 <- chisq.test(AdultUCI2_tb)
#  p-value < 2.2e-16 이므로 대립관계가 있다
Adult_ch3 <- chisq.test(AdultUCI3_tb)
#  p-value < 2.2e-16 이므로 대립관계이다다

# 문제2: 주당 일하는 시간과 소득과의 관계를 확인하시오
AdultUCI_income<-AdultUCI[,c('income','hours-per-week')]
AdultUCI_income<-na.omit(AdultUCI_income)
AdultUCI_income

AdultUCI_table<- table(AdultUCI_income)
AdultUCI_chi <- chisq.test(AdultUCI_table)
# 매핑
data <- data.frame(
  income = factor(c("small", "large", "small", "large", "small", "large")),
  hours_per_week = factor(c("part-time", "full-time", "workaholic",
                            "full-time", "part-time", "workaholic")))
AdultUCI_table <- table(data$income, data$hours_per_week)
AdultUCI_chi <- chisq.test(AdultUCI_table)  # 검정
#  p-value = 0.1353
# 관계가 없다는 귀무가설을 채택

# 문제3: 기타 위의 데이터로 부터 자기가 주장하고자 하는 내용을 확인하고 의견을 제시하시오

# 임금과 일하는 시간의 관계는 없지만 가족관계와 교육수준은 유의미한 결과를 내므로
# 학력이 임금에 영향을 크게 미친다고 생각합니다.

