
pre2<-read.csv('energe.csv',header = T,encoding = 'UTF-8',fileEncoding = 'cp949')
pre2

evo<-read.csv('2021년 대한민국 행정구역별 발전원별 발전량2.csv',header = T,encoding = 'UTF-8',fileEncoding = 'cp949')
evo

cost<-read.csv('cost.csv',header = T,encoding = 'UTF-8',fileEncoding = 'cp949')
cost

library(dplyr)

df<-data.frame(fire=fire,nuclear=nuclear)
df

elec<-read.csv('electricity.csv')


str(elec)

colnames(elec)<- c("country", "code", "Year" ,"Other.renewables" , "Electricity.from.bioenergy" ,
                       "Electricity.from.solar", "Electricity.from.wind","Electricity.from.hydro", 
                       "Electricity.from.nuclear", "Electricity.from.oil" , "Electricity.from.gas","Electricity.from.coal")

elec<-subset(elec,Year==2022,select=c("country",'code',"Other.renewables","Electricity.from.nuclear","Electricity.from.oil","Electricity.from.coal"))

elec<-na.omit(elec)

elec<-subset(elec,code!='',select=c("country","Other.renewables","Electricity.from.nuclear","Electricity.from.oil","Electricity.from.coal"))

elec
elec_a<-subset(elec,Electricity.from.coal>=50,select=c("country","Other.renewables","Electricity.from.nuclear","Electricity.from.oil","Electricity.from.coal"))

colnames(elec_a)<-c('국가','천연가스','원전','석유','석탄')

str(elec_a)
elec_a$Other.renewables<-scale(elec_a$Other.renewables)
elec_a$Electricity.from.nuclear<-scale(elec_a$Electricity.from.nuclear)
elec_a$Electricity.from.oil<-scale(elec_a$Electricity.from.oil)
elec_a$Electricity.from.coal<-scale(elec_a$Electricity.from.coal)
elec_a

library(ggplot2)
str(elec)



elec_a$Ratio1 <- elec_a$천연가스 / rowSums(elec_a[, c('천연가스','원전','석유','석탄')])
elec_a$Ratio2 <- elec_a$원전 / rowSums(elec_a[, c('천연가스','원전','석유','석탄')])
elec_a$Ratio3 <- elec_a$석유 / rowSums(elec_a[, c('천연가스','원전','석유','석탄')])
elec_a$Ratio4 <- elec_a$석탄 / rowSums(elec_a[, c('천연가스','원전','석유','석탄')])


ggplot(elec_a, aes(x = elec_a$국가)) +
  geom_bar(aes(y = Ratio1, fill = "천연가스"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Ratio2, fill = "원전"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Ratio3, fill = "석유"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Ratio4, fill = "석탄"), stat = "identity", position = "dodge") +
  labs(x = "국가", y = "비율", title = "에너지 발전량") +
  scale_fill_manual(values = c("천연가스" = "red", "원전" = "blue", "석유" = "green", "석탄" = "orange")) +
  guides(fill = guide_legend(title = "열 이름"))




electric<- read.csv('electric.csv', fileEncoding = 'EUC-KR')
str(electric)
library(tidyr)

electric <- data.frame(electric)
colnames(electric)<- c("country", "code", "Year" ,"Other.renewables" , "bioenergy" ,
                       "solar", "wind","hydro", 
                       "nuclear", "oil" , "gas","coal")

str(electric)
electric_2022 <- subset(electric, Year == 2022)

str(electric_2022)

electric_2022_nocode <- subset(electric_2022, code != "")
str(electric_2022_nocode)
electric_2022_nocode2 <- subset(electric_2022_nocode, select = -c(Other.renewables, Year, bioenergy, hydro, oil, code))


library(tidyr)
library(dplyr)
library(ggplot2)

# 각 나라별로 총 에너지 소비를 계산합니다.
selected_countries <- electric_2022_nocode2 %>%
  filter(country %in% c("South Korea", "Japan", "United Kingdom", "Ireland", "Australia")) %>%
  mutate(total = solar + wind + nuclear + gas + coal)

# 데이터를 긴 형식으로 변환합니다.
selected_countries_long <- selected_countries %>%
  pivot_longer(cols = solar:coal, names_to = "energy_source", values_to = "value") %>%
  mutate(percentage = value / total * 100)

# ggplot을 사용하여 정규화된 스택 바 차트를 생성합니다.
ggplot(selected_countries_long, aes(x = country, y = percentage, fill = energy_source)) +
  geom_bar(stat = "identity", position = "fill") + # position fill을 사용하여 정규화된 바를 생성합니다.
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Energy Consumption (%)", x = "Country", fill = "Energy Source", title = "2022년 선진국 에너지 비중") +
  scale_fill_manual(values = c("gas" = "brown","coal" = "red", "solar" = "yellow", "wind" = "#4169E1", "nuclear" = "#388E3C")) +
  guides(fill = guide_legend(title = "에너지"))  
 

 


 #############################
electric<- read.csv('electric.csv', fileEncoding = 'EUC-KR')
str(electric)
library(tidyr)

electric <- data.frame(electric)
colnames(electric)<- c("country", "code", "Year" ,"Other.renewables" , "bioenergy" ,
                       "solar", "wind","hydro", 
                       "nuclear", "oil" , "gas","coal")

str(electric)
electric_2022 <- subset(electric, Year == 2022)

str(electric_2022)

electric_2022_nocode <- subset(electric_2022, code != "")
str(electric_2022_nocode)
electric_2022_nocode2 <- subset(electric_2022_nocode, select = -c(Other.renewables, Year, bioenergy, hydro, oil, code))


library(tidyr)
library(dplyr)
library(ggplot2)

# 각 나라별로 총 에너지 소비를 계산합니다.
selected_countries <- electric_2022_nocode2 %>%
  filter(country %in% c("Indonesia", "Vietnam", "Malaysia", "Philippines", "Taiwan")) %>%
  mutate(total = solar + wind + nuclear + gas + coal)

# 데이터를 긴 형식으로 변환합니다.
selected_countries_long <- selected_countries %>%
  pivot_longer(cols = solar:coal, names_to = "energy_source", values_to = "value") %>%
  mutate(percentage = value / total * 100)



# ggplot을 사용하여 정규화된 스택 바 차트를 생성합니다.
ggplot(selected_countries_long, aes(x = country, y = percentage, fill = energy_source)) +
  geom_bar(stat = "identity", position = "fill") + # position fill을 사용하여 정규화된 바를 생성합니다.
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(y = "Energy Consumption (%)", x = " ", fill = "Energy Source", title = " ") +
  scale_fill_manual(values = c("gas" = "#bdbdbd","coal" = "black", "solar" = "#f44338", "wind" = "#91d4fa", "nuclear" = "#ffee80")) +
  guides(fill = guide_legend(title = "에너지"))






