nagdp<-read.csv('national-gdp.csv',header = T)
str(nagdp)
nagdp<-subset(nagdp,Year>=1990,select = c('Entity','Year','GDP..constant.2015.US..'))
nagdp
colnames(nagdp)<-c('Country','Year','GDP')
str(nagdp)

co2<-read.csv('annual-co2-emissions-per-country (1).csv',header = T)

str(co2)
co2<-subset(co2,Year>=1990,select =c('Entity','Year','Annual.CO..emissions') )

colnames(co2)<-c('Country','Year','CO2')


data <- merge(nagdp, co2, by = c("Country", "Year"))

################################


library(dplyr)
library(ggplot2)

data_country <- c('South Korea', 'South Africa', 'Belgium', 'Mexico', 'Japan','Sweden','	
Austria','Denmark','Bulgaria')

filtered_data <- data %>%
  filter(Country %in% data_country)

filtered_data<-na.omit(filtered_data)
is.na(filtered_data)
is.null(filtered_data)

# 로그 스케일 적용
filtered_data <- filtered_data %>%
  mutate(CO2_log = log10(CO2), GDP_log = log10(GDP))

ggplot(data = filtered_data, aes(x = Year)) +
  geom_line(aes(y = GDP_log, color = "GDP"), na.rm = TRUE) +
  geom_line(aes(y = CO2_log, color = "CO2"), na.rm = TRUE) +
  labs(title = "GDP and CO2 Growth Over the Years",
       x = "Year",
       y = "Growth") +
  scale_color_manual(values = c("GDP" = "blue", "CO2" = "red")) +
  theme_minimal() +
  facet_wrap(~Country, ncol = 2) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_continuous(limits = c(1990, 2021))
#################

# Min-Max 스케일링 함수 정의
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 데이터 필터링
filtered_data <- data %>%
  filter(Country %in% data_country)

# GDP와 CO2 열에 Min-Max 스케일링 적용
filtered_data <- filtered_data %>%
  mutate(
    GDP_scaled = min_max_scaling(GDP),
    CO2_scaled = min_max_scaling(CO2)
  )

# 그래프 생성
ggplot(data = filtered_data, aes(x = Year)) +
  geom_line(aes(y = GDP_scaled, color = "GDP")) +
  geom_line(aes(y = CO2_scaled, color = "CO2")) +
  labs(title = "Scaled GDP and CO2 Growth Over the Years",
       x = "Year",
       y = "Scaled Growth") +
  scale_color_manual(values = c("GDP" = "blue", "CO2" = "red")) +
  theme_minimal() +
  facet_wrap(~Country, ncol = 2) +
  scale_y_continuous(limits = c(0, 1)) +  # Y축 스케일 설정
  scale_x_continuous(limits = c(1990, 2021))  # X축 스케일 설정




emession<-read.csv('co2-emissions-and-gdp.csv',header = T)
emession
str(emession)

# GDP..PPP..constant.2017.international...: 2017년 국제 달러로 표시된 PPP (구매력 평가)에 따른 일정 연도의 GDP입니다. 일부 값이 NA(결측치)로 표시되어 있습니다.
#Annual.CO..emissions: 해당 국가 또는 지역의 연간 이산화탄소 (CO2) 배출량을 나타냅니다.
#Annual.consumption.based.CO..emissions: 연간 소비에 기반한 CO2 배출량입니다. 일부 값이 NA로 표시되어 있습니다.


ggplot(data = filtered_data, aes(x = Year)) +
  geom_line(aes(y = GDP_log, color = "GDP"), na.rm = TRUE) +
  geom_line(aes(y = CO2_log, color = "CO2"), na.rm = TRUE) +
  labs(title = "Scaled GDP and CO2 Growth Over the Years",
       x = "Year",
       y = "Scaled Growth") +
  scale_color_manual(values = c("GDP" = "blue", "CO2" = "red")) +
  theme_minimal() +
  facet_wrap(~Country, ncol = 2) +
  scale_x_continuous(limits = c(1990, 2021))  # X축 스케일 설정