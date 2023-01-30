library(dplyr)
library(tidyr)
library(MASS)
library(caret)
library(tidyverse)
library(tidymodels)
library(skimr)
library(gridExtra)
library(psych)
library(car)
library(ggplot2)
library(PerformanceAnalytics)
library(GGally)
library(fmsb)
library(reshape2)

#################### 데이터 불러오기, 정제

gg_data <- read.csv('C:/Users/user/Desktop/경기데이터.csv', fileEncoding = 'euc-kr')
colSums(is.na(gg_data)) # 컬럼별 na 갯수

str(gg_data)
summary(gg_data)

# 심_응
count(gg_data[gg_data$심_응 > 0, ]) # 122 : 공공심야약국 39, 응급실 95
sum(gg_data$심_응) # 134

# 컬럼 이름 바꾸기
names(gg_data)[8] <- "안전상비"
names(gg_data)[9] <- "버스정류소"
names(gg_data)[12] <- "유아인구수"
names(gg_data)[13] <- "총인구수"

# 총인구수가 na인 것 빼기
gg_data <- gg_data[!is.na(gg_data$총인구수),]
colSums(is.na(gg_data))

# 유아인구수가 na인 것에 0 대입
gg_data$유아인구수[is.na(gg_data$유아인구수)] <- 0
colSums(is.na(gg_data))


###### 응_심 : 격자별 응급실+공공심야약국 -> 0:없다, 1:있다 바꾸는 과정

gg_data[gg_data$심_응> 0, ]$심_응 <- 1
sum(gg_data$심_응) # 121 : 총인구수가 na인 것 중에 심_응이 0보다 큰 격자가 1개 있다

sum(gg_data$심_응 == 1) # 121
sum(gg_data$심_응 == 0) # 8512

str(gg_data)
summary(gg_data)


#################### 데이터 시각화

# 종속변수 시각화
g.y <- ggplot(gg_data, aes(x=as.factor(심_응))) + geom_bar() +
  xlab("경기도 응급실과 심야약국의 여부")

g.y


#################### 데이터 내보내기

write.csv(gg_data, file = 'C:/Users/user/Desktop/경기데이터_정제.csv', fileEncoding = 'euc-kr')

