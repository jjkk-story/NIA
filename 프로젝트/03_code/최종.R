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

data <- read.csv('C:/Users/user/Desktop/서울/서울데이터.csv', fileEncoding = 'euc-kr')
colSums(is.na(data)) # 컬럼별 na 갯수

str(data)
summary(data)

# 심_응
count(data[data$심_응 > 0, ]) # 92 : 공공심야약국 35, 응급실 67
sum(data$심_응) # 102

# 컬럼 이름 바꾸기
names(data)[12] <- "유아인구수"
names(data)[13] <- "총인구수"

# 총인구수가 na인 것 빼기
data <- data[!is.na(data$총인구수),]
colSums(is.na(data))

# 유아인구수가 na인 것에 0 대입
data$유아인구수[is.na(data$유아인구수)] <- 0
colSums(is.na(data))


###### 응_심 : 격자별 응급실+공공심야약국 -> 0:없다, 1:있다 바꾸는 과정

data[data$심_응> 0, ]$심_응 <- 1
sum(data$심_응) # 91 : 총인구수가 na인 것 중에 심_응이 0보다 큰 격자가 1개 있다

sum(data$심_응 == 1) # 91
sum(data$심_응 == 0) # 513

str(data)
summary(data)


#################### 모두 0이라고 예측했을 때( test에 0:159개, 1:23개 )

set.seed(3)

train <- sample(1: nrow(data), size = 0.7*nrow(data), replace=F)

data_str <- data[train, ]
data_ste <- data[-train, ]

mis.rates <- function(x, y) sum(x!=y)/length(x)

library(MLmetrics)
library(ROCR)

all0_pred <- rep(0, dim(data_ste)[1])
all0_pred <- factor(all0_pred, levels = c(0,1))

score = function(y_test_data,pred){
  confusion_matrix = table(y_test_data,pred)
  #cat(confusion_matrix,"\n","\n")
  
  accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
  Precision = confusion_matrix[2,2]/sum(confusion_matrix[,2])
  Recall = confusion_matrix[2,2]/sum(confusion_matrix[2,])
  f1score = 2*(Precision*Recall)/(Precision+Recall)
  cat("accuracy: ",accuracy,"\nPrecision: ",Precision,"\nRecall: ",Recall,"\n")
  cat("F1-score: ",f1score,"\n")
  
  error_rate = 1 - accuracy
  cat("error_rate:",error_rate,"\n")
}

score(data_ste$심_응, all0_pred)


#################### 로지스틱 회귀

reg <- glm(심_응 ~ 약국 + 소방서 + 경찰서 + 안전상비 + 버스정류소 + 지하철 + 어린이집 + 유아인구수 + 총인구수,
           data = data_str, family = 'binomial')

summary(reg)

vif(reg)

reg_prob <- predict(reg, data_ste, type = "response")
reg_pred <- rep(0, dim(data_ste)[1])
reg_pred[reg_prob > 0.5] = 1

score(data_ste$심_응, reg_pred) # 모두 0으로 예측한 것보다 정확도가 떨어짐


#################### 층화 무작위 추출을 통한 로지스틱 회귀 ( test에 0:147개, 1:34개 )

library(caret)

set.seed(300)

idx_c <- createDataPartition(data$심_응, p = 0.7, list = FALSE)

data_ctr <- data[idx_c, ]
data_cte <- data[-idx_c, ]

reg_c <- glm(심_응 ~ 약국 + 소방서 + 경찰서 + 안전상비 + 버스정류소 + 지하철 + 어린이집 + 유아인구수 + 총인구수,
           data = data_ctr, family = 'binomial')

summary(reg_c)

vif(reg_c)

reg_prob_c <- predict(reg_c, data_cte, type = "response")
reg_pred_c <- rep(0, dim(data_cte)[1])
reg_pred_c[reg_prob_c > 0.5] = 1

score(data_cte$심_응, reg_pred_c)


stats::step(reg_c, direction = "both")

reg_cs <- glm(심_응 ~ 약국 + 버스정류소 + 유아인구수 + 총인구수,
             data = data_ctr, family = 'binomial')

summary(reg_cs)

vif(reg_cs)

reg_prob_cs <- predict(reg_cs, data_cte, type = "response")
reg_pred_cs <- rep(0, dim(data_cte)[1])
reg_pred_cs[reg_prob_cs > 0.5] = 1

score(data_cte$심_응, reg_pred_cs)


#################### 데이터 내보내기

write.csv(data, file = 'C:/Users/user/Desktop/서울/서울데이터_정제.csv', fileEncoding = 'euc-kr')

