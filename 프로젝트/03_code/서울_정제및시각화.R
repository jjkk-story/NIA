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


#################### 데이터 시각화

# 종속변수 시각화
g.y <- ggplot(data, aes(x=as.factor(심_응))) + geom_bar() +
  xlab("서울 응급실과 심야약국의 여부")

g.y

# 독립변수 시각화
g.약 <- ggplot(data, aes(x=약국)) + geom_histogram()

gb.약 <- ggplot(data, aes(x=1, y=약국)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("약국의 Box Plot")

grid.arrange(g.약, gb.약, nrow=1, ncol=2)

g.소 <- ggplot(data, aes(x=소방서)) + geom_histogram(bins=3) +
  scale_x_continuous(breaks = c(0,1))

gb.소 <- ggplot(data, aes(x=1, y=소방서)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("소방서의 Box Plot")

grid.arrange(g.소, gb.소, nrow=1, ncol=2)

g.경 <- ggplot(data, aes(x=경찰서)) + geom_histogram(bins=9)

gb.경 <- ggplot(data, aes(x=1, y=경찰서)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("경찰서의 Box Plot")

grid.arrange(g.경, gb.경, nrow=1, ncol=2)

g.안 <- ggplot(data, aes(x=안전상비)) + geom_histogram()

gb.안 <- ggplot(data, aes(x=1, y=안전상비)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("안전상비의 Box Plot")

grid.arrange(g.안, gb.안, nrow=1, ncol=2)

g.버 <- ggplot(data, aes(x=버스정류소)) + geom_histogram()

gb.버 <- ggplot(data, aes(x=1, y=버스정류소)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("버스정류소의 Box Plot")

grid.arrange(g.버, gb.버, nrow=1, ncol=2)

g.지 <- ggplot(data, aes(x=지하철)) + geom_histogram(bins=11)

gb.지 <- ggplot(data, aes(x=1, y=지하철)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("지하철의 Box Plot")

grid.arrange(g.지, gb.지, nrow=1, ncol=2)

g.어 <- ggplot(data, aes(x=어린이집)) + geom_histogram()

gb.어 <- ggplot(data, aes(x=1, y=어린이집)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("어린이집의 Box Plot")

grid.arrange(g.어, gb.어, nrow=1, ncol=2)

g.유 <- ggplot(data, aes(x=유아인구수)) + geom_histogram()

gb.유 <- ggplot(data, aes(x=1, y=유아인구수)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("유아인구수의 Box Plot")

grid.arrange(g.유, gb.유, nrow=1, ncol=2)

g.총 <- ggplot(data, aes(x=총인구수)) + geom_histogram()

gb.총 <- ggplot(data, aes(x=1, y=총인구수)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("총인구수의 Box Plot")

grid.arrange(g.총, gb.총, nrow=1, ncol=2)


#################### 종속변수 그룹별 x들의 관계 plot

ggplot(data, aes(x = 약국, fill = as.factor(심_응))) + 
  geom_histogram() + facet_grid(.~as.factor(심_응)) +   
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 소방서, fill = as.factor(심_응))) + 
  geom_histogram(bins=3) + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 경찰서, fill = as.factor(심_응))) + 
  geom_histogram(bins=9) + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 안전상비, fill = as.factor(심_응))) + 
  geom_histogram() + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 버스정류소, fill = as.factor(심_응))) + 
  geom_histogram() + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 지하철, fill = as.factor(심_응))) + 
  geom_histogram() + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 어린이집, fill = as.factor(심_응))) + 
  geom_histogram() + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 유아인구수, fill = as.factor(심_응))) + 
  geom_histogram() + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")

ggplot(data, aes(x = 총인구수, fill = as.factor(심_응))) + 
  geom_histogram() + facet_grid(.~as.factor(심_응)) +
  labs(fill = "응급실과 심야약국의 여부")


#################### 정규성 검정 (안해도 되는 것 같음)

# 정규성 검정 : p값이 0.05보다 커야 정규성을 만족
data_1 <- data[data$심_응 == 1, ]
dim(data_1)

data_0 <- data[data$심_응 == 0, ]
dim(data_0)

shapiro.test(data$약국) # p-value < 2.2e-16
shapiro.test(data_1$약국) # p값 : 0.3439 : 만족
shapiro.test(data_0$약국) # p-value < 2.2e-16

shapiro.test(data$소방서) # p-value < 2.2e-16
shapiro.test(data_1$소방서) # p-value < 2.2e-16
shapiro.test(data_0$소방서) # p-value < 2.2e-16

shapiro.test(data$경찰서) # p-value < 2.2e-16
shapiro.test(data_1$경찰서) # p-value = 2.103e-11
shapiro.test(data_0$경찰서) # p-value < 2.2e-16

shapiro.test(data$안전상비) # p-value < 2.2e-16
shapiro.test(data_1$안전상비) # p값 : 0.003547
shapiro.test(data_0$안전상비) # p-value < 2.2e-16

shapiro.test(data$버스정류소) # p-value = 2.409e-12
shapiro.test(data_1$버스정류소) # p값 : 0.09556 :만족
shapiro.test(data_0$버스정류소) # p-value = 4.08e-13

shapiro.test(data$지하철) # p-value < 2.2e-16
shapiro.test(data_1$지하철) # p-value = 6.863e-09
shapiro.test(data_0$지하철) # p-value < 2.2e-16

shapiro.test(data$어린이집) # p-value < 2.2e-16
shapiro.test(data_1$어린이집) # p값 : 0.03971
shapiro.test(data_0$어린이집) # p-value < 2.2e-16

shapiro.test(data$유아인구수) # p-value < 2.2e-16
shapiro.test(data_1$유아인구수) # p값 : 0.05544 : 만족
shapiro.test(data_0$유아인구수) # p-value < 2.2e-16

shapiro.test(data$총인구수) # p-value = 4.86e-15
shapiro.test(data_1$총인구수) # p값 : 0.133 : 만족
shapiro.test(data_0$총인구수) # p-value = 1.916e-15


#################### log 함수 써서 유아인구수, 총인구수 시각화 (안해도 되는 것 같음)

g.유.l <- ggplot(data, aes(x=sqrt(유아인구수+1))) + geom_histogram()

gb.유.l <- ggplot(data, aes(x=1, y=sqrt(유아인구수+1))) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("sqrt(유아인구수)의 Box Plot")

grid.arrange(g.유, gb.유, g.유.l, gb.유.l, nrow=2, ncol=2)

shapiro.test(data$유아인구수) # p-value < 2.2e-16
shapiro.test(sqrt(data$유아인구수+1)) # p-value = 6.616e-10
shapiro.test(sqrt(data$유아인구수)) # p-value = 6.616e-10
shapiro.test(log(data$유아인구수+1)) # p-value = 6.616e-10

g.총.s <- ggplot(data, aes(x=sqrt(총인구수))) + geom_histogram()

gb.총.s <- ggplot(data, aes(x=1, y=sqrt(총인구수))) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + # x축 이름 생략
  theme(axis.title.x = element_blank()) + # x축 구분자 생략
  ggtitle("sqrt(총인구수)의 Box Plot")

grid.arrange(g.총, gb.총, g.총.s, gb.총.s, nrow=2, ncol=2)

shapiro.test(data$총인구수) # p-value = 4.86e-15
shapiro.test(sqrt(data$총인구수)) # p-value = 7.202e-14


#################### 상관관계

plot(data[,4:13])
pairs.panels(data[,4:13])
chart.Correlation(data[,4:13], histogram=, pch="+")
ggcorr(data[,4:13], name="corr", label=T)

plot(data[,5:13])
pairs.panels(data[,5:13])
chart.Correlation(data[,5:13], histogram=, pch="+")
ggcorr(data[,5:13], name="corr", label=T)

# 독립변수 간의 다중공선성 확인
# 10이상이면 제거
VIF(lm(약국~., data=data[,5:13]))
VIF(lm(소방서~., data=data[,5:13]))
VIF(lm(경찰서~., data=data[,5:13]))
VIF(lm(안전상비~., data=data[,5:13]))
VIF(lm(버스정류소~., data=data[,5:13]))
VIF(lm(지하철~., data=data[,5:13]))
VIF(lm(어린이집~., data=data[,5:13]))
VIF(lm(유아인구수~., data=data[,5:13]))
VIF(lm(총인구수~., data=data[,5:13]))


#################### var.test

# 종속변수의 그룹별로 설명변수의 분산이 같은지 var.test 진행
# 귀무가설 : 두 집단의 분산이 같다
# p-value가 0.05보다 작으면 두 집단의 분산이 다르다

X_names <- names(data.frame(data[,5:13]))
X_names

var.test_p.value_df <- data.frame() 

for(i in 1:length(X_names)){
  var.test_p.value <- var.test(data[,X_names[i]] ~ data$심_응)$p.value
  var.test_p.value_df[i,1] <- X_names[i]
  var.test_p.value_df[i,2] <- var.test_p.value
}

colnames(var.test_p.value_df) <- c("x_var_name", "p.value")
var.test_p.value_df
arrange(var.test_p.value_df, p.value)

# 약국, 안전상비, 버스정류소, 경찰서, 총인구수 : 두 집단의 분산이 같다
# 유아인구수, 지하철, 어린이집, 소방서 : 두 집단의 분산이 다르다


#################### t.test

# 종속변수의 그룹별로 설명변수 간의 차이가 존재하는지 t.test 진행
# 귀무가설 : 두 집단의 평균이 같다
# p-value가 0.05보다 큰 값을 가지면 차이가 존재하지 않으므로 설명변수 제거

# 등분산성 만족
X_names_ve <- names(data.frame(data[,c(5, 7, 8, 9, 13)]))
X_names_ve

t.test_p.value_df_ve <- data.frame() 

for(i in 1:length(X_names_ve)){
  t.test_p.value <- t.test(data[,X_names_ve[i]] ~ data$심_응, var.equal = TRUE)$p.value
  t.test_p.value_df_ve[i,1] <- X_names_ve[i]
  t.test_p.value_df_ve[i,2] <- t.test_p.value
}

colnames(t.test_p.value_df_ve) <- c("x_var_name", "p.value")
t.test_p.value_df_ve
arrange(t.test_p.value_df_ve, p.value)

# 등분산성 만족X
X_names_vue <- names(data.frame(data[,c(6, 10, 11, 12)]))
X_names_vue

t.test_p.value_df_vue <- data.frame() 

for(i in 1:length(X_names_vue)){
  t.test_p.value <- t.test(data[,X_names_vue[i]] ~ data$심_응, var.equal = TRUE)$p.value
  t.test_p.value_df_vue[i,1] <- X_names_vue[i]
  t.test_p.value_df_vue[i,2] <- t.test_p.value
}

colnames(t.test_p.value_df_vue) <- c("x_var_name", "p.value")
t.test_p.value_df_vue
arrange(t.test_p.value_df_vue, p.value) # 소방서 out

t.test_p.value_df <- rbind(t.test_p.value_df_ve, t.test_p.value_df_vue)
t.test_p.value_df <- t.test_p.value_df[t.test_p.value_df$p.value < 0.05,]
t.test_p.value_df

t.test_p.value_df <- arrange(t.test_p.value_df, desc(p.value))
t.test_p.value_df

data_소o <- data[, -6]
  

#################### box plot : p-value 작은 순서대로 : 차이가 심하다

ggplot(data, aes(x = as.factor(심_응), y = 약국, fill = as.factor(심_응))) + 
  geom_boxplot(horizental = T) + labs(fill = "응급실과 심야약국의 여부") + coord_flip()

ggplot(data, aes(x = as.factor(심_응), y = 안전상비, fill = as.factor(심_응))) + 
  geom_boxplot() + labs(fill = "응급실과 심야약국의 여부") + coord_flip()

ggplot(data, aes(x = as.factor(심_응), y = 버스정류소, fill = as.factor(심_응))) + 
  geom_boxplot() + labs(fill = "응급실과 심야약국의 여부") + coord_flip()

ggplot(data, aes(x = as.factor(심_응), y = 총인구수, fill = as.factor(심_응))) + 
  geom_boxplot() + labs(fill = "응급실과 심야약국의 여부") + coord_flip()

ggplot(data, aes(x = as.factor(심_응), y = 지하철, fill = as.factor(심_응))) + 
  geom_boxplot() + labs(fill = "응급실과 심야약국의 여부") + coord_flip()

ggplot(data, aes(x = as.factor(심_응), y = 어린이집, fill = as.factor(심_응))) + 
  geom_boxplot() + labs(fill = "응급실과 심야약국의 여부") + coord_flip()

ggplot(data, aes(x = as.factor(심_응), y = 경찰서, fill = as.factor(심_응))) + 
  geom_boxplot() + labs(fill = "응급실과 심야약국의 여부") + coord_flip()

ggplot(data, aes(x = as.factor(심_응), y = 유아인구수, fill = as.factor(심_응))) + 
  geom_boxplot() + labs(fill = "응급실과 심야약국의 여부") + coord_flip()


#################### 로지스틱 회귀

mis.rates <- function(x, y) sum(x!=y)/length(x)

set.seed(100000000)

train <- sample(1: nrow(data_소o), size = 0.7*nrow(data_소o), replace=F)

data_소o_tr <- data_소o[train, ]
data_소o_te <- data_소o[-train, ]

reg <- glm(심_응 ~ 약국 + 경찰서 + 안전상비 + 버스정류소 + 지하철 + 어린이집 + 유아인구수 + 총인구수,
           data = data_소o_tr, family = 'binomial')

summary(reg)

vif(reg)

reg_prob <- predict(reg, data_소o_te, type = "response")
reg_pred <- rep(0, dim(data_소o_te)[1])
reg_pred[reg_prob > 0.5] = 1

mis.rates(reg_pred, data_소o_te$심_응)
(acc <- 1-mis.rates(reg_pred, data_소o_te$심_응)) # 정확도 : 81.3%

library(MLmetrics)

f1 <- F1_Score(reg_pred, data_소o_te$심_응) # F1 score : 89.6%


#################### 모두 0이라고 예측했을 때( test에 0:150개, 1:32개 )

all0_pred <- rep(0, dim(data_소o_te)[1])

table(all0_pred, data_소o_te$심_응)

1-mis.rates(all0_pred, data_소o_te$심_응) # 82.4%
F1_Score(all0_pred, data_소o_te$심_응) # 90.4%


#################### 로지스틱 회귀 stepwise

reg_step <- stats::step(reg, direction = "both")

reg_s <- glm(심_응 ~ 약국 + 버스정류소 + 유아인구수 + 총인구수,
             data = data_소o_tr, family = 'binomial')

summary(reg_s)

vif(reg_s)

reg_s_prob <- predict(reg_s, data_소o_te, type = "response")
reg_s_pred <- rep(0, dim(data_소o_te)[1])
reg_s_pred[reg_s_prob > 0.5] = 1

mis.rates(reg_s_pred, data_소o_te$심_응)
(acc_s <- 1-mis.rates(reg_s_pred, data_소o_te$심_응)) # 정확도 : 81.9%

(f1_s <- F1_Score(reg_s_pred, data_소o_te$심_응)) # F1 score : 89.9%


#################### 데이터 내보내기

write.csv(data, file = 'C:/Users/user/Desktop/서울/서울데이터_정제.csv', fileEncoding = 'euc-kr')

