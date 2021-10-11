###### OOO회사 2021년 9월 이직률은 약 23%(총종업원 14,999명 중 3,571명)이다.
table(HR$left)
###### 이직율을 낮추기 위하여 각 변수들 간 상관관계를 분석하여 해결방법을 제시하였다.
#####  1-1. 프로젝트 수, 평균근무시간과 직원 만족도 간의 상관관계를 분석
#####  1-2. 이를 통해 전직원들의 평균 업무량과 월 평균 근무시간 도출
#####  2-1. 이직여부 기준 변수별 상관관계 분석 및 결과 도출
#####  2-2. 이직율을 줄이기 위한 HR 솔루션 제시

install.packages('readcsv')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('PerformanceAnalytics')

# import library
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)


#데이터 불러오기
HR = read.csv("HR_comma_sep.csv")

#데이터 파악하기
head(HR)
View(HR)
dim(HR)

# 데이터 프레임에 NaN 값 존재 여부 확인
table(is.na.data.frame(HR))

#데이터 strings 파악
sapply(HR, class)

#데이터 요약
summary(HR)

#데이터 strings 변경

# satisfaction_level : 0~1사이의 값으로 표현됨. (만족 0% ~ 100%로 이해하면 편하기 떄문에 num에서 타입을 유지함.)
# last_evaluation :  0~1사이의 값으로 표현됨. (평가 0% ~ 100%로 이해하면 편하기 떄문에 num에서 타입을 유지함.)
# number_project : 2~7사이의 정수로 구성되어 있음. (이는 연속변수가 아님. 따라서 factor로 하는 것이 좋다고 판단함)
# average_monthly_hours : 96~310시간으로 구성되어 있음 (연속변수는 아니지만, 214개의 값이 들어감. 따라서 int로 유지)
# time_spend_company : 재직 연수로 2년~10년임. (이는 연속변수가 아님. 따라서 factor로 하는 것이 좋다고 판단함)
# Work_accident : 사고 유무로 판단하는데 0,1값만 존재 (logical로 설정하는 것이 좋을 것이라고 판단함. True, False이므로)
# left : 이직유무도 0.1만 존재함. (logical로 설정하는 것이 좋음. True, False이므로)
# promotion_last_5years : 0,1만 존재함. (logical로 설정하는 것이 좋음. True, False이므로)
# sales : 직군으로 구성되어 있음. (일반적인 문자열이지만, factor로 보는 것이 좋음. 8개 직군이므로)
# salary : high, low, medium으로 3가지로 나뉨. (문자열이지만 factor로 설정하는 것이 더 분석에 어울림.)

# 따라서 satisfaction_level, last_evaluation, average_monthly_hours의 타입은 유지하고 나머지 7개를 변경해준다.

HR$number_project = as.factor(HR$number_project)
HR$time_spend_company = as.factor(HR$time_spend_company)
HR$Work_accident=as.logical(HR$Work_accident)
HR$left=as.logical(HR$left)
HR$promotion_last_5years=as.logical(HR$promotion_last_5years)
HR$sales=as.factor(HR$sales)
HR$salary=as.factor(HR$salary)

sapply(HR, class)  

# 분석파트

#### 전직원들의 평균 업무량과 월 평균 근무시간 도출
### 1-1. 프로젝트 수와 만족도 상관관계 분석

boxplot(HR$satisfaction_level)      # 전체직원 만족도 중앙값 0.64
boxplot(HR$satisfaction_level)$stat # 전체 직원 만족도의 박스플롯 통계치

# hr1 : 프로젝트 개수별로 출력한 만족도의 평균치
hr1 = HR %>% 
  group_by(number_project) %>% 
  select(satisfaction_level) %>% 
  summarise(mean_sl = mean(satisfaction_level)) %>% 
  arrange(number_project)
hr1

# hr1으로 출력힌 col_plot (프로젝트 개수 별 만족도의 평균치)
ggplot(data = hr1, aes(x=number_project, y=mean_sl)) + 
  geom_col(aes(fill=number_project), alpha = 0.5)

# 원 데이터 HR로 출력힌 값 (프로젝트 개수별 만족도의 분포에 이직여부를 추가함)
# 걀국 우리는 이직을 결정하는 요소를 보는 것이기 때문에 이직여부를 추가하여 분석에 활용할 필요가 있음.
ggplot(data = HR, aes(x=number_project, y=satisfaction_level)) + 
  geom_violin(aes(fill=number_project), trim=TRUE, 
               adjust=10) +
  geom_jitter(position = position_jitter(.5),
              aes(col = left),
              alpha=0.25)

# 하지만, cor test를 보면, 상관성이 크지 않다는 것을 확인할 수 있음. (크게 중요하진 않음.)
# 애초에 project수행이 증가하다가 감소하는 형태로 linear한 상관성은 나오지 않기 때문임.
cor.test(as.numeric(hr1$number_project), hr1$mean_sl)


## conclusion
# 전체직원 만족도 중앙값 0.6
# 2개 : 평균만족도는 0.479. (어느정도 자신의 일이 주어져야 만족도가 높은 듯)
# 3~5개 : 평균만족도 0.68 ~ 0.695
# 6개 : 평균만족도 : 0.119 ~ 0.273
# 직원들의 평균 업무량은 3~5개가 적당
# 하지만, 이직을 보면, 4~5개 프로젝트를 수행한 인원 중 만족도가 높은 부분에 이직이 발생한 것도 주목해야 함.

### 1-2. 평균근무시간과 만족도 상관관계 분석 
boxplot(HR$average_montly_hours)        # 전체직원의 근무시간 중앙값 200
boxplot(HR$average_montly_hours)$stat   # (수정) 중앙값 외에도 box플롯의 통계치를 제시하고자 만듬 

# hr2 : 근무 시간 별 만족도의 평균 수치를 출력
hr2 = HR %>% 
  group_by(average_montly_hours) %>% 
  select(satisfaction_level) %>% 
  summarise(mean_sl = mean(satisfaction_level)) %>% 
  arrange(desc(mean_sl))
hr2

# hr2로 출력힌 line_plot (근무 시간 별 만족도의 평균치)
ggplot(data = hr2, aes(x=average_montly_hours, y=mean_sl)) + geom_line()

# hr3 : 근무 시간을 순차적으로 나열하고 이직 여부를 반영한, 만족도 평균치를 추출.
hr3 = HR %>% 
  group_by(average_montly_hours, left) %>% 
  select(satisfaction_level) %>% 
  summarise(mean_sl = mean(satisfaction_level)) %>% 
  arrange(average_montly_hours)
hr3

# hr3로 출력힌 col_plot (근무 시간 별 만족도의 평균치)
ggplot(hr3, aes(average_montly_hours, mean_sl)) + 
  geom_line() +
  geom_point(position = position_jitter(.9),
              aes(col = left))

# 여기서는 만족도가 너무 높아도, 너무 낮아도 이직이 발생하는 것을 확인할 수 있다.
# 대체적으로 사람들은 0.625 ~ 0.75 정도의 만족도를 가진 경우 이회사에 남아있는 것을 택한다. (월 근무시간은 130~275시간 사이)

## conclusion
# 전체직원의 근무시간 중앙값 200
# 100~150 hours : 0.4 ~ 0.6
# 150~275 hours : higher satisfaction (upper 0.6)
# upper 275 : dramatically lower satisfaction (0.2 ~ 0.4)
# 직원들의 월평균 근무시간은 150시간에서 275시간 유지 필요


##### 2.이직여부를 판단하여 HR 솔루션 제시
#### 2-1.이직여부기준 상관관계 분석
### 2-1-1. 이직여부기준 부서 및 만족도 상관관계조사
table(HR$left) # 이직 유무를 좀더 수치화하여 표현하기 위해 table사용

ggplot(HR, aes(x=sales, y=satisfaction_level)) + 
  geom_violin(aes(fill=sales), trim = TRUE) +
  geom_jitter(position=position_jitter(0.5), 
              aes(col=left),  alpha=0.5)+
  labs(col ='left or not')

ggplot(HR, aes(x=sales, y=satisfaction_level)) + 
  geom_boxplot(aes(fill=sales), outlier.color = 'red') +
  geom_jitter(position=position_jitter(0.5),
              aes(col=left), alpha=0.5)+
  labs(col ='left or not')

## 해석
# 대체로 만족도는 0.38 ~ 1.00에 많이 포진되어 있는 편임.
# 또한 0.05~0.1 에 위치한 사람들도 많이 보임.
# 직군 중에서는 sales, support, technical에서 사람들이 많이 있는 듯 함.
# 여기서 주목할 점은 만족도가 0.75~0.93, 0.35~0.44?, 0.1 미만인 곳에서 이직이 이뤄진다는 점이다.
# 이는 전 직군에 공통적으로 발생하고 있다. 
# 또한, sales, suppport, technical에서는 상대적으로 주목할 부분 외에도 이직이 간간히 발생한다.
# 이에 대한 해석으로는 아무래도 직군에 종사하는 인원의 차이 때문인 듯 하다.

### 2-1-2. 이직여부기준 임금수준, 만족도 그래프
ggplot(HR,aes(x=left,y=satisfaction_level)) +
  geom_boxplot(aes(fill = salary),alpha = I(0.4),outlier.colour = 'red') +
  xlab("left") + ylab("satisfaction")+
  labs(fill = "salary")

l()## 해석 : 3가지 케이스로 구분(하위 25% ~ 하위75%)
# (1) low : 0.23 ~ 0.73
# (2) medium : 0.125 ~ 0.75
# (3) high : 0.375 ~ 0.45
# "low"와 "medium"의 만족도를 높이기 위한 솔루션 필요 


### 2-1-3. 이직여부기준 월평균근무시간, 만족도 그래프
ggplot(HR,aes(x=average_montly_hours,y=satisfaction_level))+
  geom_point(aes(col = left)) + 
  labs(col = "left") + xlab("hours") + ylab("satisfaction")

## 해석 : 3가지 케이스로 구분
# (1) 만족도 : 0.375 ~ 0.47 and 월평균 근무시간 : 125시간 ~ 160시간
# (2) 만족도 : 0.125이하 and 월평균 근무시간 : 240시간 ~ 300시간 이상
# (3) 만족도 : 0.75이상 and 월평균 근무시간 : 210시간 ~ 260시간
# 월평균 200시간 넘게 일한 직원들을 위한 솔루션 필요 

### 2-1-4. 이직여부기준 월평균근무시간, 임금수준 그래프

## 그래프 그리기
ggplot(HR,aes(x=salary,y=average_montly_hours)) +
  geom_jitter(aes(col = left)) +
  labs(col = "left") + xlab("salary") + ylab("hours") +
  scale_x_discrete(limits = c("low", "medium", "high"))

## 해석 : 임금별로 구분
# (1) low : 125 ~ 165시간, 225시간~300시간 이상
# (2) medium : 125시간 ~ 165시간, 225시간 300시간 이상
# (3) high : 이직자수가 적어 관리 불필요
# "low"임금, "medium"임금을 받는 사람 중 275시간 이상 근무하는 직원에게 인센티브 지급하여 이직률 상승 방지


### 2-1-5. 이직여부기준 프로젝트 수, 마지막평가점수 그래프
ggplot(HR,aes(x=number_project, y=last_evaluation)) +
  geom_jitter(aes(col = left)) +   
  labs(col = "left") + xlab("project_number") + ylab("evaluation")

## 해석 
# (1) 2개 프로젝트를 수행하면서 평가점수가 가 0.58 이하 
# (2) 4 ~ 5개의 프로젝트를 수행하면서 만족도가 0.8 이상
# (3) 7개 프로젝트를 수행하는 직원의 이직 방지를 솔루션 필요 
# (4) 7개 프로젝트를 수행하는 직원의 업무 강도를 낮춰주는 형태도 중요
HR$satisfaction_level = ifelse(HR$satisfaction_level < 0.1, 1,
                                      ifelse(HR$satisfaction_level < 0.3, 2,
                                             ifelse(HR$satisfaction_level < 0.5, 3,
                                                    ifelse(HR$satisfaction_level < 0.75, 4, 5))))

ggplot(HR,aes(x=number_project, y=last_evaluation)) +
  geom_jitter(aes(col = satisfaction_level), position=position_jitter(0.5)) +   
  labs(col = "satisfaction_level") + xlab("project_number") + ylab("evaluation")


