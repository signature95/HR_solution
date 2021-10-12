# Nan value

df = data.frame(sex = c('M',"F",NA,'M','F'),
                score = c(5,4,3,4,NA))
df

is.na(df)
table(is.na(df))      #df의 nan값을 태이블로 만들어라.
table(is.na(df$sex))  #df의 sex칼럼에 대해 nan값을 태이블로
table(is.na(df$score))

# Nan 값이 있으면, 평균과 합계를 산출할 수 없음
mean(df$score)
sum(df$score)


# delete Nan value (column)
library(dplyr)
df %>%filter(is.na(score))    # score의 nan 값을 필터로 불러와라
df %>% filter(!is.na(score))  # score의 nan 값이 아닌 것을 필터로 불러와라.

df_nomiss = df %>% filter(!is.na(score))
mean(df_nomiss$score)
sum(df_nomiss$score)

# delete Non Value (dataframe)
df_nomiss = df %>% filter(!is.na(score)&!is.na(sex))
df_nomiss
df_nomiss2 = na.omit(df)
df_nomiss2

# Sum, Mean (without Non Value)
mean(df$score, na.rm = T) # Nan remove = True 
sum(df$score, na.rm = T)

# Practice
library(readxl)
exam = read.csv('csv_exam.csv')
exam[c(3,8,15), 'math'] = NA    # Math에 NA 할당
exam 


exam %>% summarise(mean_math = mean(math))  # mean_math = NA
exam %>% summarise(mean_math = mean(math, na.rm = T))
exam %>% summarise(mean_math = mean(math, na.rm = T),
                   sum_math = sum(math, na.rm = T),
                   median_math = median(math, na.rm = T))

# replace NA with mean_math
mean(exam$math, na.rm = T)  # mean값 확인
exam$math = ifelse(is.na(exam$math), 55, exam$math)  # mean값으로 NA 대체하기.
table(is.na(exam$math))  # NA값 확인하기.

exam


################################################################################################
# exercise

# 1. 
mpg = as.data.frame(ggplot2 :: mpg)
mpg[c(65,124,131,153,212), 'hwy'] = NA

table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# 2. 
mpg %>% 
  group_by(drv) %>% 
  filter(!is.na(hwy)) %>% 
  summarise(mean_hwy = mean(hwy))

################################################################################################

      