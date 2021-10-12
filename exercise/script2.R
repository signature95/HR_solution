b <- 2
b * 3
library(ggplot2)
x <- c('a','b','c','a')
qplot(x)
x <- c(90,80,60,70)
mean_x <-mean(x)

###############################################################################################

qplot(data = mpg, x = hwy)
qplot(data = mpg, x = drv, y = hwy, geom='boxplot', colour = drv)
english <- c(90,80,60,70)
english
math <- c(50,60,100,20)
math
df_midterm <- data.frame(english,math)
mean(df_midterm$english)
mean(df_midterm$math)
df_midterm = data.frame(english = c(90,80,60,70),
                        math = c(50,60,100,20),
                        class = c(1,1,2,2))
df_midterm
###############################################################################################
# test

df_sale = data.frame(product = c('apple','strawberry','watermelon'),
                     price = c(1800, 1500, 3000),
                     sales = c(24,38,13))
mean(df_sale$price)
mean(df_sale$sales)

###############################################################################################

install.packages("readxl")

df_exam = read_excel("excel_exam.xlsx")
df_exam
mean(df_exam$math)
mean(df_exam$english)
mean(df_exam$science)

df_exam_novar = read_excel("excel_exam_novar.xlsx")
df_exam_novar

df_exam_novar = read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam_novar

# col_names = False 를 사용하여 변수명을 '...숫자'로 자동 지정해줌.

df_exam_sheet = read_excel("excel_exam_sheet.xlsx", sheet = 3)
df_exam_sheet

# 해당 파일에는 3번째 시트에 데이터가 있기 때문에, sheet=3로 지정하였다.

df_csv_exam = read.csv('csv_exam.csv')
df_csv_exam

df_csv_exam = read.csv("csv_exam.csv", stringsAsFactors = F)
df_csv_exam

#문자가 있는 파일인 경우, 변수를 factor가 아닌 string으로 불러오기 위해 stringAsFactors=F를 사용한다.

###############################################################################################

df_midterm = data.frame(english = c(90,80,60,70),
                        math = c(50,60,100,20),
                        class = c(1,1,2,2))
df_midterm

write.csv(df_midterm, file = 'df_midterm.csv')

# csv파일로 저장하려면 write.csv를 사용하고 df를 부른다음 파일 명을 지정해준다.

saveRDS(df_midterm, file='df_midterm.rds')

# R 전용 파일로 RDS를 생성할 수있음. (외부에서 사용하는 경우 csv를 주로 이용하긴함.)

rm(df_midterm)

# Ram에 잇는 데이터를 remove하는 코드
# 우측 환경 창에 있는 df_midterm을 삭제하는 것임. 따라서 df_midterm을 사용하면 에러가 발생함.

df_midterm

df_midterm = readRDS('df_midterm.rds')
df_midterm

# df_midterm을 다시 불러오는 형태이다. 우측 창에 df_midterm이 다시 보이는 것을 확인할 수 있다.

###############################################################################################
# Practice

exam = read.csv('csv_exam.csv')
head(exam)  # default=6으로 설정되어 있음. 
tail(exam, 10)
View(exam)  # 불러온 csv를 뷰어창으로 보는 법
dim(exam)   # shape라고 보면 편함. (dim = dimension)
str(exam)   # info로 보면 됨
summary(exam) # 파이썬과 동일함. (요약통계량, describe)

###############################################################################################
# exercise

library(ggplot2)
mpg = as.data.frame(ggplot2::mpg) # mpg를 데이터프레임 형태로 불러오는 법
head(mpg)
tail(mpg)
dim(mpg)
str(mpg)
summary(mpg)  # 연산이 가능한 부분은 통계치를 제공하지만, 불가능하면 len으로 출력함.
?mpg

###############################################################################################
# Practice

df_raw = data.frame(var1 = c(1,2,1),
                    var2 = c(2,3,2))
df_raw

library(dplyr)

df_new = df_raw  # copy 생성
df_new

df_new = rename(df_new, v2=var2)  # var2칼럼을 v2로 변경하는 법 (rename(df, col_new = col_old))
df_new
df_raw
###############################################################################################
# exercise

mpg = rename(mpg, city=cty, highway=hwy)
head(mpg)
print(mpg)
print(head(mpg))

###############################################################################################
# Practice

df = data.frame(var1=c(4,3,8),
                var2=c(2,6,1))
df
df$var_sum = df$var1 + df$var2
df
df$var_mean = (df$var1 + df$var2)/2
df

# dataset $ new_col(mean) = (old_col1 + old_col2 + ... + old_coln)/n 형태로 출력할 수 있음.

###############################################################################################
# exercise 

mpg$total = (mpg$city + mpg$highway)/2
head(mpg)
mean(mpg$total)

summary(mpg$total)
hist(mpg$total)

mpg$test = ifelse(mpg$total >= 20, 'pass', 'fail')
# 조건문 (dataset$column <,>,= 등, condition, 'when True', 'when False')

head(mpg, 20)

table(mpg$test)  # table 형태로 pass, fail에 맞는 데이터를 출력한다.
                 # value_counts로 생각하면 된다.
qplot(mpg$test)  # plot형태로 mpg$test를 확인한다.

mpg$grade = ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 20, "B", "C"))
# multiple conditional function

table(mpg$grade)
qplot(mpg$grade)


mpg$grade2 = ifelse(mpg$total>= 30, "A", 
                    ifelse(mpg$total >= 25, "B", 
                           ifelse(mpg$total >= 20, "C", "D")))
table(mpg$grade2)
qplot(mpg$grade2)

###############################################################################################
# test

# 1.
midwest = data.frame(midwest)
summary(midwest)
str(midwest)
dim(midwest)

# 2.
midwest = rename(midwest, total=poptotal, asian=popasian)
head(midwest)

# 3.
midwest$asian_ratio = (midwest$asian)/(midwest$total)*100
mean(midwest$asian_ratio)
hist(midwest$asian_ratio)

# 4.
midwest$condition = ifelse(midwest$asian_ratio >= mean(midwest$asian_ratio), 'large','small')

# 5.
qplot(midwest$condition)
table(midwest$condition)

###############################################################################################
