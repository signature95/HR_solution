# Practice

exam = read.csv("csv_exam.csv")
exam

# %>% 는 파이프라인이라고 보면 됨. (다음 함수에 그대로 전달해 줌.)

exam %>% filter(class == 1)

# dataset %>% filter (col_name == or != ?)을 통해서 추출하면 된다.
# %>% = ( command + shift + m )

exam %>% filter(class != 1)
exam %>% filter(math > 50)
exam %>% filter(english >= 70)

#########
# filter에 and를 의미하는 &를 사용하는 법

exam %>% filter(class == 1 & math >= 50)

# or을 의미하는 |(vertical bar)를 사용하여 사용한다.

exam %>% filter(math >= 90 | english >= 90)

# 특정 목록에 해당하는 행을 추출하고 싶은 경우

exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(class %in% c(1,3,5))

# creat new dataset

class1 = exam %>% filter(class==1)
class2 = exam %>% filter(class==2)
mean(class1$math)
mean(class2$math)


###############################################################################################
# test

# 1.
mpg_4 = mpg %>% filter(displ <= 4)
mpg_5 = mpg %>% filter(displ >= 5)
mean(mpg_4$highway)
mean(mpg_5$highway)

# 2.
mpg_audi = mpg %>% filter(manufacturer == 'audi')
mpg_toyota = mpg %>% filter(manufacturer == 'toyota')
mean(mpg_audi$city)
mean(mpg_toyota$city)

# 3.
mpg_car = mpg %>% filter(manufacturer %in% c('chevrolet', 'ford', 'honda') )
mean(mpg_car$highway)


###############################################################################################
# extract feature

exam %>% select(math)

exam %>% select(class, math, english)

# delete feature

exam %>% select(-math)
exam %>% select(-math, -english)

# combination filter, select

exam %>% filter(class == 1) %>% select(english)

# 가독성을 높이는 법

exam %>% 
  filter(class == 1) %>% 
  select(english)

exam %>%
  select(id, math) %>%  # exam 에서 id, math를 추출하고
  head                  # head를 통해 6개행을 출력한다.


###############################################################################################
# test

# 1.
mpg_sel = mpg %>% select(class,city)
head(mpg_sel, 10)

# 2. 
mpg_suv = mpg_sel %>% filter(class == 'suv')
mean(mpg_suv$city)

mpg_compact = mpg_sel %>% filter(class == 'compact')
mean(mpg_compact$city)

###############################################################################################
# practice

# 오름차순 정렬

exam %>% arrange(math)
exam %>% arrange(class, math)

# 내림차순 정렬

exam %>% arrange(desc(math))

###############################################################################################
# test
# 1. 
mpg %>% 
  filter(manufacturer == 'audi') %>% 
  arrange(desc(highway)) %>% 
  head(5)

###############################################################################################
# practice

exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(desc(total)) %>% 
  head

exam %>% 
  mutate(total = math + english + science,
         mean = total/3) %>% 
  arrange(desc(mean)) %>% 
  head

exam %>% 
  mutate(total = math + english + science,
         mean = total/3,
         test =ifelse(mean >= 60, 'pass', 'fail')) %>% 
  head

################################################################################################ test
library(ggplot2)
mpg = as.data.frame(ggplot2::mpg)
mpg = rename(mpg, city=cty, highway=hwy)


# 1.
mpg_new = mpg
mpg_new %>% 
  mutate(sum_c_h = city + highway) %>% 
  head

# 2. 
mpg_new = mpg_new %>% 
            mutate(sum_c_h = city + highway,
            avg_c_h = sum_c_h/2)
# 3.
mpg_new %>% 
  arrange(desc(avg_c_h)) %>% 
  head(3)

# 4.
mpg = mpg %>% 
  mutate(sum_c_h = city + highway,
         avg_c_h = sum_c_h/2) %>% 
  arrange(desc(avg_c_h))

head(mpg, 10)

################################################################################################ practice

# summarise by group
exam %>% summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>%                # 클래스별로 분리하여 
  summarise(mean_math = mean(math))  # math 평균을 산출한다.

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())  # 학생 수를 의미함.

# mean, sd(standard deviation), sum, median, min, max, n(frequence)를 의미한다.

mpg %>% 
  group_by(manufacturer, drv) %>%        # 제조사, 구동방식 별로 그룹화
  summarise(mean_city = mean(city)) %>%  # mean_city로 그룹화한 것에 city average 산출
  head(10)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == 'suv') %>% 
  mutate(avg_c_h = (city + highway)/2) %>% 
  summarise(mean_avg = mean(avg_c_h)) %>% 
  arrange(desc(mean_avg)) %>% 
  head(5)

################################################################################################ test

# 1.
mpg %>% 
  group_by(class) %>% 
  summarise(mean_city = mean(city))

# 2. 
mpg %>% 
  group_by(class) %>% 
  summarise(mean_city = mean(city)) %>% 
  arrange(desc(mean_city))

# 3. 
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_h = mean(highway)) %>% 
  arrange(desc(mean_h)) %>% 
  head(3)

# 4. 
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == 'compact') %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(5)



