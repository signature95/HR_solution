# practice

test1 = data.frame(id = c(1,2,3,4,5),
                   midterm = c(60,80,70,90,85))
test2 = data.frame(id = c(1,2,3,4,5),
                   final = c(70,83,65,95,80))

test1
test2

# dataframe integrate (left_join)

total = left_join(test1, test2, by='id')   # test1, 2를 id에 기반해서 묶어라.
total


name = data.frame(class = c(1,2,3,4,5),
                  teacher = c('kim', 'lee','park','choi','jung'))
name

exam_new = left_join(exam, name, by='class')
exam_new

# dataframe integrate (bind_rows)

group_a = data.frame(id = c(1,2,3,4,5),
                     test = c(60, 80, 70, 90, 85))
group_b = data.frame(id = c(1,2,3,4,5),
                     test = c(70, 83, 65, 95, 80))
group_all = bind_rows(group_a, group_b)
group_all

################################################################################################
# exercise

#1.

fuel = data.frame(fl = c('c', 'd', 'e', 'p', 'r'),
                  price_f1 = c(2.35, 2.38, 2.11, 2.76, 2.22),
                  stringsAsFactors = F)
mpg = left_join(mpg, fuel, by = 'fl')


# 2.

mpg %>% 
  select(model, fl, price_f1) %>% 
  head(5)

################################################################################################
# test
midwest = data.frame(midwest)

# 1. 
midwest_new = midwest
midwest_new = midwest_new %>%
  mutate(non_adult_ratio = (poptotal - popadults)/poptotal * 100)

# 2.
midwest_new %>% 
  arrange(desc(non_adult_ratio)) %>% 
  select(county, non_adult_ratio) %>% 
  head(5)

# 3.
midwest_new = midwest_new %>% 
  mutate(class_nonadult = ifelse(non_adult_ratio >= 40, 'large' ,
                ifelse(non_adult_ratio >= 30, 'middle', 'small')))
midwest_new %>% 
  group_by(class_nonadult) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# 4.
midwest_new = midwest_new %>% 
  mutate(asian_ratio = popasian / poptotal * 100)
midwest_new %>% 
  select(state,county,asian_ratio) %>% 
  arrange(asian_ratio) %>% 
  head(10)
