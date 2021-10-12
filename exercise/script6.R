data = data.frame(x1 = c(0,1,0,7,1),
                  x2 = c(5,0,0,0,0),
                  x3 = 3)

data
colnames(data)
max.col(data[,1:3])
colnames(data)[max.col(data[1,1:3])]

# delete outlier

outlier = data.frame(sex=c(1,2,1,3,2,1),
                     score=c(5,4,3,4,2,6))
outlier

# check outliers

table(outlier$sex)
table(outlier$score)

# preprocessing

# sex == 3일때 NA로 처리하라. (outlier $ sex 칼럼에 대입)
outlier$sex = ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

# score > 5 일때, NA로 처리하라. (outlier $ scroe 칼럼에 대입)
outlier$score = ifelse(outlier$score > 5, NA, outlier$score)
outlier

# outlier에서 sex, score 중 NA가 아닌것을 filter로 처리하고 sex로 묶어서 score_mean을 구하라.
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

# boxplot
mpg = as.data.frame(ggplot2 :: mpg)
boxplot(mpg$hwy)

# [1,] : 하단 극단치 경계
# [2,3,4] : 1,2,3 분위수 
# [5] : 상단 극단치 경계
boxplot(mpg$hwy)$stats

mpg$hwy = ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm =T))

################################################################################################
# exercise
mpg = as.data.frame(ggplot2 :: mpg)
mpg[c(10,14,58,93), 'drv'] <- 'k'
mpg[c(29,43,129,203), 'cty'] <- c(3,4,39,42)

# 1. 
table(mpg$drv)
mpg$drv = ifelse(mpg$drv %in% 'k', NA, mpg$drv)
table(mpg$drv)

# 2.
boxplot(mpg$cty)$stats
mpg$cty = ifelse(mpg$cty > 26 | mpg$cty < 9, NA, mpg$cty)
boxplot(mpg$cty)

# 3.
mpg %>% 
  filter((!is.na(mpg$drv)) & (!is.na(mpg$cty))) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))%>% 
  arrange(desc(mean_cty))
