# scatter
library(plotly)
library('ggplot2')

mpg = as.data.frame(ggplot2::mpg)

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(col = fl)) +
  xlim(3, 6)

# 해당 displ의 값에는 7인 값도 있으므로, 105행이 missing 되었다는 에러 메세지가 뜸.
# Removed 105 rows containing missing values (geom_point). 

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(col = fl)) +
  xlim(3, 6) + ylim(10, 30)

##########################################################################################################################################
# exercise
# 1.
ggplot(data = mpg, aes(x=cty, y=hwy)) +
  geom_point(aes(col = class))

# 2. 
midwest = as.data.frame(ggplot2::midwest)
a1 = ggplot(data = midwest, aes(x=poptotal, y=popasian)) +
  geom_point(aes(col = state)) +
  xlim(0, 500000) + ylim(0, 10000)
ggplotly(a1)
##########################################################################################################################################
# bar chart
# 데이터 요약을 먼저 수행한 후에 진행한다.  
library(dplyr)

df_mpg = mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

df_mpg

ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) +
  geom_col(aes(fill=drv))

ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) +
  geom_col(aes(fill=drv))

# 빈도 막대 그리기.
# 원자료를 이용하여 수행한다.
ggplot(data = mpg, aes(x = drv)) +
  geom_bar(aes(fill=drv), alpha = .7)

ggplot(data = mpg, aes(x = hwy)) +
  geom_bar(aes(fill=drv))

##########################################################################################################################################
# exercise

# 1.
mpg_df = mpg %>%
  filter(mpg$class == 'suv') %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>%
  head(5)
mpg_df

ggplot(data = mpg_df, aes(reorder(x = manufacturer, mean_cty), y = mean_cty)) +
  geom_col(aes(fill = manufacturer), alpha = 0.5)


# 2. 
ggplot(data = mpg, aes(x = class)) +
  geom_bar(aes(fill = manufacturer), alpha = .7)

##########################################################################################################################################
# line chart (ggplot2에 내장되어 있는 economics 사용)
economics

a = ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line()
ggplotly(a)
b =ggplot(economics, aes(date, psavert)) +
  geom_line()
ggplotly(b)
##########################################################################################################################################
# box plot

ggplot(mpg, aes(drv, hwy)) +
  geom_boxplot(aes(fill = drv), outlier.color = 'red')

ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(fill = class), outlier.color = 'red')

##########################################################################################################################################
# exercise

# 1.
mpg_df  = mpg %>% 
  filter(class == c('compact', 'subcompact', 'suv')) %>% 
  select(class, cty)

ggplot(mpg_df, aes(class, cty)) +
  geom_boxplot(aes(fill=class), outlier.color = 'red')

library(plotly)
p = ggplot(mpg_df, aes(class, cty)) +
  geom_boxplot(aes(fill=class), outlier.color = 'red')
ggplotly(p)
