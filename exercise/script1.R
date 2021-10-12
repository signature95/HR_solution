library(ggplot2)
x <- c('a','b','c','a')
qplot(x)
x <- c(90,80,60,70)
mean_x <-mean(x)

qplot(data = mpg, x = hwy)
qplot(data = mpg, x = drv, y = hwy, geom='boxplot', colour = drv)
english <- c(90,80,60,70)
english
math <- c(50,60,100,20)
math
df_midterm <- data.frame(english,math)
df_midterm

