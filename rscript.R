install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
head(mpg)
dim(mpg)
str(mpg)
summary(mpg)
View(mpg)
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy))
lm_mpg <- lm(data=mpg, hwy~displ) #종속~독립
summary(lm_mpg)
qplot(data=mpg, x=displ,y=hwy)
mean(mpg$hwy)
max(mpg$hwy)
hist(mpg$hwy)
a <- 1 #alt+-하면 화살표
a
b <- 2
b
c <- 3
c
ab <- 3.5
ab
a+b
a+b+c
4/b
d <- c(1,2,3,4,5)
e <- c(1:5)
e
d+2
d+e
a2 <- "a"
a2
b2 <- "text"
b2
c2 <- "Hello world!"
c2
d2<- c("a","b","c")
d2
e2 <- c("hello","world","is","good!")
e2
b2+2
a2+b2
paste(e2, collapse=" ")
e2_paste <- paste(e2, collapse=" ")
e2_paste
e3_paste <- paste(e2, collapse=",")
e3_paste
b <- c("a","a","b","c")
b
qplot(b)
library(ggplot2)
qplot(data=mpg,x=hwy)
qplot(data=mpg,x=cty)
qplot(data=mpg,y=hwy,x=drv,geom="boxplot")
qplot(data=mpg,y=hwy,x=drv,geom="boxplot",colour=drv)
?qplot
qplot(mpg,wt,data=mtcars)

history <- c(90,80,60,70)
history
math <- c(50,60,100,20)
math
df_midterm <- data.frame(history,math)
df_midterm
class <- c(1,1,2,2)
class
df_midterm <- data.frame(history,math,class)
df_midterm
mean(df_midterm$history)
mean(df_midterm$math)
df_midterm <- data.frame(history=c(90,80,60,70),
                         math=c(50,60,100,20),
                         class=c(1,1,2,2))
df_midterm
install.packages("readxl")
library(readxl)
df_finalexam <- read_excel("finalexam.xlsx",sheet=1,col_names=T)#첫번째 행을 col_name으로 지정?=>True
df_finalexam
mean(df_finalexam$math)
exam <- read.csv("csv_exam.csv",header=T)
exam
write.csv(df_finalexam,file='output_newdata.csv')
head(exam)#앞에 6개
head(exam,10)
tail(exam)
tail(exam,10)
View(exam)
dim(exam)# 행 열
str(exam)
summary(exam)
mpg <- ggplot2::mpg
mpg
head(mpg)
tail(mpg)
dim(mpg)
str(mpg)
summary(mpg)
