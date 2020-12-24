df_raw <- data.frame(var1=c(1,2,1),var2=c(2,3,2))
df_raw
df_new <- df_raw #복사본 백업
df_new
df_new <- rename(df_new,v2=var2)
df_new
#문제1
mpg_raw <- ggplot2::mpg
mpg_raw
mpg_new <- mpg_raw
mpg_new
mpg_new <- rename(mpg_new,city=cty,highway=hwy)
mpg_new

df <- data.frame(var1=c(4,8,6),var2=c(2,6,1))
df
df$var_sum <- df$var1+df$var2
df
df$var_mean <- df$var_sum/2
df

mpg$total <- (mpg$cty+mpg$hwy)/2
head(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)
mpg$test <- ifelse(mpg$total>=20,"pass","fail")
head(mpg,20)
table(mpg$test) # 빈도
library(ggplot2)
qplot(mpg$test)
mpg$grade <- ifelse(mpg$total>=30,"A",ifelse(mpg$total>=20,"B","C"))
table(mpg$grade)
qplot(mpg$grade)

#문제2
midwest_new <- ggplot2::midwest
head(midwest_new)
midwest_new <- rename(midwest_new,total=poptotal,asian=popasian)
midwest_new
midwest_new$asirate <- midwest_new$asian/midwest_new$total*100
summary(midwest_new$asirate)
hist(midwest_new$asirate)
midwest_new$asigrade <- ifelse(midwest_new$asirate> mean(midwest_new$asirate),"large","small")
midwest_new
midwest_new$asirate
midwest_new$asigrade
table(midwest_new$asirate)
table(midwest_new$asigrade)
qplot(midwest_new$asirate)
qplot(midwest_new$asigrade)
exam <- read.csv("csv_exam.csv")
exam
exam %>% filter(class==1) #ctrl+shift+m
exam %>% filter(class!=1)
exam %>% filter(class==1&math>50)
exam %>% filter(math>=90|english>50)
exam %>% filter(class==1|class==3|class==5)
exam %>% filter(class %in% c(1,3,5))

exam_c1 <- exam %>% filter(class==1)
mean(exam_c1$math)
exam_c2 <- exam %>% filter(class==2)
mean(exam_c2$math)

#문제3
mpg <- ggplot2::mpg
#Q1
displ_4 <- mpg %>% filter(mpg$displ<=4)
mean(displ_4$hwy)
displ_5 <- mpg %>% filter(mpg$displ>=5)
mean(displ_5$hwy)
#Q2
audi <- mpg %>% filter(mpg$manufacturer=="audi")
mean(audi$cty)
toyota <- mpg %>% filter(mpg$manufacturer=="toyota")
mean(toyota$cty)
hwy_cfh <- mpg %>% filter(mpg$manufacturer %in% c("chevrolet","ford","honda"))
mean(hwy_cfh$hwy)

exam %>% select(math)
exam %>% select(class,math,english)
exam %>% select(-math,-english)
exam %>% 
  filter(class==1) %>% 
  select(english) %>% 
  head(10)#파이프-앞에 결과 뒤에 전달

#문제4
#Q1
mpg %>% select(class,cty)
#Q2
suv <- mpg %>% filter(mpg$class=="suv")
compact <- mpg %>% filter(mpg$class=="compact")
mean(suv$cty)
mean(compact$cty)

exam %>% arrange(math)
exam %>% arrange(desc(math))

#문제5
mpg %>% 
  filter(mpg$manufacturer=="audi")%>%
  arrange(desc(hwy)) %>%
  head(5)

exam %>% 
  mutate(total=math+english+science,
         mean=total/3) %>% 
  arrange(total)#$기호없음
exam %>% 
  mutate(test=ifelse(science >+60, "pass","fail"))

#문제6
mpg_new <- ggplot2::mpg
mpg_new <- mpg_new %>% mutate(total=cty+hwy)
mpg_new <- mpg_new %>% mutate(avg=total/2)
mpg_new %>%
  arrange(desc(avg)) %>%
  head(3)
#Q4
mpg %>% 
  mutate(total=cty+hwy,
         avg=total/2) %>% 
  arrange(desc(avg)) %>% 
  head(3)

exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            n=n())#빈도-학생수

#문제
#Q1
mpg %>% group_by(class) %>% 
  summarise(city_avg=mean(cty))
#Q2
mpg %>% group_by(class) %>% 
  summarise(city_avg=mean(cty)) %>% 
  arrange(desc(city_avg))
#Q3
mpg %>% group_by(manufacturer) %>% 
  summarise(hwy_avg=mean(hwy)) %>% 
  arrange(desc(hwy_avg)) %>% 
  select(manufacturer) %>% 
  head(3)
#Q4
mpg %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()+
  xlim(3,6)+ylim(10,30)

#Q2
ggplot(data=midwest,aes(x=poptotal,y=popasian))+
  geom_point()+
  xlim(0,500000)+ylim(0,10000)

ggplot(data=economics,aes(x=date,y=unemploy))+geom_line()

#
ggplot(data=economics,aes(x=date,y=psavert))+geom_line()

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))
df_mpg

ggplot(data=df_mpg,aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+geom_col()

#
mpg_1 <- mpg %>% 
  filter(class=="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)
mpg_1
ggplot(data=mpg_1,aes(x=reorder(manufacturer,-mean_cty),y=mean_cty))+geom_col()
ggplot(data=mpg,aes(x=drv,y=hwy))+geom_boxplot()

#
mpg <- mpg %>% 
  filter(class %in% c("compact","subcompact","suv"))
ggplot(data=mpg,aes(x=class,y=cty))+geom_boxplot()
#subcompact는 연비가 낮~높 다양
#suv는 subcompact보다 연비가 낮은데 어떤 차는 subcompact의 중간값보다 큰 값을 가진다.

