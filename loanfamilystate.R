library(readxl)
data <- read_excel("Koweps2014.xlsx",sheet=1,col_names=T)
head(data)
data_new <- data %>% select("금융기관대출액","가정불화")
data_new
data_new <- rename(data_new,loan=금융기관대출액,f_state=가정불화)
head(data_new,10)
data_new <- data_new %>% filter(loan>0)
summary(data_new$loan)

data_new$loangrade <- ifelse(data_new$loan>=8950,"D",ifelse(data_new$loan>=6900,"C",ifelse(data_new$loan>=3900,"B","A")))


head(data_new)
data_new %>% group_by(data_new$loangrade)
ggplot(data=data_new,aes(x=loangrade,y=f_state))+geom_col()
#분석:금융기관대출액이 상위25%인 8950만원이상인 가정에서 가정불화를 가장 많이 느낀다고 답하였다. 하지만 금융기관대출액이 하위25%인 3900만원이하인 그룹에서도 가정불화 비율이 높게 나왔다.