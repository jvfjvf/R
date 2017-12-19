##案例参考: 知乎用户:R语言小学生
library(ggplot2)
library(magrittr)  #使用管道函数
DAU <- read.csv(file = "F:\\R\\手游分析\\手游热度分析(列联表及卡方检验)\\用户8-9月登陆数据.csv",header=T)[,-2]
USER <- read.csv(file ="F:\\R\\手游分析\\手游热度分析(列联表及卡方检验)\\用户个人信息库.csv",header=T )[,-2]
dau.user <- merge(x = DAU,y = USER,by ='user_id',all.x = T)
head(dau.user)
dau.user$log_month <- substr(x = dau.user$log_date,start = 6,stop = 6)
class(dau.user$log_month)

#进行卡方检验并画出图观察(登录次数是否受性别影响)
table(dau.user$log_month,dau.user$gender) %T>%  #计算频数
{print(chisq.test(.))} %>%        #卡方检验
  data.frame()    %>%                  #转化成数据框
  ggplot(aes(Var1,Freq,fill=Var2)) +
  geom_bar(stat = 'identity',position = 'fill')+
  labs(x='月份',y='频率')
#卡方中p远大于0.05,所以拒绝原假设,并从图中也可看出,登录次数与性别无关

#登录次数是否受年龄影响
(P2 <- table(dau.user$log_month,dau.user$generation))%T>%
{print(chisq.test(.))}  %>%
  data.frame() %>%
  ggplot(aes(Var1,Freq,fill=Var2))+geom_bar(stat = 'identity',position = 'fill')
(P2[1,]-P2[2,])/P2[1,]  #看看每个年龄段下降的幅度
P2[,4]/rowSums(P2)     #查看40岁年年龄段玩家占比
chisq.test(x = P2[,-4])  #这次P值大于0.05,不拒绝原假设,认为登录次数减少与年龄层次无关

#分析登录次数是否受设备影响
table(dau.user$log_month,dau.user$device_type)%T>%
{print(chisq.test(.))}  %>%
  data.frame()   %>%
  ggplot(aes(Var1,Freq,fill=Var2))+geom_bar(stat='identity')
#p远小于0.05,且图形显示安卓用户明显下降,进行深度分析

table(dau.user[,c(2,6)]) %>%data.frame()  %>%
  ggplot(aes(as.Date(log_date),Freq,shape=device_type,color=device_type))+
  geom_point()+geom_line()

