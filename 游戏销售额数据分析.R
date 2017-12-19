#案例数据来源 知乎用户:R语言小学生
#某手游6-7月数据分析
#读取数据
## log_date用户登录时间 / DAU (Daily Active User)日活跃用户数量
DAU <- read.csv(stringsAsFactors = F,file = "F:\\R\\手游分析\\销售额\\dau.csv",header=T)[,c(1,3)]
#stringsAsFactors = F,指不需要将读取数据转化为因子
head(DAU)
#用户消费时间和消费金额
DPU <- read.csv(stringsAsFactors = F,file = "F:\\R\\手游分析\\销售额\\dpu.csv",header=T)[,-2]
head(DPU)
#INS 用户初次安装应用的时间
INS <- read.csv(stringsAsFactors = F,file = "F:\\R\\手游分析\\销售额\\install.csv",header=T)[,-2]
head(INS)
DID0 <-merge(x = INS,y =DAU ,by = "user_id",all.y=T)  #合并初次安装时间和登录时间
length(DID0$install_date) #all.x为146569 ,all.y为139112。差值代表有些玩家安装后没有玩,我们只取有登录玩家
head(DID0)

DID <- merge(x = DID0,y = DPU,by=c("log_date","user_id"),all.x = T) #将DPU合并进去
DID$payment[is.na(DID$payment)] <- 0   #原始数据没有产生消费金额时payment为缺失值,设为0
head(DID)

#登录时间和安装时间都只保留月份
DID$log_date <- substr(x = DID$log_date,start = 6,7)
DID$install_date <- substr(x = DID$install_date,start = 6,stop = 7)
head(DID)

#按月份整合数据
DID$log_date <- as.numeric(DID$log_date)
DID$install_date <- as.numeric(DID$install_date)
final.data <- aggregate(x = DID,by = list(DID$user_id,DID$log_date,DID$install_date),FUN = sum)
#final.data <- aggregate(x=DID,by=list(DID$user_id),FUN = sum)
head(final.data)
tail(final.data)
final.data <- final.data[c(1,2,3,7)]
names(final.data) <- c('user.id','log_date','install_date','payment')

#判断新老用户
final.data$label <- ifelse(final.data$log_date==final.data$install_date,'新用户消费','老用户消费')

#计算盈利
final.sum <- aggregate(x = final.data[,2:4],by=list(final.data$log_date,final.data$label),sum)
#str(final.sum)
head(final.sum)
final.sum <- final.sum[c(1,2,5)]
names(final.sum) <- c('月份','收入来源','销售额')


#制作图表
library(ggplot2)
#制作条形图
p <-ggplot(data = final.sum,aes(x=factor(月份),y=销售额,fill=收入来源))+  
  geom_bar(stat = 'identity')+
  labs(x='月份',y='销售额')+
  scale_x_discrete(breaks = c(6,7),labels = c('6月','7月'))
p
#修改刻度标签的文本.在需要设置刻度标签的地方同时为breaks和labels赋值即可
  #geom_text(aes(label=final.sum$销售额),size=3.4,vjust=1)

#制作饼图
p+coord_polar(theta = 'y')+theme(axis.text = element_blank(),axis.ticks = element_blank())
 #axis.text 坐标值 ，axis.ticks 坐标刻度

#对比详细充值金额
ggplot(data = final.data,aes(x = payment,y=..count..))+geom_bar(stat = 'count')

library(magrittr)  #管道函数使用
#新老客户和是否消费有无关系
final.data$if_pay <- ifelse(final.data$payment==0,0,1)
table(final.data[,c(5,6)])%T>% 
{print(chisq.test(.))} %>%
  data.frame() %>%
  ggplot(aes(label,Freq,fill=if_pay))+geom_bar(stat = 'identity',position = 'dodge')
         
