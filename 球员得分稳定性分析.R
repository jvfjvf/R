#NBA球员稳定性分析
library(reshape2)
player<- read.csv(file = "F:\\R\\ggplot图\\NBA\\球员\\球员分析.csv",header=T,sep=',')
#for (i in 1:length(player)) {
 # average[i] <- mean(player[[i]])        #求每个球员平均值
#}
  name <-colnames(player)           #得到球员姓名
 #求球员下行标准差
  player0=as.data.frame(player0)
  average=low_sd=NULL
  #player0=NULL
  for (i in 1:length(player)) {  
   for (j in 1:length(player[[i]])) {
     average[i] <- mean(player[[i]])        #求每个球员平均值
     #挑选出球员低于平均分的场得分
     player0[j,i] <-   ifelse(player[j,i]<average[i],player[j,i],average[i])  
     #注意j,i的设置,分别代表行和列
     low_sd[i] <-sd(x = player0[,i])  #求下行标准差
      }}
   player_ana <- data.frame(球员=colnames(player),场均得分=average,下行标准差=low_sd)
  
#制作散点图
   library(ggplot2)
   library(ggthemes)  #可新增gpplot2包的不同主题
   ggplot(data = player_ana,aes(场均得分,下行标准差,factor(场均得分)))+
     geom_point(shape=16,color='red')+       #
     geom_text(aes(label=球员),vjust=1.5,hjust=0.5,size=3)+
    # geom_vline(mean(下行标准差)) 
     geom_vline(xintercept=mean(player_ana$场均得分),linetype=3,color='blue')+
     geom_hline(yintercept=mean(player_ana$下行标准差),linetype=2,color='blue')+
     theme(panel.grid = element_blank(),plot.background = element_rect(fill = 'skyblue'),panel.background = element_rect(fill = 'pink'))  #删除网格线
     #theme(panel.background = element_rect(fill='transparent',color = 'blue'),axis.text=element_text(color = 'red'))
      #theme_few()  ggthemes包中主题
