#字符串处理、汇总数据
library(plyr)
library(stringr)
library(sqldf)
#爬虫相关
library(RCurl)
library(XML)
library(rvest)
#读取数据
library(data.table)
#数据可视化
library(ggplot2)
library(ggthemes)
library(ggradar)
#markdown 文件生成
library(knitr)
library(rmarkdown)

#爬取诊股排行榜股票代码
url <- 'http://doctor.10jqka.com.cn/'
web <- read_html(url)
point <- web%>%html_nodes("div.inner")%>%html_nodes('span')%>%html_nodes('a.s_name')%>%html_text
point <- str_match(string = point,pattern = "[0-9]{1,6}")  #提取文本中为6个数的数字
point <- as.data.frame(point)           #转化成数据框
point <- na.omit(point)                 #删除缺失值
point$V1 <-as.character(point$V1)      #得到五个当天热门的股票代码
#as.numeric()的话,002419等股票只显示2419

#利用股票代码跳转至个股分析界面
technical=funds=message=trade=basic=level=0
for (i in 1:5) {
  url0 <-paste(url,point$V1[i],'/',sep = '')
  temp <- getURL(url0,.encoding = 'UTF-8')
  doc <- htmlParse(temp)
  points <- getNodeSet(doc,'//div[@class="chart_base"]/div[@class="column_3d"]/div[@class="label"]')
  points <- sapply(points,xmlValue)
   
  technical[i] <- as.numeric(substr(points[1],1,3))
  funds[i]     <- as.numeric(substr(points[2],1,3))
  message[i]   <- as.numeric(substr(points[3],1,3))
  trade[i]     <- as.numeric(substr(points[4],1,3))
  basic[i]     <- as.numeric(substr(points[5],1,3))
  #level[i]     <- sapply(getNodeSet(doc,'//span[@class="cur"]'),xmlValue)
  level[i]     <-  read_html(url0)%>%html_nodes("div.value_bar")%>%html_nodes("li>span.cur")%>%html_text()
  }
point <-data.frame(股票代码=point$V1,技术面=technical,资金面=funds,消息面=message,行业面=trade,基本面=basic,level)
#write.csv(x = point,file = "F:\\R\\爬虫\\同花顺股票可视化\\1.csv")
DT::datatable(point)
