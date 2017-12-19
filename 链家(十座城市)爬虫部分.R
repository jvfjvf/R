library(rvest)#加载rvest包,用于网页爬虫
library(stringr)
url0 <- "https://gz.fang.lianjia.com/loupan/"  #目标为链家广州楼盘
city <- c('gz','sz','bj','cd','xm','tj','jn','hz','zz','nj')     
#广州,深圳,北京,成都,厦门,天津,济南,杭州,郑州,南京
#提取楼盘名字，面积，价格，类型，地址，状态
name=area=price=type=address=status=NULL 
for (j in 1:length(city)) {         #length(city)
for (i in 1:5)   #爬取前5页
{
  url <- paste("https://",city[j],".fang.lianjia.com/loupan/","pg",i,sep = '')  #观察到网址连接字为pg,i为1-10
  web <-read_html(url)
  # name <-c(name,web%>% html_nodes('div.info-panel')%>%html_nodes('a')%>% html_text())#数组形式！！才能提取到1-10页
  name <- c(name,web%>%html_nodes("div.col-1")%>%html_nodes("a")%>%html_text())#楼盘名称(只能提取到第一页)
  area <- c(area,web%>%html_nodes("div.col-1>div.area>span")%>%html_text())  #面积
  price <- c(price,web%>%html_nodes("div.col-2>div.price>div.average")%>%html_text()) #>span.num
  address <- c(address,web%>%html_nodes("div.col-1")%>%html_nodes("div.where")%>%html_text())#楼盘地点
  type <- c(type,web%>%html_nodes("div.col-1>div.type>span.live")%>%html_text())
  
  #对面积area 数据处理
  #area <-sub(pattern = "建面",replacement = '',x = area)
  #area <-sub(pattern = "m²",replacement = '',x = area)
  #string包中的str_match函数可提取出数字，正则表达式为"[0-9]+~[0-9]+|[0-9]+"
  area  <-str_match(string = area,pattern = "[0-9]+~[0-9]+|[0-9]+")
  #
  
  #对价格进行处理，有些价格未定
 # price<-gsub(pattern = '万',replacement='0000',price)
  price<-gsub(pattern = "\n",replacement = '',price)
  price<-gsub(pattern = '\t',replacement = "",price)
  price<-str_match(string = price,pattern = "[0-9]+|价格待定")
  
}
}

data =data.frame(楼盘名=name,性质=type,价格=price,面积=area,地址=address)
DT::datatable(data)
length(price)
data_gz <- data[1:50,]
data_sz <- data[51:100,]
data_bj <- data[101:150,]
data_cd <- data[151:200,]
data_xm <- data[201:250,]
data_tj <- data[251:300,]
data_jn <- data[301:350,]
data_hz <- data[351:400,]
data_zz <- data[401:450,]
data_nj <- data[451:500,]
data<-list(广州=data_gz,深圳=data_sz,北京=data_bj,成都=data_cd,厦门=data_xm,天津=data_tj
             ,济南=data_jn,杭州=data_hz,郑州=data_zz,南京=data_nj)
write.table(x = data,file = "F:\\R\\链家数据分析\\链家(十座城市).csv",sep = ",",row.names = F)
#保存为csv格式
save(data,data_gz,data_sz,data_bj,data_cd,data_xm,data_tj,data_san,data_hz,data_zz,data_nj,file="F:\\R\\链家数据分析\\爬虫(10座城市).rdata")

