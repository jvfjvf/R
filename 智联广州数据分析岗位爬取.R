library(rvest)
library(stringr)
library(rJava)
library(xlsx)
url1='http://sou.zhaopin.com/jobs/searchresult.ashx?jl=%E5%B9%BF%E5%B7%9E&kw=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&p=1&isadv=0'
zhiwei=company=salary=adress=time=zwhref=NULL
for (i in 1:10) {
  url=paste(url1,i,sep = '')
  web=read_html(url)
  zhiwei1=web%>%html_nodes('td.zwmc')%>%html_nodes('a')%>%html_text()
  #head(zhiwei1)
  zhiwei=c(zhiwei,zhiwei1[str_length(zhiwei1)!=0])  #职位名称
  length(zhiwei)
  company1=web%>%html_nodes('td.gsmc')%>%html_nodes('a')%>%html_text()
  #head(company1)
  company=c(company,company1[str_length(company1)!=0])  #公司名称
  #head(company)
  salary=web%>%html_nodes('td.zwyx')%>%html_text()    #工资
  #head(salary)
  adress=web%>%html_nodes('td.gzdd')%>%html_text()     #地址
  #head(adress)
  time=web%>%html_nodes('td.gxsj')%>%html_text()       #发布时间
  #head(time)
  zhiwei_href=web%>%html_nodes("td.zwmc")%>%html_nodes('a')%>%html_attr('href')
  #head(zhiwei_href)                               
  zwhref=c(zwhref,zhiwei_href[str_length(zhiwei_href)>40])  #职位明细地址
  #head(zwhref)
}
zhilian=data.frame(职位=zhiwei,公司=company,月薪=salary,地址=adress,时间=time,链接=zwhref)
DT::datatable(zhilian)

 #子页???(职位详情)
yuexin=didian=shijian=xingzhi=jingyan=xueli=num=leibie=guimo=leixing=hangye=xxt1=xxt2=NULL
xnn=length(zwhref)
for (i in 1:xnn) {
  url=zwhref[i]
  hweb=read_html(url)
  txt=hweb%>%html_nodes('ul.terminal-ul>li>strong')%>%html_text()
  #head(txt)
  yuexin=c(yuexin,str_trim(txt[1]))
  didian=c(didian,txt[2])
  shijian=c(shijian,txt[3])
  xingzhi=c(xingzhi,txt[4])
  jingyan=c(jingyan,txt[5])
  xueli=c(xueli,txt[6])
  num=c(num,txt[7])
  leibie=c(leibie,txt[8])
  guimo=c(guimo,txt[9])
  leixing=c(leixing,txt[10])
  hangye=c(hangye,txt[11])
  xxt=hweb%>%html_nodes('div.tab-cont-box>div')%>%html_text()
  xxt1=c(xxt1,str_trim(strsplit(str_trim(xxt)[1],'\\r\\n')[1][1]))
  xxt2=c(xxt2,str_trim(strsplit(str_trim(xxt)[1],'\\r\\n')[1][6]))
  #head(xxt2)
  #length(xxt2)

  }
zhilian1=data.frame(职位=zhiwei,公司=company,职位月薪=yuexin,工作地点=didian,发布时间=shijian,工作性质=xingzhi,最低学历=xueli,招聘人数=num,职位类别=leibie,公司规模=guimo,公司性质=leixing,公司行业=hangye,超链接=zwhref)
DT::datatable(zhilian1) 
write.csv(zhilian1,'F:\\1\\智联广州')
write.xlsx(zhilian1,'F:\\R\\智联')
