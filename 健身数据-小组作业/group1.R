library(readxl)
e1<-read_excel("F:\\R\\exercise_in_xian.xlsx")


#删除有效期、规格中的缺省值
e2=subset(e1,e1$规格 != 'NA')
e2=subset(e2,e2$有效期 != 'NA')

attach(e2)
#用平均数代替评分中的na
install.packages('psych')
library(psych)
describe(e2$评分)
e2$评分[is.na(e2$评分)]=4.69

#用0代替评论数中的na
e2$评论数[is.na(e2$评论数)]=0


#用中位数代替价格中的na（极端值太多，平均值过高）
describe(e2$`团购价格(元)`)
e2$`团购价格(元)`[is.na(e2$`团购价格(元)`)]=188


#起始与终止时间格式化
e2$start=substr(e2$有效期,1,11)
e2$end=substr(e2$有效期,13,23)
install.packages("stringr")
library(stringr)
e2$start=str_replace(e2$start, "年", "-")
e2$start=str_replace(e2$start, "月", "-")
e2$start=str_replace(e2$start, "日", "")

e2$end=str_replace(e2$end, "年", "-")
e2$end=str_replace(e2$end, "月", "-")
e2$end=str_replace(e2$end, "日", "")

e2$end=as.Date(e2$end)
e2$start=as.Date(e2$start)

#起始与终止季度
e2$sq=quarters(e2$start)
e2$eq=quarters(e2$end)
e2$sqn=substr(e2$sq,2,2)
e2$eqn=substr(e2$eq,2,2)


#求出经历时间（duration）
e2$dura=e2$end-e2$start
e2$dura=as.numeric(e2$dura)

e2$q1=0
e2$q2=0
e2$q3=0
e2$q4=0

#获取年份
library(lubridate)
e2$ys=year(e2$start)
e2$ys=as.numeric(e2$ys)
e2$yn=year(e2$end)
e2$yn=as.numeric(e2$yn)

#对于跨一年的，其经历的季度为开始季度到第一年第四季度，再加上第二年初到结束季度
e2$q1[e2$sqn==1 & e2$ys==e2$yn-1]=1
e2$q2[e2$sqn<=2 & e2$ys==e2$yn-1]=1
e2$q3[e2$sqn<=3 & e2$ys==e2$yn-1]=1
e2$q4[e2$sqn<=4 & e2$ys==e2$yn-1]=1
e2$q1[e2$eqn>=1 & e2$ys==e2$yn-1]=1
e2$q2[e2$eqn>=2 & e2$ys==e2$yn-1]=1
e2$q3[e2$eqn>=3 & e2$ys==e2$yn-1]=1
e2$q4[e2$eqn>=4 & e2$ys==e2$yn-1]=1

#跨两年及以上的，四个季度都经历
e2$q1[e2$yn-e2$ys>=2]=1
e2$q2[e2$yn-e2$ys>=2]=1
e2$q3[e2$yn-e2$ys>=2]=1
e2$q4[e2$yn-e2$ys>=2]=1

#同一年的，经历季度为开始季度到结束季度
e2$q1[e2$sqn==1 & e2$ys==e2$yn]=1
e2$q2[e2$sqn<=2 & e2$eqn>=2 & e2$ys==e2$yn]=1
e2$q3[e2$sqn<=3 & e2$eqn>=3 & e2$ys==e2$yn]=1
e2$q4[e2$eqn==4 & e2$ys==e2$yn]=1

#图表分析
table(e2$q3)
