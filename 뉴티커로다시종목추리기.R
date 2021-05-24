#symbol정리
library(dplyr)
library(magrittr)
setwd('C:/Users/55754/Documents/야후파이낸스 재무재표')
ticker<-read.csv('data/ticker/newticker.csv',row.names = 1,stringsAsFactors = F)
ticker
name<-ticker$symbol%>%as.character()
valuelist<-list()
for(i in 1:length(name)){
value=read.csv(paste0('data/statics/',name[i],'.csv'),row.names = 1,encoding = 'UTF-8')
value=value[value$index%in%
c('Return on Assets (ttm)',
'Return on Equity (ttm)',
'Quarterly Revenue Growth (yoy)',
'Quarterly Earnings Growth (yoy)'),]
rownames(value)<-value$index
value[,1]=NULL
colnames(value)<-name[i]
value_list[[i]]<-value
}
