library(dplyr)
library(magrittr)
Sys.setlocale("LC_ALL","English")
ticker<-read.csv('data/ticker/newticker.csv',row.names = 1,stringsAsFactors = F,encoding = 'UTF-8')
price<-read.csv('data/price_t.csv',row.names = 1,stringsAsFactors = F)

name=ticker$symbol
value_list=list()
for(i in 1:length(name)){
value<-read.csv(paste0('data/statics/',name[i],'.csv'),header=T,row.names = 1,stringsAsFactors = F, fileEncoding = "UTF-8", encoding = "UTF-8")
rownames(value)<-value[,1]
value<-value[row.names(value)%in%c('Operating Margin (ttm)','Return on Equity (ttm)','Quarterly Earnings Growth (yoy)','Quarterly Revenue Growth (yoy)'),]
value[,1]<-NULL
colnames(value)<-name[i]
value_list[[i]]<-value
}
l<-do.call(cbind,value_list)
l<-l[,-4693]
write.csv(l,'data/value_t.csv')
