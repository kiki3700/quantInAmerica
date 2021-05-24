#티커 크롤링
library(httr)
library(rvest)
library(readr)
library(tidyquant)
library(stringr)
library(quantmod)
url<-'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
ticker<-url%>%read_html()%>%html_node(xpath='//*[@id="constituents"]')%>%html_table()
ticker%>%head
symbol<-ticker$Symbol
symbol
for (i in 1:length(symbol)) {
  symbol[i]
  getSymbols(symbol[i])
  price<-get(symbol[i])
  close<-price[,4]
  close<-close%>%as.data.frame()
  close<-cbind(index(close),close)
  colnames(close)<-c('date','price')
  write.csv(close,paste0('data/price/',symbol[i],'.csv'))
}
#오류 넘기는 기능 필요요