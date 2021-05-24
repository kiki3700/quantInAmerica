#단순 수익률 계산기
library(quantmod)
library(xts)
library(magrittr)
port<-read.csv('data/stra/port_nq.csv',row.names = 1)
symbol<-port$symbol%>%as.character()%>%c()
symbol%>%str
getSymbols(symbol,scr='yahoo',from='2020-10-26',to=Sys.Date())
