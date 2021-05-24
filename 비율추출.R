library(httr)
library(rvest)
library(readr)
library(tidyquant)
library(stringr)
library(quantmod)
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
err<-list()
nrow(ticker)
#가치지표 퀄리티지표 뽑기
for(i in 1:nrow(ticker)){
  name<-ticker$symbol[i]
url<-paste0('https://finance.yahoo.com/quote/',name,'/key-statistics?p=',name)
tryCatch({
per<-read_html(url)%>%
  html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[3]/td[2]')%>%
  html_text()
pbr<-read_html(url)%>%
  html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[7]/td[2]')%>%
  html_text()
roe<-read_html(url)%>%
  html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[3]/div/div/table/tbody/tr[2]/td[2]')%>%
  html_text()
roe<-gsub('%','',roe)
revenue_growth<-read_html(url)%>%
  html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[3]/td[2]')%>%
  html_text()
revenue_growth<-gsub('%','',revenue_growth)
value<-data.frame(c(per,pbr,roe,revenue_growth))
colnames(value)<-name
rownames(value)<-c('per','pbr','roe','revenue_growth')
write.csv(value,paste0('data/value/',name,'_value.csv'))
},error=function(e){
  err<-append(err,name)
  warning(paste0('error in ',name))
})
cat(paste0("\r==== Progress: ", i, "/",length(ticker$symbol),"===="))
Sys.sleep(0.3) #잘 돌아가는지 확인용 코드가 있을 경우 sleep 삭제 후 작성
}
warnings()
#안된거 다시 해보기=> 다 되버렸다.!!
uncomple_value<-c('BSQR','CPSS','ERIE','PFSW','ARE','ARCH','AIZ','COG','SFRW')
write.csv(uncomple_value,'data/uncomple.csv')
for(i in 1){
  name<-"SFTW"
  url<-paste0('https://finance.yahoo.com/quote/',name,'/key-statistics?p=',name)
  tryCatch({
    per<-read_html(url)%>%
      html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[3]/td[2]')%>%
      html_text()
    pbr<-read_html(url)%>%
      html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[7]/td[2]')%>%
      html_text()
    roe<-read_html(url)%>%
      html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[3]/div/div/table/tbody/tr[2]/td[2]')%>%
      html_text()
    roe<-gsub('%','',roe)
    revenue_growth<-read_html(url)%>%
      html_node(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[3]/td[2]')%>%
      html_text()
    revenue_growth<-gsub('%','',revenue_growth)
    value<-data.frame(c(per,pbr,roe,revenue_growth))
    colnames(value)<-name
    rownames(value)<-c('per','pbr','roe','revenue_growth')
    write.csv(value,paste0('data/value/',name,'_value.csv'))
  },error=function(e){
    err<-append(err,name)
    warning(paste0('error in ',name))
  })
  cat(paste0("\r==== Progress: ", i, "/",length(ticker$symbol),"===="))
  Sys.sleep(0.3) #잘 돌아가는지 확인용 코드가 있을 경우 sleep 삭제 후 작성
}
value_list<-list()
value_list[[]]
#통합해야합니다.~
value_list<-list()
for(i in 4220:nrow(ticker)){
  name<-ticker$symbol[i]
  value<-read.csv(paste0('data/value/',name,'_value.csv'),row.names = 1)
  value_list[[i]]<-value%>%t()
  cat(paste0("\r==== Progress: ", i, "/",length(ticker$symbol),"===="))
  Sys.sleep(0.3) #잘 돌아가는지 확인용 코드가 있을 경우 sleep 삭제 후 작성
}
value_list[[4220]]%>%colnames()
value_t = do.call(rbind, value_list) %>% na.locf()
value_t
write.csv(value_t,'data/value_list.csv')
value<-read.csv('data/')