library(rvest)
library(magrittr)

#1.1데이터 클렌징 캐피탈 색터 산업 추출 
ticker_nd<-read.csv('data/ticker/nasdaq.csv',row.names = 1)
ticker_nd%>%str
#etf제거
ticker_nd<-ticker_nd[ticker_nd$ETF=="N",]
ticker_nd<-ticker_nd[,1:2]
ticker_nd%>%head()
nrow(ticker_nd)
#크롤링
capital<-list()
sector<-list()
industry<-list()
name
for (i in 1:nrow(ticker_nd)) {
  name<-ticker_nd$Symbol[i]
  url1<-paste0('https://finance.yahoo.com/quote/',name,'?p=',name)
  url2<-paste0('https://finance.yahoo.com/quote/',name,'/profile?p=',name)
  tryCatch({
    c<-read_html(url1)%>%
      html_nodes(xpath='//*[@id="quote-summary"]/div[2]/table/tbody/tr[1]/td[2]/span')%>%html_text()
    s<-read_html(url2)%>%
      html_nodes(xpath='//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[2]')%>%html_text()
    ind<-read_html(url2)%>%
      html_nodes(xpath='//*//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[4]')%>%html_text()
    if(identical(c,character(0))){
      c<-NA
    }
    if(identical(s,character(0))){
      s<-NA
    }
    if(identical(ind,character(0))){
      ind<-NA
    }
  },error=function(e){
    c<-"error"
    s<-"error"
    ind<-"error"
    warning(paste0('error',name))
  })
  capital=rbind(capital,c)
  sector=rbind(sector,s)
  industry=rbind(industry,ind)
  cat(paste0("\r==== Progress: ", i, "/",nrow(ticker_nd),"===="))
  Sys.sleep(0.2) #잘 돌아가는지 확인용 코드가 있을 경우 sleep 삭제 후 작성
}
#크기확인
#직접 데이터 편집하였다.
ticker_nd<-read.csv('data/ticker/nasdaq.csv',row.names = 1)
ticker_nd<-ticker_nd[ticker_nd$ETF=='N',]
nrow(ticker_nd)
write.csv(ticker_nd,'data/ticker/ticker_nd.csv')
ticker_nd<-read.csv('data/ticker/ticker_nd.csv',row.names = 1)
ticker_ny<-read.csv('data/ticker/ticker_ny.csv',row.names = 1)
ticker_ny%>%head()
ticker_nd%>%head()
#ticker 통합
#전처리
ticker<-read.csv('data/ticker/ticker.csv')
ticker<-ticker[ticker$capital!="N/A",]
is.na(ticker$capital)
ticker<-ticker[!is.na(ticker$capital),]
ticker<-ticker[!is.na(ticker$symbol),]
write.csv(ticker,'data/ticker/ticker.csv')
ticker<-read.csv('data/ticker/ticker.csv')
ticker<-ticker[ticker$capital!="N/A",]
ticker<-ticker[ticker$sector!="N/A",]
ticker$industty
write.csv(ticker,'data/ticker/ticker.csv')
ticker
