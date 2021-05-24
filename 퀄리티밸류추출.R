library(httr)
library(magrittr)
library(rvest)
library(dplyr)
library(progress)
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
symbol<-ticker$symbol%>%as.character()

#quality index 추리기
for(i in 1:nrow(ticker)){
  tryCatch({
  i=1
url<-paste0('https://finance.yahoo.com/quote/',symbol[i],'/key-statistics?p=',symbol[i])
profit_margin<-read_html(url)%>%html_nodes(xpath='
                       //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[2]/div/div/table/tbody/tr[1]/td[2]
                                 ')%>%
  html_text()

opm<-read_html(url)%>%html_nodes(xpath='
                       //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[2]/div/div/table/tbody/tr[2]/td[2]
                                 ')%>%
  html_text()
roa<-read_html(url)%>%html_nodes(xpath='
                       //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[3]/div/div/table/tbody/tr[1]/td[2]
                                 ')%>%
  html_text()

roe<-read_html(url)%>%html_nodes(xpath='
                      //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[3]/div/div/table/tbody/tr[2]/td[2]
                                 ')%>%
  html_text()

revenue_growth<-read_html(url)%>%html_nodes(xpath='
                      //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[3]/td[2]
                                 ')%>%
  html_text()
earning_growth<-read_html(url)%>%html_nodes(xpath='
                    //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[8]/td[2]
                                 ')%>%
  html_text()
'//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[8]/td[2]'

#자료 정리

quality<-rbind(profit_margin,opm,roa,roe,revenue_growth,earning_growth)
quality<-gsub('%','',quality)
quality<-gsub('N/A',NA,quality)

colnames(quality)<-symbol[i]
write.csv(quality,paste0('data/quality/',symbol[i],'_quality.csv'))
  },
  error=function(e){print(paste0("erroe in", symbol[i]))})

cat('\r',paste0("====",i,"/",nrow(ticker),"===="))
Sys.sleep(sample(1:3,1)*sample(1:2,1))
}
err<-list()
#value index 추리기
for(i in 1:nrow(ticker)){
  tryCatch({
url<-paste0('https://finance.yahoo.com/quote/',symbol[i],'/key-statistics?p=',symbol[i])
url
per<-read_html(url)%>%html_nodes(xpath='
                           //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[3]/td[2]')%>%
  html_text()
psr<-read_html(url)%>%html_nodes(xpath='
                          //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[6]/td[2]')%>%
  html_text()
pbr<-read_html(url)%>%html_nodes(xpath='
                         //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[7]/td[2]
                                 ')%>%
  html_text()

ev_revenue<-read_html(url)%>%html_nodes(xpath='
                         //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[8]/td[2]
                                 ')%>%
  html_text()
ev_ebitda<-read_html(url)%>%html_nodes(xpath='
                       //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table/tbody/tr[9]/td[2]
                                 ')%>%
  html_text()
value<-rbind(per,pbr,psr,ev_revenue,ev_ebitda)
value<-gsub('N/A',NA,value)
colnames(value)<-symbol[i]
value
write.csv(value,paste0('data/value/',symbol[i],'_value.csv'))
},
error=function(e){
  print(paste0("error in ",symbol[i]))
  err[[i]]<-symbol[i]
})
cat('\r',paste0("====",i,"/",nrow(ticker),"===="))
Sys.sleep(sample(10:20,1)*sample(1.5:3,1))
}

ql<-list()
for(i in 1:nrow(ticker)){
  if(file.exists(paste0('data/quality/',symbol[i],'_quality.csv'))==F){
    ql[[i]]<-symbol[i]
  }

  cat('\r',paste0("====",i,"/",nrow(ticker),"===="))
}
ql<-do.call(cbind,ql)
ql<-ql%>%as.character()
ql<-ql[!is.na(ql)]
symbol<-ql
symbol
#누락된거 체크
for(i in 1:length(ql)){
  tryCatch({
    url<-paste0('https://finance.yahoo.com/quote',symbol[i],'/key-statistics?p=',symbol[i])
    url
    profit_margin<-read_html(url)%>%html_nodes(xpath='
                       //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[2]/div/div/table/tbody/tr[1]/td[2]
                                 ')%>%
      html_text()
    
    opm<-read_html(url)%>%html_nodes(xpath='
                       //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[2]/div/div/table/tbody/tr[2]/td[2]
                                 ')%>%
      html_text()
    roa<-read_html(url)%>%html_nodes(xpath='
                       //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[3]/div/div/table/tbody/tr[1]/td[2]
                                 ')%>%
      html_text()
    
    roe<-read_html(url)%>%html_nodes(xpath='
                      //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[3]/div/div/table/tbody/tr[2]/td[2]
                                 ')%>%
      html_text()
    
    revenue_growth<-read_html(url)%>%html_nodes(xpath='
                      //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[3]/td[2]
                                 ')%>%
      html_text()
    earning_growth<-read_html(url)%>%html_nodes(xpath='
                    //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[8]/td[2]
                                 ')%>%
      html_text()
    '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[3]/div/div[4]/div/div/table/tbody/tr[8]/td[2]'
    
    #자료 정리
    
    quality<-rbind(profit_margin,opm,roa,roe,revenue_growth,earning_growth)
    quality<-gsub('%','',quality)
    quality<-gsub('N/A',NA,quality)
    
    colnames(quality)<-symbol[i]
    write.csv(quality,paste0('data/quality/',symbol[i],'_quality.csv'))
  },
  error=function(e){print(paste0("erroe in", symbol[i]))})
  
  cat('\r',paste0("====",i,"/",length(ql),"===="))
  Sys.sleep(sample(1:3,1)*sample(1:2,1))
}

#value 누락
vl<-list()
for(i in 1:nrow(ticker)){
  if(file.exists(paste0('data/value/',symbol[i],'_value.csv'))==F){
    vl[[i]]<-symbol[i]
  }
}
vl<-do.call(cbind,vl)
vl<-vl%>%as.character()
vl<-vl[!is.na(vl)]
vl
symbol<-vl
symbol
