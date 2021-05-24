ticker_n
s<-NULL
ind<-NULL
sector<-list()
industry<-list()
for( i in 1:nrow(ticker_n)){
  name<-ticker_n$Symbol[i]
  url<-paste0('https://finance.yahoo.com/quote/',name,'/profile?p=',name)
  tryCatch({
    s<-read_html(url)%>%
      html_nodes(xpath='//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[2]')%>%html_text()
    ind<-read_html(url)%>%
      html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[4]')%>%html_text()
  },error=function(e){
    s<<-NA
    ind<<-NA
  })
  write.csv(s,paste0('data/ticker/sector/',name,'_s.csv'))
  write.csv(ind,paste0('data/ticker/industry/',name,'_ind.csv'))
  cat(paste0("\r==== Progress: ", i, "/",nrow(ticker_n),"===="))
  Sys.sleep(0.2) #잘 돌아가는지 확인용 코드가 있을 경우 sleep 삭제 후 작성
}

identical(character(0)|"1",character(0))
warnings()
library(magrittr)
sector<-list()
s<-NULL
name<-NULL
for(i in 1:100){
  tryCatch({name<-ticker_n$Symbol[i]
  s<-read.csv(paste0('data/ticker/sector/',name,'_s.csv'),row.names = 1)
  s<-cbind(name,s)},
  error=function(e){
    s<-cbind(name,"NA")
    warning(paste0(name))
  })
  sector<-rbind(sector,s)
}
sector<-list()
s<-NULL
name<-NULL
for(i in 1:nrow(ticker_n)){
 symbol<<-ticker_n$Symbol[i]
 s<-read.csv(paste0('data/ticker/sector/',symbol,'_s.csv'),row.names = 1)
 names(symbol)<-'name'
 names(s)<-'n'
 if(is.na(s[1,1])){
   s<-'NA'
   names(symbol)<-'name'
   names(s)<-'n'
 }
 names(symbol)<-'name'
 names(s)<-'n'
  s<-cbind(symbol,s)
  sector<-rbind(sector,s)
}
symbol<-ticker_n$Symbol[3186]
s<-read.csv(paste0('data/ticker/sector/',symbol,'_s.csv'),row.names = 1)
if(is.na(s[1,1])){
  s<-'NA'
  s<-cbind(symbol,s)
}
names(s)
names(name)
name<-ticker_n$Symbol[3186]
s<-cbind(ticker_n$Symbol[3186],s)
s
sector<-rbind(sector,s)
ticker_n[ticker_n$Symbol=='YAC.WS',]
