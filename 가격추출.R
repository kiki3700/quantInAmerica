#가격추출
library(stringr)
library(quantmod)
library(xts)
library(magrittr)
#가격 크롤링
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
ticker[which(ticker$market=='nasdaq'),]
ticker%>%nrow
#어디서 끊겼는지 확인
which(ticker$symbol=='WVE')
err<-list()
complete<-list()
length(ticker$symbol)
for (i in 1:nrow(ticker)) {
name<-ticker$symbol[i]%>%as.character()
tryCatch({
getSymbols(Symbols=name,src = 'yahoo')
price<-get(name)
price<-price[,4]
price<-price%>%as.data.frame()
price<-cbind(price%>%rownames(),price)
rownames(price)<-NULL
colnames(price)<-c('date',name)
write.csv(price,paste0('data/price/',name,'_price.csv'))
complete<-rbind(complete,name)
},error=function(e){
  err<-rbind(err,name)
  warning(paste0('error in ',name))
})
cat(paste0("\r==== Progress: ", i, "/",length(ticker$symbol),"===="))
Sys.sleep(sample(1.0:5.0,1)*sample(1:2,1)) #잘 돌아가는지 확인용 코드가 있을 경우 sleep 삭제 후 작성
}

samaple()



warnings()
complete%>%nrow()
complete
err
errwarnings()
#통합

price_total<-list()
for(i in 1:nrow(ticker)){
tryCatch({name<-ticker$symbol[i]
price<-read.csv(paste0('data/pyprice/',name,'_price.csv'),row.names = 1)
price<-price%>%as.xts()
price<-price[,6]
colnames(price)=name
price_total[[i]]=price
  },
error=function(e){
  warning(paste0('error in ',name))
})
  cat(paste0("\r==== Progress: ", i, "/",length(ticker$symbol),"===="))  
}
price_total[[1]]
price_total
warnings()
price_list = do.call(cbind, price_total) %>% na.locf()
price_list
price_t<-price_list%>%data.frame()

row.names(price_t)%>%last()
write.csv(price_t,'data/price_t.csv')
price_t
ticker$symbol%>%last()
ncol(price_t)
length(ticker$symbol)
