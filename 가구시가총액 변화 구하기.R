library(dplyr)
library(PerformanceAnalytics)
library(quantmod)
library(magrittr)
library(httr)
library(rvest)
library(xts)
#1. furnishing 산업에 있는 가구 가격 지표크롤링(2007년부터 상장한 기업)
#티커 불러오기
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
ticker%>%head()
ind<-'Furnishings'
ticker
tic<-ticker[ticker$industry==ind,]%>%na.omit()
symbol<-tic$symbol
symbol%>%as.character()

#가격 리스트 구하기
for(i in 1:nrow(tic)){
  name<-tic$symbol[i]
  price<-read.csv(paste0('data/price/',name,'_price.csv'),row.names = 1)
  rownames(price)<-price[,1]
  price[,1]<-NULL
  price<-price%>%as.xts
  fur[[i]]<-price
}
furprice<-do.call(cbind,fur)%>%na.locf()




#2. 시가총액으로 wacc을 만든다.
#비중 다시 구하기
#2007년 부터 가격지표 있는 가구사 구하기
furprice<-furprice[,!is.na(furprice%>%head(.,1))]
tic<-tic[tic$symbol%in%colnames(furprice),]
#시가총액 정수화하기
tic$capital<-tic$capital%>%as.character()%>%gsub(',','',.)%>%as.numeric()
tic$capital
weight<-tic$capital/sum(tic$capital)
#비중 곱하기
for(i in 1:nrow(tic)){
  furprice[,i]<-furprice[,i]*weight[i]
}
#가중평균 구하기
wacc<-apply(furprice,MARGIN = 1,sum)
wacc%>%head
wacc<-wacc%>%as.xts()

#3. 경기제 etf와 가격 비교 단기 VS 장기
#경기제 가격 지표 크롤링
getSymbols('VCR')
vcad<-VCR$VCR.Close


##장기 비교
#etf와 fur인덱스 수익률 구하기
furret<-CalculateReturns(wacc)%>%na.omit()
vcret<-CalculateReturns(vcad)%>%na.omit()
#길이 잘라내기
furret%>%head
furret%>%tail
index(furret)<-index(vcret["2007-01-04::2020-11-27"])

R<-cbind(furret,vcret)


R.cum = data.frame(cumprod(1+R)) %>%
  mutate(Date = rownames(.),
         Date = as.Date(Date)) %>%
  gather(key, value, -Date)
R.cum

ggplot(R.cum, aes(x = Date, group = key, color = key)) +
  geom_line(aes(y = value)) +
  ggtitle('가구vs소비재 장기 주가 비교')

##단기 비교
#etf와 fur인덱스 수익률 구하기
furret<-CalculateReturns(wacc)%>%na.omit()
vcret<-CalculateReturns(vcad)%>%na.omit()
#길이 잘라내기
furret<-furret%>%xts::last(.,168)
furret%>%head
furret%>%tail
index(furret)<-index(vcret["2020-04-01::2020-11-27"])
vcret<-vcret["2020-04-01::2020-11-27"]
R<-cbind(furret,vcret)


R.cum = data.frame(cumprod(1+R)) %>%
  mutate(Date = rownames(.),
         Date = as.Date(Date)) %>%
  gather(key, value, -Date)
R.cum

ggplot(R.cum, aes(x = Date, group = key, color = key)) +
  geom_line(aes(y = value)) +
  ggtitle('가구vs소비재 단기 주가 비교')






#=>신문기사로 살을 붙여준다.

#티커 불러오기
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
ticker%>%head()
ind<-'Furnishings'
ticker
tic<-ticker[ticker$industry==ind,]%>%na.omit()
tic<-tic[c(-2,-7),]
symbol<-tic$symbol
symbol%>%as.character()


#가중평균을 구하겠습니다.
#뉴머릭화한깐 깨지는것을 확인했다.
  tic$capital<-tic$capital%>%as.character()%>%gsub(',','',.)%>%as.numeric()
  
tic$capital
#비중구하기
weight<-tic$capital/sum(tic$capital)
    fur<-list()
tic%>%nrow()
#가격 리스트 구하기
    for(i in 1:nrow(tic)){
    name<-tic$symbol[i]
  price<-read.csv(paste0('data/price/',name,'_price.csv'),row.names = 1)
  rownames(price)<-price[,1]
  price[,1]<-NULL
  price<-price%>%as.xts
  fur[[i]]<-price
    }

furprice<-do.call(cbind,fur)%>%na.locf()
#비중 다시 구하기
furprice<-furprice[,c(-5,-9,-11,-12,-16,-18)]
identical(tic$symbol,colnames(furprice))
tic<-tic[tic$symbol%in%colnames(furprice),]
weight<-tic$capital/sum(tic$capital)

#비중 곱하기
for(i in 1:nrow(tic)){
  furprice[,i]<-furprice[,i]*weight[i]
}
furprice%>%tail
weight
#가중평균 구하기
wacc<-apply(furprice,MARGIN = 1,sum)
wacc%>%head
wacc%>%as.xts()
plot(wacc%>%as.xts())
#수익률 구하기
ret<-wacc%>%CalculateReturns()
ret<-ret%>%na.omit()
ret<-ret%>%as.xts()
ret%>%tail
#누적 수익률 구하기
cumret<-cumprod(1+ret)
rownames(cumret)<-rownames(ret)
cumret
cumret%>%plot
#다우 지수 구하기
getSymbols('^DJI')
djad<-DJI$DJI.Close
djret<-CalculateReturns(djad)%>%na.omit
djcumret<-cumprod(1+djret)
djcumret%>%plot
#다우지수보다 열등합니다.
tic
#같은 섹터인 사이시컬 etf와 비교
getSymbols('VCR')
vcad<-VCR$VCR.Close
vcret<-CalculateReturns(vcad)%>%na.omit
vccumret<-cumprod(1+vcret)
vccumret<-vccumret
vccumret%>%plot
vccumret<-vccumret
cumret%>%str
#퍼니쳐 인덱스
getSymbols('BSET')
bs<-BSET$BSET.Close
bsret<-CalculateReturns(bs)%>%na.omit()
bscumret<-cumprod(1+bsret)
bscumret%>%plot
vccumret%>%str
library(ggplot2)
library(tidyr)
vcad
wacc<-as.xts(wacc)
wacc<-wacc%>%na.omit()
wacc%>%head
vcad<-vcad["2020-02-06::2020-11-27"]
index(wacc)<-index(vcad)
wr<-CalculateReturns(wacc)%>%na.omit()
vr<-CalculateReturns(vcad)%>%na.omit()

R<-cbind(wr,vr)
R.cum = data.frame(cumprod(1+R)) %>%
  mutate(Date = rownames(.),
         Date = as.Date(Date)) %>%
  gather(key, value, -Date)
R.cum

ggplot(R.cum, aes(x = Date, group = key, color = key)) +
  geom_line(aes(y = value)) +
  ggtitle('Portfolio Cumulative Return')

tic$symbol    
