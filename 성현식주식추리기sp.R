library(quantmod)
library(xts
        )
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)
#주식
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
price<-read.csv('data/price_t.csv',row.names = 1)

#모멤텀
ret<-price%>%Return.calculate()
ret_12m<-price%>%Return.calculate()%>%xts::last(252)%>%
  sapply(., function(x){prod(1+x)-1})
ret_6m<-price%>%Return.calculate()%>%xts::last(180)%>%
  sapply(., function(x){prod(1+x)-1})
ret_1m<-price%>%Return.calculate()%>%xts::last(30)%>%
  sapply(., function(x){prod(1+x)-1})
ret_1w<-price%>%Return.calculate()%>%xts::last(5)%>%
  sapply(., function(x){prod(1+x)-1})
ret_bind=cbind(-ret_1m,ret_6m,ret_12m)%>%data.frame()
ret_bind

factor_mom=ret_bind%>%
  mutate_all(list(~min_rank(desc(.))))%>%
  mutate_all(list(~scale(.)))%>%
  rowSums()
invest_mom<-rank(factor_mom)<=30
stock<-ticker[invest_mom,]%>%
  mutate('1y'=ret_12m[invest_mom],
         '1w'=ret_1w[invest_mom])
stock
write.csv(stock,'data/stock1.csv')
