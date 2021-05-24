#위험중립 모멤텀 추출
library(dplyr)
library(quantmod)
library(magrittr)
library(PerformanceAnalytics)
library(xts)
price_list<-read.csv('data/price_t.csv',row.names = 1)
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
price_list
ret<-CalculateReturns(price_list)
#수익률
ret_12m<-last(ret,252)%>%sapply(.,function(x){
  prod(1+x)-1
})
std_12m = ret%>%last(252) %>% apply(., 2, sd) %>% multiply_by(sqrt(252))
ret_6m<-last(ret,60)%>%sapply(.,function(x){
  prod(1+x)-1})

std_6m = ret%>% last(60) %>% apply(., 2, sd) %>% multiply_by(sqrt(60))
ret_1w<-last(ret,7)%>%sapply(.,function(x){
  prod(1+x)-1})
std_1w = ret %>%last(7) %>% apply(., 2, sd) %>% multiply_by(sqrt(7))
ret_12m
std_12m
#사프레시오
sharpe_12m = ret_12m / std_12m
sharpe_6m = ret_6m / std_6m
sharpe_1w = -ret_1w / std_1w

ret_bind=cbind(sharpe_12m,sharpe_6m,sharpe_1w)
ret_bind<-as.data.frame(ret_bind)

factor_mom = ret_bind %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()
factor_mom
invest_mom<-rank(factor_mom)<31
invest_mom
rec<-ticker[invest_mom,]%>%select(`symbol`,`name`,`sector`,`industty`)%>%
  mutate('price'=last(price_list[invest_mom])%>%as.numeric(),
    '12m'=ret_12m[invest_mom],
         '6m'=ret_6m[invest_mom],
         '1w'=ret_1w[invest_mom])
ret_1w%>%str
rec
last(price_list[invest_mom])
write.csv(rec,'data/stra/recovery.csv')
