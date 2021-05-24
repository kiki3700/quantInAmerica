#이제할 것은 바로 모든 것을 통합하고 순서를 매기는 것입니다만
#모멘텀 분석
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)

price<-read.csv('data/price_t.csv',row.Nnames = 1)
ticker<-read.csv('data/ticker/ticker.csv',row.names=1)
price<-price%>%as.xts()
ret<-Return.calculate(price)%>%xts::last(252)
#1년 모멘템 줄세우기
ret_12m=ret%>%sapply(., function(x){
  prod(1+x)-1
})
ret_12m[rank(-ret_12m)<=30]
invest_mom=rank(-ret_12m)<=30
ticker[invest_mom,]%>%select(.,'symbol','name')%>%
  mutate('return'=round(ret_12m[invest_mom],
                        4))

#위험조정 수익률
std_12m<-ret%>%apply(.,2,sd)%>%multiply_by(sqrt(252))
sharpe_12m<-ret_12m/std_12m
invest_mom_sharpe
invest_mom_sharpe=rank(-sharpe_12m)<=30
ticker[invest_mom_sharpe,]%>%select('symbol','name')%>%
  mutate('return'=round(ret_12m[invest_mom_sharpe],2),
         'sd'=round(std_12m[invest_mom_sharpe],2),
         '위험조정수익률'=round(sharpe_12m[invest_mom_sharpe]),2)
intersect(ticker[invest_mom,'symbol'],
          ticker[invest_mom_sharpe,'symbol'])

library(xts)
library(tidyr)
library(ggplot2)

price[, invest_mom_sharpe] %>%
  fortify.zoo() %>%
  gather(ticker, price, -Index) %>%
  ggplot(aes(x = Index, y = price)) +
  geom_line() +
  facet_wrap(. ~ ticker, scales = 'free') +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
#퀄리티 지표: pbr per로 줄세우기
ratio<-read.csv('data/value_list.csv',row.names = 1)
ratio$per%>%as.character()%>%as.numeric()
for(i in 1:4){
  ratio[,i]<-ratio[,i]%>%as.character()%>%as.numeric()
}
value<-ratio[,1:2]
value%>%colnames()
#밸류지표
factor_value<-value%>%
  mutate_all(list(~min_rank(desc(.))))%>%
  mutate_all(list(~scale(.)))%>%
  rowSums()
#퀄리티 지표
#roe <=0 제거하기
ratio[which(ratio[,3]<=0),3]<-NA

quality<-ratio[,c(1,3)]
quality%>%str()
factor_quality=quality%>%
  mutate_all(list(~min_rank(.)))%>%
  mutate_all(list(~scale(.)))%>%
  rowSums()

#모멤텀지표
price<-read.csv('data/price_t.csv',row.names = 1)
ret_1m=Return.calculate(price)%>%xts::last(30)%>%
  sapply(.,function(x){prod(1+x)-1})
ret_6m=Return.calculate(price)%>%xts::last(120)%>%
  sapply(.,function(x){prod(1+x)-1})
ret_12m=Return.calculate(price)%>%xts::last(252)%>%
  sapply(.,function(x){prod(1+x)-1})
ret_bind = cbind(ret_1m, ret_6m, ret_12m) %>% data.frame()
factor_mom=ret_bind%>%
  mutate_all(list(~min_rank(desc(.))))%>%
  mutate_all(list(~scale(.)))%>%
  rowSums()
factor_mom %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()
#그래프로 확인
library(corrplot)

cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  setNames(c('Quality', 'Value', 'Momentum')) %>%
  cor(use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))

factor_qvm =
  cbind(factor_quality, factor_mom) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>%
  mutate(factor_quality = factor_quality * 0.5,
         factor_mom = factor_mom * 0.5) %>%
  rowSums()

invest_qvm = rank(factor_qvm) <= 32
invest_qvm[3904]<-FALSE
which(ticker$symbol=="KB")
roe<-quality$roe[invest_qvm]%>%as.character()%>%as.numeric()
per<-value$per[invest_qvm]%>%as.character()%>%as.numeric()
p<-ticker[invest_qvm, ] %>%
  select('symbol', 'name','sector','industty') %>%
  cbind(round(roe, 2)) %>%
  cbind(round(per, 2)) %>%
  cbind(round(ret_12m[invest_qvm]%>%as.numeric(), 2)) %>%
  setNames(c('종목코드', '종목명','sector','industry', 'ROE', 'PER', '12M'))
p%>%nrow()
write.csv(p,'data/port4.csv')


quality$roe[invest_qvm ]
value$pbr[invest_qvm]
ret_12m[invest_qvm]
t<-ticker[invest_qvm,]
write.csv(t,'data/portfolio.csv')
