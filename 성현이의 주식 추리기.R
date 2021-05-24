library(RiskPortfolios)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(magrittr)
library(dplyr)
??mutate
#시장에 대한 분석
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
value<-read.csv('data/value_list.csv',row.names = 1)
price<-read.csv('data/price_t.csv',row.names = 1)

#시장별로 나누기
nrow(ticker[which(ticker$market=='nasdaq'),])
#나스닥 범위1:2925 뉴욕범위2926:nrow(ticker)
ticker_nq<-ticker[1:2925,]
write.csv(ticker_nq,'data/ticker/ticker_nq.csv')
ticker_ny<-ticker[2926:nrow(ticker),]
write.csv(ticker_ny,'data/ticker/ticker_ny.csv')
price_nq<-price[,1:2925]
write.csv(price_nq,'data/price_nq.csv')
price_ny<-price[,2926:nrow(ticker)]
write.csv(price_ny,'data/price_ny.csv')
value_ny<-value[2926:nrow(ticker),]
value_nq<-value[1:2925,]
write.csv(value_ny,'data/value_ny.csv')
write.csv(value_nq,'data/value_nq.csv')
#나눴다. 나스닥은 성장률이랑 모멤텀 제일 주의
#나스닥 모멤텀
ret_12m_nq<-Return.calculate(price_nq)%>%xts::last(252)%>%
  sapply(., function(x){prod(1+x)-1})
ret_6m_nq<-Return.calculate(price_nq)%>%xts::last(120)%>%
  sapply(., function(x){prod(1+x)-1})
ret_1m_nq<-Return.calculate(price_nq)%>%xts::last(30)%>%
  sapply(., function(x){prod(1+x)-1})
ret_bind_nq=cbind(ret_1m_nq,ret_6m_nq,ret_12m_nq)%>%data.frame()
factor_mom_ny=ret_bind_ny%>%
  mutate_all(list(~min_rank(desc(.))))%>%
  mutate_all(list(~scale(.)))%>%rowSums()


#그리고 성장률 을 토대로
gro<-value_nq$revenue_growth%>%as.character()%>%as.numeric()
factor_gro=-gro%>%scale()
factor_gro

factor_dream =
  cbind(factor_gro, factor_mom_nq) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>%
  mutate(factor_gro = factor_gro * 0.50,
         factor_mom_nq = factor_mom_nq * 0.50) %>%
  rowSums()
invest_nq<-rank(factor_dream)<=60
invest_nq<-ticker_nq[invest_dream]%>%
  mutate(gro=gro[invest_nq])%>%
  mutate(ret12=ret_12m_nq[invest_nq])%>%
  mutate(ret6=ret_6m_nq[invest_nq])%>%
  mutate(ret1=ret_1m_nq[invest_nq])%>%
  mutate(price=price_nq[invest_])
invest_nq<-cbind(invest_nq,t(price1))
price1<-price_nq[nrow(price_nq),invest_dream]
ret_
#추출 성공
#저장
write.csv(invest_nq,'data/stra/invest_nqsafe.csv')
#헬스케어 빼버림
invest_nq<-read.csv('data/stra/invest_nqsafe.csv',row.names = 1)
invest_nq
invest_dream<-which(ticker_nq$symbol%in%invest_nq$symbol)


#공분산구하기
ret_dream<-price_nq[,invest_dream]%>%Return.calculate()%>%na.omit()
ret_dream
covmax<-cov(ret_dream)
?optimalPortfolio
#포트폴리오 구하기
pnq<-optimalPortfolio(covmax,control = list(type='minvol',
                                       constraint='user',
                                       LB=rep(0.03,16),
                                       UB=rep(0.20,16)))%>%round(.,4)%>%
  setNames(colnames(ret_dream))
pnq
#포트폴리오 가격 전체 평균 내기
pnq<-(pnq+(1-sum(pnq))/16)
pnq
price_nq[,invest_dream]
invest_nq<-cbind(invest_nq,pnq)
write.csv(invest_nq,'data/stra/port_nq.csv')
ret<-price_nq[,invest_dream]%>%Return.calculate()%>%na.omit()
ret
result<-Return.portfolio(ret,weights = pnq, rebalance_on = 'years',
                 verbose = TRUE)
result$returns
charts.PerformanceSummary(result$returns)
