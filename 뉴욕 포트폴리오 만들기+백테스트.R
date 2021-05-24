#공분산 구하기
library(RiskPortfolios)
library(PerformanceAnalytics)
price<-price_ny[,invest]
price
rets<-price%>%Return.calculate()%>%na.omit()
rets
covmax<-cov(rets)
cor(rets)
#포트폴리오 구성

ncol(rets)
pny<-optimalPortfolio(covmax,control=list(
  type='lowvol',
  constraint='lo'))%>%
  round(., 4) %>%
  setNames(colnames(rets))

pny1 = optimalPortfolio(covmax,
                       control = list(type = 'minvol',
                                      constraint = 'lo')) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(pny1)  

pny2= optimalPortfolio(covmax,
                       control = list(type = 'minvol',
                                      constraint = 'user',
                                      LB = rep(0.05, 15),
                                      UB = rep(0.20, 15))) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(pny2)


#백테스트
pny1=pny1-sum(pny1)/15
pny1
pny2*price
b=list()
for (i in 1:15) {
  price[,i]<-price[,i]*pny2[i]
}
r<-rowSums(price%>%na.omit())
r<-r%>%Return.calculate()%>%na.omit()
VaR(r,p=0.95)
ret<-price%>%na.omit()%>%Return.calculate()%>%na.omit()
portfolio = Return.portfolio(R = rets,
                             weights = pny2,
                             rebalance_on = 'years',
                             verbose = TRUE)
portfolio
charts.PerformanceSummary(portfolio$returns)
write.csv(pny2,'data/stra/weight.csv')
