price<-read.csv('data/price_list.csv',row.names = 1)
price[,which(ticker$symbol%>%as.character()%in%price%>%colnames())]%>%ncol()
ticker$symbol%in%price%>%colnames()
ticker$symbol%>%as.character()%>%str()
price%>%colnames()%>%str()

#value리스트는 통과한듯
setdiff(rownames(value_list),ticker$symbol%>%as.character())
setdiff(ticker$symbol%>%as.character(),rownames(value_list))
se
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
value_list<-read.csv('data/value_list.csv',row.names = 1)
value_list
x<-c('a','b')
y<-c('a','b','c')
setdiff(x,y)

#차집합을 구해봅시다.
#결과 분석 price에는 kron, sprb, sttk, TURE가 없다.
setdiff(ticker$symbol%>%as.character(),colnames(price))
#결과 분석 결과 price는 추가적으로 존나 많은 1296개의 추가 종목을 가지고 있다 이것을 어떻게 없앨 것인가
extra<-setdiff(price%>%colnames,ticker$symbol%>%as.character())
write.csv(extra,'data/extra_symbol.csv')
#그리고 부족한 것을 어떻게 채울것인가가 관건입니다. csv를 직접 수정합니다. 순서는 tiker열보고 추가해주시면 됩니다.
ncol(price)-setdiff(price%>%colnames,ticker$symbol%>%as.character())%>%length()
setdiff(ticker$symbol%>%as.character(),colnames(price))
extra%>%str()
price
extra<-read.csv('data/extra_symbol.csv',row.names = 1)
which(extra%in%price%>%colnames())
extra<-extra%>%as.character()
which(setdiff(price%>%colnames(),extra)%>%length()%in%price%>%colnames())
setdiff(price%>%colnames(),extra)%>%length()%in%price%>%colnames()
price[,which(extra[1]==colnames(price))]
#for문으로 조지기
for(i in 1:length(extra)){
  n<-which(extra[i]==colnames(price))
  price[,n]<-NULL
}
ncol(price)
write.csv(price,'data/price.csv')
#프라이스에 체워주기기
setdiff(ticker$symbol%>%as.character(),price%>%colnames())

#데이터 확인
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
price<-read.csv('data/price_list.csv',row.names = 1)
value<-read.csv('data/value_list.csv',row.names = 1)
t<-ticker$symbol%>%as.character()
p<-price%>%colnames()
v<-value%>%rownames()
setdiff(t,p)
setdiff(p,t)
setdiff(t,v)
setdiff(v,t)
setdiff(v,p)
which("TRUE."%in%p)

#섹터 확인
ticker<-read.csv('data/ticker/ticker.csv',row.names = 1)
ticker%>%head()
unique(ticker$sector)
unique(ticker$industty)
