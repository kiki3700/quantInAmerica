library(rvest)
library(stringr)
library(magrittr)

page <- read_html('https://finance.yahoo.com/quote/TCEHY/financials?p=TCEHY')
nodes <- page %>%html_nodes(".fi-row")
df = NULL

for(i in nodes){
  r <- list(i %>%html_nodes("[title],[data-test='fin-col']")%>%html_text())
  df <- rbind(df,as.data.frame(matrix(r[[1]], ncol = length(r[[1]]), byrow = TRUE), stringsAsFactors = FALSE))
}

matches <- str_match_all(page%>%html_node('#Col1-3-Financials-Proxy')%>%html_text(),'\\d{1,2}/\\d{1,2}/\\d{4}')  
headers <- c('Breakdown','TTM', matches[[1]][,1]) 
names(df) <- headers
write.csv(df,'TCEHY.csv',row.names = T)
