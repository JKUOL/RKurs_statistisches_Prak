

ticker <- "NKE"



print("assingning urls and loading HTML")

url_fin <- paste0('https://finance.yahoo.com/quote/', 
                  ticker, '/financials?p=', ticker)
html_fin <- read_html(url_fin) %>% html_node('body') %>% 
  html_text() %>% toString()
url_ana <- paste0('https://finance.yahoo.com/quote/',
                  ticker,'/analysis?p=',ticker)
html_ana <- read_html(url_ana) %>% html_node('body') %>% 
  html_text() %>% toString()

Year <- qdapRegex::ex_between(html_fin, "financialsChart\":{\"yearly\":", "quarterly\":[{\"")[[1]]
Year <- qdapRegex::ex_between(Year, "{\"date\":", ",\"revenue\"")[[1]]
Year <- as.numeric(Year)
Year <- c(Year, paste0(Year[4]+1, "E"), paste0(Year[4]+2, "E"), paste0(Year[4]+3, "E"), paste0(Year[4]+4, "E"))

# extracting total revenue

TotalRevenue <- qdapRegex::ex_between(html_fin, ",\"revenue\":{\"raw\":", ",\"fmt\":\"")[[1]]
TotalRevenue <- TotalRevenue[1:4]
TotalRevenue <- as.numeric(TotalRevenue)

# extracting projectet revenue

ProjRev <- qdapRegex::ex_between(html_ana, "revenueEstimate", "numberOfAnalysts")[[1]]
ProjRev <- ProjRev[3:4]
ProjRev <- c(qdapRegex::ex_between(ProjRev, "\"avg\":{\"raw\":", ",\"fmt")[[1]],
             qdapRegex::ex_between(ProjRev, "\"avg\":{\"raw\":", ",\"fmt")[[2]])
ProjRev <- as.numeric(ProjRev)

TotalRevenue <- c(TotalRevenue, ProjRev)

meanRevGrowthRate <- mean(TotalRevenue)

ProjAnalysts <- qdapRegex::ex_between(html_ana, "revenueEstimate", "yearAgoRevenue")[[1]]
ProjAnalysts <- ProjAnalysts[3:4]
ProjAnalysts <- c(qdapRegex::ex_between(ProjAnalysts, "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[1]],
                  qdapRegex::ex_between(ProjAnalysts, "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[2]] )
ProjAnalysts <- as.numeric(ProjAnalysts)












