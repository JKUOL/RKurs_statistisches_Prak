# extracts Price, Industry, EPS (earnings per share)  and Company from 
# Yahoo Finacance and calculates the PE (Price to earnings)

basics_data <- function(ticker) {
  
  # assignes the URL of the ticker to a Variable and reads the HTML text
  
  url_profile <- paste0('https://finance.yahoo.com/quote/', ticker)
  
  html_profile <- read_html(url_profile) %>% html_node('body') %>% 
    html_text() %>% 
    toString()
  
  # extracts Price Price, gathers all text in the HTML_text between the specified passages
  # converts it into a numeric argument and rounds it to 2 decimal points
  
  Price <- qdapRegex::ex_between(html_profile, "currentPrice\":{\"raw\":", ",\"fmt\":\"")[[1]]
  Price <- as.numeric(Price)
  Price <- round(Price, 2)
  
  # extracts Sector
  
  Industry <- qdapRegex::ex_between(html_profile, "industry\":\"", "\"")[[1]]
  Industry <- Industry[1]
  
  
  # extracts the EPS, uses only the first variable of the 
  # data which is extracted, because there are more then one passage with the
  # word matching in the HTML_text
  
  
  EPS <- qdapRegex::ex_between(html_profile, "EPS (TTM)", "Earnings")[[1]]
  EPS <- EPS[1]
  EPS <- as.numeric(EPS)
  
  # calculates the PE ratio and rounds it to 2 decimal points
  
  PE <- round(Price/EPS,2)
  
  # extracts the company name uses only the first variable of the 
  # data which is extracted, because there are more then one passage with the
  # word matching in the HTML_text
  
  Company <- qdapRegex::ex_between(html_profile, "\":{\"title\":\"", " (")[[1]]
  Company <- Company[1]
  
  # assigns a list with Price, Industry, PE, and Company to out
  
  out <- list (
    Price = Price,
    Industry = Industry,
    PE = PE,
    Company = Company
  )
  return(out)
}
