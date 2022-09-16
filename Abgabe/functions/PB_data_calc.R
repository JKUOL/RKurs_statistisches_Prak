# Erstellt eine Funktion zur Berechnung des price to book value

bookv_data <- function(ticker) {
  
  # weist der URL den ticker zu und ließt den HTML Text aus
  url_stats <- paste0('https://finance.yahoo.com/quote/',
                      ticker, '/key-statistics?p=', ticker)
  html_stats <- read_html(url_stats) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_profile <- paste0('https://finance.yahoo.com/quote/', ticker)
  html_profile <- read_html(url_profile) %>% html_node('body') %>% 
    html_text() %>% 
    toString()
  
  # extrahiert den Book Value per Share
  BVS <- qdapRegex::ex_between(html_stats, "Book Value Per Share (mrq)", 
                               "Cash Flow ")[[1]]
  BVS <- as.numeric(BVS)
  
  # nimmt den Preis aus dem Stocks df und berechnet mit dem BVS den
  # Price per Book Value
  PB <- round(as.numeric(Stocks$Price[x])/BVS,2)
  
  out <- data.frame (
    PB = PB
  )
}