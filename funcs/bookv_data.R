
bookv_data <- function(ticker) {
  
  
  # ticker <- "HD"
  
  # assignes the URL of the ticker to a Variable and reads the HTML text
  
  url_stats <- paste0('https://finance.yahoo.com/quote/',
                      ticker, '/key-statistics?p=', ticker)
  html_stats <- read_html(url_stats) %>% html_node('body') %>% 
    html_text() %>% toString()
  
  # extracts Open Price, gathers all text in the HTML_text between the specified passages
  
  BVS <- qdapRegex::ex_between(html_stats, "Book Value Per Share (mrq)", "Cash Flow ")[[1]]
  BVS <- as.numeric(BVS)
  
  basics <- basics_data(ticker)
  attach(basics)
  Price <- Price
  detach(basics)
  
  PB <- round(Price/BVS,2)
  
  out <- data.frame (
    PB = PB
  )
}

# bookv_data <- function(ticker) {
#   
#   
#   # ticker <- "JPM"
#   
#   # assignes the URL of the ticker to a Variable and reads the HTML text
#   
#   url_stats <- paste0('https://finance.yahoo.com/quote/',
#                       ticker, '/key-statistics?p=', ticker)
#   html_stats <- read_html(url_stats) %>% html_node('body') %>% 
#     html_text() %>% toString()
#   
#   # extracts Open Price, gathers all text in the HTML_text between the specified passages
#   
#   PB <- qdapRegex::ex_between(html_stats, "Price/Book (mrq)", "Enterprise")[[1]]
#   PB <- as.numeric(PB)
#   
#   out <- data.frame (
#     PB = PB
#   )
# }