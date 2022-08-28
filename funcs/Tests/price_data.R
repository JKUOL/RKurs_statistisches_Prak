price_data <- function(ticker) {
  
  # assignes the URL of the ticker to a Variable and reads the HTML text
  
  url_profile <- paste0('https://finance.yahoo.com/quote/', ticker)
  
  html_profile <- read_html(url_profile) %>% html_node('body') %>% 
    html_text() %>% 
    toString()
  
  # extracts Open Price, gathers all text in the HTML_text between the specified passages
  
  Open <- qdapRegex::ex_between(html_profile, "Open", "Bid")[[1]]
  Open <- as.numeric(Open[1])
  Open <- data.frame(Open)
  
}