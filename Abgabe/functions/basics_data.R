# Die folgenden Funktionen sind eigentlich dazu gedacht gesourced zu werden
# werden jedoch der vollständigkeitshalber hier aufgeführt

# extrahiert den Preis, die Industry, EPS (earnings per share) und den 
# Firmennamen von Yahoo Finances und berechnet den PE (Price to earnings)

basics_data <- function(ticker) {
  
  # fügt der URL mit hilfe der paste0 funktion die Variable ticker hinzu
  # die 0 an paste bewirkt das weglassen von leerzeichen
  url_profile <- paste0('https://finance.yahoo.com/quote/', ticker)
  
  # weist der Variablen den HTML Text der URL zu
  # hier werden der HTML Text des Profils der Firma hinzugefügt
  html_profile <- read_html(url_profile) %>% html_node('body') %>% 
    html_text() %>% 
    toString()
  
  # extrahiert den HTML Text der financials Seite von Yahoo Finance der Firma
  url_fin <- paste0('https://finance.yahoo.com/quote/', 
                    ticker, '/financials?p=', ticker)
  html_fin <- read_html(url_fin) %>% html_node('body') %>% 
    html_text() %>% toString()
  
  # extrahiert aus dem HTML Text den aktuellen Börsenpreis der Firma 
  # die qdapRegex Funktion extrahiert hierbei alle Textpassagen zwischen den
  # Schlagwörtern. Da die Zahlen aus dem Text als nicht numerisch extrahiert 
  # wird, müssen diese im FOlgenden immer in numerische charactere umgewandelt 
  # werden.
  # Im Anschluss wird die Zahl auf zwei Nachkommastellen gerundet
  Price <- qdapRegex::ex_between(html_profile, "currentPrice\":{\"raw\":",
                                 ",\"fmt\":\"")[[1]]
  Price <- as.numeric(Price)
  Price <- round(Price, 2)
  
  # Extrahiert den Subsektor der Firma, da mehrere Textpassagen extrahiert werden
  # wird nur der erste genutzt (die gewollte Industry)
  Industry <- qdapRegex::ex_between(html_profile, "industry\":\"",
                                    "\"")[[1]]
  Industry <- Industry[1]
  
  # extrahiert die Währung
  Currency <- qdapRegex::ex_between(html_fin, "financialCurrency\":\"",
                                    "\"},\"price")[[1]]
  Currency <- data.frame(Currency)
  
  # extrahiert den EPS   
  
  EPS <- qdapRegex::ex_between(html_profile, "EPS (TTM)",
                               "Earnings")[[1]]
  EPS <- EPS[1]
  EPS <- as.numeric(EPS)
  
  # berechnen den PE und rundet auf zwei Nachkommastellen
  
  PE <- round(Price/EPS,2)
  
  # extrahiert den Firmennamen
  
  Company <- qdapRegex::ex_between(html_profile, "\":{\"title\":\"",
                                   " (")[[1]]
  Company <- Company[1]
  
  # weist einer Liste die Variablen Price, Industry, PE, and Company zu und
  # gibt diese aus
  
  out <- list (
    Price = Price,
    Industry = Industry,
    PE = PE,
    Company = Company,
    Currency = Currency
  )
  return(out)
}