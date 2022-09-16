# eine while Schleife, nimmt den Ticker und weist diesem die korrekte Industry,
# den Preis und P/E, sowie den Firmennamen zu 
# in Klammern steht toassaign_df, da bei nutzen der Funktion, dieser zugewiesen
# werden kann

basics_assignement <- function(toassaign_df) {
  
  # x <- 1, damit die while Schleife in der ersten Reihe des data frames 
  # beginnt zu zählen. Erstellt einen data frame aus toassaign_df, Industry, 
  # Price und P/E und füllt die Reihen mit den Variablen
  x<-1
  Company <- "Company"
  Company <- data.frame(Company)
  toassaign_df <- bind_cols(Company, toassaign_df)
  Industry<-"Industry"
  Industry<-data.frame(Industry)
  toassaign_df<- bind_cols(toassaign_df, Industry) 
  Currency<-"Currency"
  Currency<-data.frame(Currency)
  toassaign_df<- bind_cols(toassaign_df, Currency) 
  Price<-"Price"
  Price<-data.frame(Price)
  toassaign_df<- bind_cols(toassaign_df, Price)
  PE <- "P/E" 
  PE <- data.frame(PE)
  toassaign_df<- bind_cols(toassaign_df, PE)
  
  # entfernt die Variablen, damit sie mit attach() genutz werden können
  rm(Company,Industry,Price,PE, Currency)
  
  # es wird eine while schleife erstellt, welche von der ersten Reihe (x=1) bis 
  # zur letzten Reihe (nrow) des data frames zählt
  while (x<=nrow(toassaign_df)) {
    # der aktuelle Ticker wird zugewiesen und die function basics_data wird 
    # angewended um die benötigten datan des tickers zu scrapen
    ticker <- toassaign_df[paste(x),2] 
    basics <- basics_data(paste(ticker))
    
    # attach ermöglicht hier den direkten zugriff auf die col Namen
    attach(basics)
    
    # Gescrapten Daten werden dem data frame zugewiesen
    # Zudem werden Statments gedruckt, welche den Nutzer wissen lässt, 
    # an welcher Stelle die zuweisung aktuell ist
    toassaign_df[paste(x),1] <- Company
    print(paste("assining Industry", x, "of",nrow(toassaign_df), "to",
                Company))
    toassaign_df[paste(x),3] <- Industry
    print(paste("assining Currency", x, "of",nrow(toassaign_df), "to",
                Company))
    toassaign_df[paste(x),4] <- Currency
    print(paste("assining Price", x, "of",nrow(toassaign_df), "to",
                Company))
    toassaign_df[paste(x),5] <- Price
    print(paste("calculating and assining P/E", x, "of",nrow(toassaign_df),
                "to", Company))
    toassaign_df[paste(x),6] <- round(PE,2)
    
    # addiert x + 1 damit die nächste Reihe bearbeitet wird
    # detach wird angewandt, damit die daten nicht maskiert werden
    x<-x+1  
    detach(basics)
  }
  # gibt eine Liste mit den gescrapten und zugewiesenen daten aus 
  out <- list(
    list = toassaign_df
  )
  return(out)
}