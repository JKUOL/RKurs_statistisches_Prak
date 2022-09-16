# nimmt einen data frame und berechnet die calenderization für diesen
calenderization <- function(toassaign_df) {
  # nimmt die berechnetet rateofffye des DCF scrapers und weist sie
  # einer neuen Variablen zu
  rateofffye <- scraped_DCF_data[[1]]$T0Data$rateofffye
  
  # weist der varibalen y den Wert 2 zu, damit später damit auf die zweite col
  # zugegriffen wird
  y<-2
  
  # wiederholt die erste while schleife, von 2 bis das ende der col Anzahl des 
  # df erreicht ist. 
  while (y<=ncol(toassaign_df)) {
    # weist x den Wert 1 zu, damit in der ersten Reihe begonnen wird zu zählen
    x <- 1
    # wiederholt die zweite Schleife bis das ende der Reihen erreicht ist
    
    while(x<=nrow(toassaign_df)){
      # variable a und b werden genutzt um die Rechnung zu verkleinern
      a <- toassaign_df[as.numeric(paste(x)),as.numeric(paste(y))]
      
      # addiert x + 1 damit die Daten der Nächsten reihe genutzt werden
      x <- x+1
      b <- toassaign_df[as.numeric(paste(x)),as.numeric(paste(y))]
      
      # berechnet die calenderized data mit Hilfe der rye
      calen <- a*rateofffye+b*(1-rateofffye)
      
      # x - 1 damit die gewünschte Zeile mit der calenderized data ersetzt wird
      x <- x-1
      toassaign_df[as.numeric(paste(x)),as.numeric(paste(y))] <- calen
      
      # addiert x + 1 damit die Schleife mit der nächsten Reihe weiter rechnet
      x <- x+1
    }
    # wenn eine col mit der Hilfe der zweiten Schleife ersetzt wurde, wird 
    # y + 1 addiert damit in der nächsten col weiter gerechnet wird
    y <- y+1
    
  }
  out <- list(
    list = toassaign_df
  )
  return(out)
}   