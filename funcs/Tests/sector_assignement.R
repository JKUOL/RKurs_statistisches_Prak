sector_assignement <- function(toassaign_df = Dow) {
  
  x<-1
  Sector<-"Sector"
  Sector<-data.frame(Sector)
  toassaign_df<- bind_cols(toassaign_df, Sector)   
  
  while (x<=nrow(toassaign_df)) {
    company <- toassaign_df[paste(x),1]
    print(paste("assisning sector", x, "of",nrow(toassaign_df), "to", company))
    ticker <- toassaign_df[paste(x),2]
    Sector <- sector_data(paste(ticker))
    Dow [paste(x),3] <- Sector
    x<-x+1  
  }
  out <- list(
    toassaign_df = Dow
  )
  return(out)
}