 
scraper_and_cal_apply <- function(toassaign_df) {
  
  x<-30
  FVoEq <-"FVoEq"
  FVoEq <- data.frame(FVoEq)
  PB <- "P/B"
  PB <- data.frame(PB)
  toassaign_df<- bind_cols(toassaign_df, FVoEq)
  toassaign_df<- bind_cols(toassaign_df, PB)
  while (x<=30) {
    company <- toassaign_df[paste(x),1]
    print(paste("checking Sector", x, "of",nrow(toassaign_df), "of", company))
    ticker <- toassaign_df[paste(x),2]
    check_for_fin <- isTRUE(toassaign_df[paste(x),3]=="Financial Services")
    if(check_for_fin=="FALSE"){
      print(paste("applying DCF scraper to", company))
      scraped_DCF_data <- lapply(toassaign_df$Ticker[as.numeric(paste(x))],DCF_data_scraper)
      
      print(paste("calculating fair value of equit of", company))
      FVoEq <- DCF_calculation()
      toassaign_df[paste(x),6] <- FVoEq$FVoEq
      print(paste("applying book value scraper to", company))
      toassaign_df[paste(x),7] <- bookv_data(ticker)
    }
    else {
      print(paste("applying book value scraper to", company))
      toassaign_df[paste(x),6] <- "NA"
      toassaign_df[paste(x),7] <- bookv_data(ticker)
    }
    x<-x+1 
  
  }
  out <- list (
    toassaign_df = toassaign_df,
    scraped_DCF_data =scraped_DCF_data
  )
  return(out)
}

