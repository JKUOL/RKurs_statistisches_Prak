# a while loop takes the ticker and uses the function basics_data to assign 
# the correct Industry, Price and P/E to the Company and applies 
# the function to Dow outputting a list

basics_assignement <- function(toassaign_df) {
  
  # x <- 1 so the while loop starts to count in the first row of the data frame
  # creates a data frame out of Company, toassaign_df, Industry, Price and P/E
  # and fills the rows the respective variable

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
  
  # removes the varibales so they can be accessed with attach()
  
  rm(Company,Industry,Price,PE, Currency)
  
  # creates a while loop, which counts from the start row of the data frame (x=1),
  # to its end
  # the current ticker is assigned and the function basics_data is used
  # which scraped the needed data for the current ticker
  
  while (x<=nrow(toassaign_df)) {
    ticker <- toassaign_df[paste(x),2] 
    basics <- basics_data(paste(ticker))
    # attach enables the direct use of the col names 
    attach(basics)
    # prints statements so user knows what is processed and assignes the 
    # scraped data to the data frame 
    toassaign_df[paste(x),1] <- Company
    print(paste("assining Industry", x, "of",nrow(toassaign_df), "to", Company))
    toassaign_df[paste(x),3] <- Industry
    print(paste("assining Currency", x, "of",nrow(toassaign_df), "to", Company))
    toassaign_df[paste(x),4] <- Currency
    print(paste("assining Price", x, "of",nrow(toassaign_df), "to", Company))
    toassaign_df[paste(x),5] <- Price
    print(paste("calculating and assining P/E", x, "of",nrow(toassaign_df), "to", Company))
    toassaign_df[paste(x),6] <- round(PE,2)
    # adds +1 to x so the next row can be accessed and detach so the data doesnt 
    # get masked
    x<-x+1  
    detach(basics)
  }
  # outputs a list with the scraped and assignened data of all stocks
  out <- list(
    list = toassaign_df
  )
  return(out)
}
