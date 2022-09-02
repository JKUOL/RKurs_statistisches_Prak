# takes a data frame and calculates the Calenderised data for it 

calenderization <- function(toassaign_df) {

  rateofffye <- scraped_DCF_data[[1]]$T0Data$rateofffye
 
  # assigns the count 2 to the variable y so the second colum gets accessed
  
  y<-2
  
  # repeats the first while loop till the end of the col number of the df is reached
  
  while (y<=ncol(toassaign_df)) {
  
  # assigns the count 1 to the variable x so the first row gets accessed
    
  x <- 1
  
  # repeats the second loop till the end of the row number of the df is reached
  
  while(x<=nrow(toassaign_df)){
  
  # variable a and b are used to shorten the calculation    
    
  a <- toassaign_df[as.numeric(paste(x)),as.numeric(paste(y))]
  
  # counts x + 1 so the next data of the next row can be accessed
  
  x <- x+1
  b <- toassaign_df[as.numeric(paste(x)),as.numeric(paste(y))]
  
  # calculates the calenderized data with the rate of the fiscal year end
  
  calen <- a*rateofffye+b*(1-rateofffye)
  
  # substracts x-1 so the correct row gets subsituted with the calenderized data
  
  x <- x-1
  toassaign_df[as.numeric(paste(x)),as.numeric(paste(y))] <- calen
  
  # counts x+1 so the loop repeats on the next row of the df
  
  x <- x+1
  }
  
  # counts y+1 so the first loop repeats after completion of the first col 
  
  y <- y+1
  }
  out <- list(
    list = toassaign_df
  )
  return(out)
    
}

