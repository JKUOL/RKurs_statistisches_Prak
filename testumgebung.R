basics_data <- function(ticker) {
  
  
  # assignes the URL of the ticker to a Variable and reads the HTML text
  
  url_profile <- paste0('https://finance.yahoo.com/quote/', ticker)
  
  html_profile <- read_html(url_profile) %>% html_node('body') %>% 
    html_text() %>% 
    toString()
  
  # extracts Price Price, gathers all text in the HTML_text between the specified passages
  
  Price <- qdapRegex::ex_between(html_profile, "currentPrice\":{\"raw\":", ",\"fmt\":\"")[[1]]
  Price <- as.numeric(Price)
  Price <- round(Price, 2)
  
  # extracts Sector, gathers all text in the HTML_text between the specified passages
  
  Sector <- qdapRegex::ex_between(html_profile, "\",\"sector\":\"", "\",\"")[[1]]
 
  # assignes the URL of the ticker to a Variable and reads the HTML text
  
  EPS <- qdapRegex::ex_between(html_profile, "EPS (TTM)", "Earnings")[[1]]
  EPS <- EPS[1]
  EPS <- as.numeric(EPS)
 
  PE <- round(Price/EPS,2)
  
  out <- list (
    Price = Price,
    Sector = Sector,
    PE = PE
  )
}

basics_assignement <- function(toassaign_df = Dow) {
  
  toassaign_df <- Dow
  
  
  x<-1
  Sector<-"Sector"
  Sector<-data.frame(Sector)
  toassaign_df<- bind_cols(toassaign_df, Sector)   
  Price<-"Price"
  Price<-data.frame(Price)
  toassaign_df<- bind_cols(toassaign_df, Price)
  PE <- "P/E" 
  PE <- data.frame(PE)
  toassaign_df<- bind_cols(toassaign_df, PE)
  rm(Sector,Price,PE)
  
  while (x<=nrow(toassaign_df)) {
    company <- toassaign_df[paste(x),1]
    print(paste("assining sector", x, "of",nrow(toassaign_df), "to", company))
    ticker <- toassaign_df[paste(x),2]
    basics <- basics_data(paste(ticker))
    attach(basics)
    toassaign_df[paste(x),3] <- Sector
    print(paste("assining Price", x, "of",nrow(toassaign_df), "to", company))
    toassaign_df[paste(x),4] <- Price
    print(paste("calculating and assining P/E", x, "of",nrow(toassaign_df), "to", company))
    toassaign_df[paste(x),5] <- round(PE,2)
    x<-x+1  
    detach(basics)
  }
  out <- list(
    list = toassaign_df
  )
  return(out)
}
Dow <- basics_assignement()
Dow <- Dow$list

bookv_data <- function(ticker) {
  
  ticker <- "JPM"
  
  # assignes the URL of the ticker to a Variable and reads the HTML text 
  
  url_stats <- paste0('https://finance.yahoo.com/quote/',
                     ticker, '/key-statistics?p=', ticker)
  html_stats <- read_html(url_stats) %>% html_node('body') %>% 
    html_text() %>% toString()
  
  url_balance <- paste0('https://finance.yahoo.com/quote/', ticker,
                     '/balance-sheet?p=JPM', ticker)
  html_balance <- read_html(url_balance) %>% html_node('body') %>% 
    html_text() %>% toString()
  
  # extracts outstanding shares
  
  floatshares <- qdapRegex::ex_between(html_stats, "\"floatShares\":{\"raw\":", ",\"fmt\"")[[1]]
  floatshares <- as.numeric(floatshares)
  
  # extracts book value
  
  bookv <- qdapRegex::ex_between(html_balance, "annualTangibleBookValue", "annualOtherCurrentBorrowings")[[1]]
  bookv <- qdapRegex::ex_between(bookv, "\"raw\":", ",\"fmt\"")[[1]]
  bookv <- bookv[4]
  bookv <- as.numeric(bookv)
  
  # calculating book value per share
  
  BPS <- bookv/floatshares

}


# creates a function to acquire the needed Data for the Calculation of
# Fair Value of Equity with the DCF Method
# the needed Variables are the ticker the expected Growth of the global 
# economy and the timeframe for the average Return of the S%P500 of the past

DCF_data_scraper <- function(ticker, GloGrow, timeframe) {
  
  ticker<-"VZ"
  GloGrow<-0.029
  timeframe<-30
  
  # current Year and start Date for the exraction of the av. Return of S&P500
  
  curYear<-format(Sys.Date())
  curYear<-year(curYear)
  start<-curYear-timeframe
  
  # assignes the URL of the ticker to a Variable and reads the HTML text  
  
  print("assingning urls and loading HTML")
  url_fin <- paste0('https://finance.yahoo.com/quote/', 
                    ticker, '/financials?p=', ticker)
  html_fin <- read_html(url_fin) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_cf <- paste0('https://finance.yahoo.com/quote/',
                   ticker, '/cash-flow?p=', ticker)
  html_cf <- read_html(url_cf) %>% html_node('body') %>% html_text() %>% toString()
  url_bonds <- 'https://finance.yahoo.com/bonds'
  html_bonds <- read_html(url_bonds) %>% 
    html_node('body') %>%  html_text() %>% toString()
  url_stats<- paste0('https://finance.yahoo.com/quote/',
                     ticker, '/key-statistics?p=', ticker)
  html_stats <- read_html(url_stats) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_balance<- paste0('https://finance.yahoo.com/quote/',
                       ticker, '/balance-sheet?p=', ticker)
  html_balance <- read_html(url_balance) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_ana<-paste0('https://finance.yahoo.com/quote/',
                  ticker,'/analysis?p=',ticker)
  html_ana <- read_html(url_ana) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_ret<-paste0('https://www.officialdata.org/us/stocks/s-p-500/',
                  start,'?amount=100&endYear=',curYear)
  html_ret <- read_html(url_ret) %>% html_node('body') %>% 
    html_text() %>% toString()
  
  print("extracting Currency and Revenue")
  
  # extracts Years, gathers all text in the HTML_text between the specified passages
  # and creates a dataframe for the yearly data
  
  year <- qdapRegex::ex_between(html_fin, "financialsChart\":{\"yearly\":", "quarterly\":[{\"")[[1]]
  year <- qdapRegex::ex_between(year, "{\"date\":", ",\"revenue\"")[[1]]
  yearly_data <- data.frame(year)
  
  # extracts Currency
  
  curr <- qdapRegex::ex_between(html_fin, "financialCurrency\":\"", "\"},\"price")[[1]]
  curr <- data.frame(curr)
  yearly_data <- bind_cols(yearly_data, curr)
  
  # extracts revenue and renames it to Total revnenue
  
  rev <- qdapRegex::ex_between(html_fin, ",\"revenue\":{\"raw\":", ",\"fmt\":\"")[[1]]
  rev <- rev[1:4]
  rev <- as.numeric(rev)
  rev <- data.frame(rev)
  rev <- rev %>% 
    rename(
      TotalRevenue = rev    
    )
  yearly_data <- bind_cols(yearly_data, rev)
  
  print("extracting Total Cash Flow")
  
  # extracts Total Free Cashflow
  
  TCF <- qdapRegex::ex_between(html_cf, "\"totalCashFromOperatingActivities\":", "\"fmt\"")[[1]]
  TCF <- TCF[1:4]
  
  # replaces unwanted components of the vector 
  
  TCF <- gsub("{\"raw\":", "", TCF, fixed=T)
  TCF <- gsub(",", "", TCF, fixed=T)
  TCF <- as.numeric(TCF)
  
  # Yahoo Finance has the Dates in reveresed Ordern sometimes in reveresed order
  # rev() is used to  reverese the order into the needed form
  
  TCF<-rev(TCF)
  
  #TCF is renamed in the Data frame to TotalCashFlow
  
  TCF<-data.frame(TCF)
  TCF<-TCF %>% 
    rename(
      TotalCashFlow = TCF    
    )
  yearly_data <- bind_cols(yearly_data, TCF)    
  
  print("extracting Capital Expenditure")
  
  # extracts CAPEX (Capital Expenditure) 
  
  CAPEX <- qdapRegex::ex_between(html_cf, "\"capitalExpenditures\":", "\"fmt\"")[[1]]
  CAPEX <- CAPEX[1:4]
  CAPEX <- gsub("{\"raw\":", "", CAPEX, fixed=T)
  CAPEX <- gsub(",", "", CAPEX, fixed=T)
  CAPEX <- as.numeric(CAPEX)
  CAPEX <- rev(CAPEX)
  CAPEX <- data.frame(CAPEX)
  yearly_data <- bind_cols(yearly_data, CAPEX)
  
  # extracts NetIncome 
  
  print("extracting Net Income")
  
  NetIncome <- qdapRegex::ex_between(html_cf, "\"netIncome\":", "\"fmt\"")[[1]]
  NetIncome <- NetIncome[1:4]
  NetIncome <- gsub("{\"raw\":", "", NetIncome, fixed=T)
  NetIncome <- gsub(",", "", NetIncome, fixed=T)
  NetIncome <- as.numeric(NetIncome)
  NetIncome <- rev(NetIncome)
  NetIncome <- data.frame(NetIncome)
  yearly_data  <- bind_cols(yearly_data, NetIncome)
  
  #extracts 10 Year Bondrate (R_f) 
  
  print("extracting 10 Year Bond Rates")
  
  BR <- qdapRegex::ex_between(html_bonds, "Yield 10 Years", "0")[[1]]
  BR <- BR[1]
  BR <- as.numeric(BR)/100
  T0Data <- data.frame(BR)
  
  # extracts Beta
  
  print("extracting Beta")
  
  Beta <- qdapRegex::ex_between(html_stats, "(5Y Monthly)", "-")[[1]]
  Beta <- Beta[1]
  Beta <- as.numeric(Beta)
  Beta <- data.frame(Beta)
  T0Data <- bind_cols(T0Data,Beta)
  
  # extracts Market Cap
  
  print("extracting Market Cap")
  
  MarkCap <- qdapRegex::ex_between(html_stats, "trailingMarketCap", "\"fmt\":\"")[[1]]
  MarkCap <- qdapRegex::ex_between(MarkCap, "\"raw\":", ",")[[1]]
  MarkCap <- as.numeric(MarkCap)
  MarkCap <- data.frame(MarkCap)
  T0Data <- bind_cols(T0Data,MarkCap)
  
  # extracts Total Debt
  
  print("extracting Total Debt")
  
  TotDebt <- qdapRegex::ex_between(html_balance, "annualTotalDebt", "annual")[[1]]
  TotDebt <- str_split(TotDebt, "\"raw\"")
  TotDebt <- TotDebt[[1]]
  TotDebt <- rev(TotDebt)[1]
  TotDebt <- qdapRegex::ex_between(TotDebt, ":", ",\"fmt\"")[[1]]
  TotDebt <- as.numeric(TotDebt)
  TotDebt <- data.frame(TotDebt)
  T0Data <- bind_cols(T0Data,TotDebt)
  
  # extracts outstanding shares
  
  print("extracting Outstanding Shares")
  
  floatshares<-qdapRegex::ex_between(html_stats, "\"floatShares\":{\"raw\":", ",\"fmt\"")[[1]]
  floatshares<-as.numeric(floatshares)
  floatshares<-data.frame(floatshares)
  T0Data<-bind_cols(T0Data,floatshares)
  
  # extracts Interest Expense
  
  print("extracting Interest Expense")
  
  IntExpense <- qdapRegex::ex_between(html_fin, "interestExpense\":{\"raw\":", ",\"fmt\"")[[1]]
  IntExpense <- IntExpense[5]
  IntExpense <- as.numeric(IntExpense)
  IntExpense <- data.frame(IntExpense)
  T0Data <- bind_cols(T0Data,IntExpense)
  
  # extracts long term Debt
  # only the latest number is needed, because some Companys only show the latest
  # 3 years (ex. MSFT) the long Debt is reveresed and the first number is used
  # to extractr the current Debt
  
  print("extracting long term Debt")
  
  LongDebt <- qdapRegex::ex_between(html_balance, "LongTermDebt\":[{\"", "annual")[[1]]
  LongDebt <- qdapRegex::ex_between(LongDebt, "\"raw\":", ",\"fmt\":\"")[[1]]
  LongDebt <- rev(LongDebt)
  LongDebt <- LongDebt[1]
  LongDebt <- as.numeric(LongDebt)*-1
  LongDebt <- data.frame(LongDebt)
  T0Data <- bind_cols(T0Data,LongDebt)
  
  # extracts current Debt
  
  print("extracting current Debt")
  
  CurrDebt <- qdapRegex::ex_between(html_balance, "CurrentDebtAndCapitalLeaseObligation", "annual")[[1]]
  CurrDebt <- CurrDebt[2]
  CurrDebt <- qdapRegex::ex_between(CurrDebt, "\"raw\":", ",\"fmt\":\"")[[1]]
  CurrDebt <- rev(CurrDebt)
  CurrDebt <- CurrDebt[1]
  CurrDebt <- as.numeric(CurrDebt)*-1
  CurrDebt <- data.frame(CurrDebt)
  T0Data <- bind_cols(T0Data,CurrDebt)
  
  # extracts income tax expense
  
  print("extracting income tax expense")
  
  IncTaxEx <- qdapRegex::ex_between(html_balance, "\"incomeTaxExpense\":{\"raw\":", ",\"fmt\":")[[1]]
  IncTaxEx <- IncTaxEx[5]
  IncTaxEx <- as.numeric(IncTaxEx)
  IncTaxEx <- data.frame(IncTaxEx)
  T0Data <- bind_cols(T0Data,IncTaxEx)
  
  # extracts pre income tax
  
  print("extracting pre income tax")
  
  PreIncTax <- qdapRegex::ex_between(html_fin, "incomeBeforeTax\":{\"raw\":", ",\"fmt\"")[[1]]
  PreIncTax <- PreIncTax[5]
  PreIncTax <- as.numeric(PreIncTax)
  PreIncTax <- data.frame(PreIncTax)
  T0Data <- bind_cols(T0Data,PreIncTax)
  
  # extracts projected revenue and number of Analysts, number of Analysts 
  # can be used to determine the reliability of the forecast
  
  print("extracting Projected Revenue and number of Analysts")
  
  # extracts the avr. projected revenue for the coming 2 years and the number of 
  # Analysten for the projection
  
  ProjRev <- qdapRegex::ex_between(html_ana, "revenueEstimate", "numberOfAnalysts")[[1]]
  ProjRev <- ProjRev[3:4]
  ProjRev <- c(qdapRegex::ex_between(ProjRev, "\"avg\":{\"raw\":", ",\"fmt")[[1]],
               qdapRegex::ex_between(ProjRev, "\"avg\":{\"raw\":", ",\"fmt")[[2]] )
  ProjRev <- as.numeric(ProjRev)
  ProjRev <- data.frame(ProjRev)
  
  ProjAnalysts <- qdapRegex::ex_between(html_ana, "revenueEstimate", "yearAgoRevenue")[[1]]
  ProjAnalysts <- ProjAnalysts[3:4]
  ProjAnalysts <- c(qdapRegex::ex_between(ProjAnalysts, "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[1]],
                    qdapRegex::ex_between(ProjAnalysts, "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[2]] )
  ProjAnalysts <- as.numeric(ProjAnalysts)
  ProjAnalysts <- data.frame(ProjAnalysts)
  ProjRev <- bind_cols(ProjRev,ProjAnalysts)
  
  print(paste("extracting Average Return of S&P 500 over Timeframe of", timeframe, "years"))
  
  # extracts the avr. rate of return for the S$P 500 in the given timeframe
  
  AvRet <- qdapRegex::ex_between(html_ret, ", or", "% per year")[[1]]
  AvRet <- AvRet[1]
  AvRet <- as.numeric(AvRet)/100
  AvRet <- data.frame(AvRet)
  T0Data <- bind_cols(T0Data,AvRet)
  
  T0Data<-data.frame(T0Data,GloGrow)
  
  out <- list(
    yearly_data = yearly_data,
    T0Data = T0Data,
    ProjRev = ProjRev
  )
  
  return(out)
  
  # rm(list=setdiff(ls(), "yearly_data", "T0Data"))
  
}


DCF_calculation <- function(yearly_data = scraped_DCF_data[[1]]$yearly_data, 
                            T0Data = scraped_DCF_data[[1]]$T0Data,
                            ProjRev = scraped_DCF_data[[1]]$ProjRev$ProjRev) {
  
  # yearly_data <- scraped_DCF_data[[1]]$yearly_data
  # T0Data <- scraped_DCF_data[[1]]$T0Data
  # ProjRev <- scraped_DCF_data[[1]]$ProjRev$ProjRev
  
  # calculates free cash flow to equity for the current and past 3 years 
  # TM stand for T minus year
  
  FCFE <- c(yearly_data$TotalCashFlow[1]+yearly_data$CAPEX[1],
            yearly_data$TotalCashFlow[2]+yearly_data$CAPEX[2],
            yearly_data$TotalCashFlow[3]+yearly_data$CAPEX[3],
            yearly_data$TotalCashFlow[4]+yearly_data$CAPEX[4])
  
  FCFE <- data.frame(FCFE)
  yearly_data <- bind_cols(yearly_data,FCFE)
  
  
  # calculates FCFE per net income, a more conservative approach is wished.
  # because of that the lowest value is used for the calculation
  # and created the Dataframe T0_calculation
  
  FCFEpNetInc <- c(yearly_data$FCFE[1]/yearly_data$NetIncome[1],
                   yearly_data$FCFE[2]/yearly_data$NetIncome[2],
                   yearly_data$FCFE[3]/yearly_data$NetIncome[3],
                   yearly_data$FCFE[4]/yearly_data$NetIncome[4])
  FCFEpNetIncmin <- min(FCFEpNetInc)
  T0_cal <- data.frame(FCFEpNetIncmin)
  
  # creates the Dataframe Projection for the current and coming 3 years
  
  curYear<-format(Sys.Date())
  curYear<-year(curYear)
  
  Year <- c(curYear,curYear+1,curYear+2,curYear+3)
  Projection <- data.frame(Year)
  
  # calculates the Revenue growth rate and takes the mean for further 
  # calculations
  
  RevGrowthRate <- c((yearly_data$TotalRevenue[2]/yearly_data$TotalRevenue[1])-1,
                     (yearly_data$TotalRevenue[3]/yearly_data$TotalRevenue[2])-1,
                     (yearly_data$TotalRevenue[4]/yearly_data$TotalRevenue[3])-1,
                     (ProjRev[1]/yearly_data$TotalRevenue[4])-1,
                     (ProjRev[2]/ProjRev[1])-1)
  
  meanRevGrowthRate <- mean(RevGrowthRate)
  meanRevGrowthRate <- data.frame(meanRevGrowthRate)
  T0_cal<-bind_cols(T0_cal,meanRevGrowthRate)
  
  # takes the Revenue growth rate and projects the future Revenue
  
  Rev_Proj<-c(ProjRev,
              (ProjRev[2]*T0_cal$meanRevGrowthRate+ProjRev[2]),
              (ProjRev[2]*T0_cal$meanRevGrowthRate+ProjRev[2])
              *T0_cal$meanRevGrowthRate+(ProjRev[2]*
                                           T0_cal$meanRevGrowthRate+ProjRev[2]))
  
  Rev_Proj<-data.frame(Rev_Proj)
  Projection<-bind_cols(Projection,Rev_Proj)
  
  # calculates the net income margin and takes its mininum for a conservative
  # approach
  
  NetIncMarg <- c(yearly_data$NetIncome[1]/yearly_data$TotalRevenue[1],
                  yearly_data$NetIncome[2]/yearly_data$TotalRevenue[2],
                  yearly_data$NetIncome[3]/yearly_data$TotalRevenue[3],
                  yearly_data$NetIncome[4]/yearly_data$TotalRevenue[4])
  NetIncMargmin <- min(NetIncMarg)
  NetIncMargmin <- data.frame(NetIncMargmin)
  T0_cal<- bind_cols(T0_cal,NetIncMargmin)
  
  # Projects the future net income
  
  NetIncome_Proj <- c(Projection$Rev_Proj[1]*T0_cal$NetIncMargmin,
                      Projection$Rev_Proj[2]*T0_cal$NetIncMargmin,
                      Projection$Rev_Proj[3]*T0_cal$NetIncMargmin,
                      Projection$Rev_Proj[4]*T0_cal$NetIncMargmin)
  NetIncome_Proj <- data.frame(NetIncome_Proj)
  Projection <- bind_cols(Projection,NetIncome_Proj)
  
  # Projects the future free cash flow to equity
  
  FCFE_Proj <- c(Projection$NetIncome_Proj[1]*T0_cal$FCFEpNetIncmin,
                 Projection$NetIncome_Proj[2]*T0_cal$FCFEpNetIncmin,
                 Projection$NetIncome_Proj[3]*T0_cal$FCFEpNetIncmin,
                 Projection$NetIncome_Proj[4]*T0_cal$FCFEpNetIncmin)
  FCFE_Proj <- data.frame(FCFE_Proj)
  Projection <- bind_cols(Projection,FCFE_Proj)
  
  # Calculation of the Date for calculation of WACC (weighted avereage cost of return)
  
  # calculation of average rate and effective tax rate for cost of debt (r_d)
  
  AvRate <- T0Data$IntExpense/(T0Data$LongDebt+T0Data$CurrDebt)
  AvRate <- data.frame(AvRate)
  T0_cal <- bind_cols(T0_cal,AvRate)
  
  EffTaxRate <- T0Data$IncTaxEx/T0Data$PreIncTax
  EffTaxRate <- data.frame(EffTaxRate)
  T0_cal <- bind_cols(T0_cal,EffTaxRate)
  
  # Calculation of cost of debt r_d
  
  r_d <- T0_cal$AvRate*(1-T0_cal$EffTaxRate)
  r_d <- data.frame(r_d)
  T0_cal <- bind_cols(T0_cal,r_d)
  
  # Calculation of capital asseet pricing model R_a
  
  R_a <- T0Data$BR+T0Data$Beta*(T0Data$AvRet-T0Data$BR)
  R_a <- data.frame(R_a)
  T0_cal <- bind_cols(T0_cal,R_a)
  
  # Calculation of total amount of capital
  
  TotCap <- T0Data$MarkCap+T0Data$TotDebt
  TotCap <- data.frame(TotCap)  
  T0_cal <- bind_cols(T0_cal,TotCap)
  
  # Calculation of weighted Debt w_d and weighted equity w_e
  
  w_d <- T0Data$TotDebt/T0_cal$TotCap
  w_d <- data.frame(w_d)
  T0_cal <- bind_cols(T0_cal,w_d)
  
  w_e <- 1-T0_cal$w_d
  w_e <- data.frame(w_e)
  T0_cal <-bind_cols(T0_cal,w_e)
  
  # Calculation of WACC
  
  WACC <- T0_cal$w_d*T0_cal$r_d+T0_cal$w_e*T0_cal$R_a
  WACC <- data.frame(WACC)
  T0_cal<- bind_cols(T0_cal,WACC)
  
  TermValue <- (Projection$FCFE_Proj[4]*(1+T0Data$GloGrow))/(T0_cal$WACC-T0Data$GloGrow)
  TermValue <- data.frame(TermValue)
  T0_cal <- bind_cols(T0_cal,TermValue)
  
  DiscFac <- c((1+T0_cal$WACC)^1,(1+T0_cal$WACC)^2,(1+T0_cal$WACC)^2,(1+T0_cal$WACC)^2)
  DiscFac <- data.frame(DiscFac)
  Projection <- bind_cols(Projection,DiscFac)
  
  PreVoFCF <- c(Projection$FCFE_Proj[1]/Projection$DiscFac[1],
                Projection$FCFE_Proj[2]/Projection$DiscFac[2],
                Projection$FCFE_Proj[3]/Projection$DiscFac[3],
                Projection$FCFE_Proj[4]/Projection$DiscFac[4],
                T0_cal$TermValue/Projection$DiscFac[4])
  
  TodayVoFCF <- sum(PreVoFCF)          
  
  FVoEq <- round(TodayVoFCF/T0Data$floatshares)
  
  out <- list(
    T0_cal = T0_cal,
    FVoEq = FVoEq
  )
}



















