# creates a function to acquire the needed Data for the Calculation of
# Fair Value of Equity with the DCF Method
# the needed Variables are the ticker the expected Growth of the global 
# economy and the timeframe for the average Return of the S%P500 of the past

DCF_data_scraper <- function(ticker, GloGrow = 0.029, timeframe = 30) {
  
  ticker <- "aapl"
  GloGrow <- 0.029
  timeframe <- 30
  
  # current Year and start Date for the exrtaction of the av. Return of S&P500
  
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
  url_stats <- paste0('https://finance.yahoo.com/quote/',
                      ticker, '/key-statistics?p=', ticker)
  html_stats <- read_html(url_stats) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_balance <- paste0('https://finance.yahoo.com/quote/',
                        ticker, '/balance-sheet?p=', ticker)
  html_balance <- read_html(url_balance) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_ana <- paste0('https://finance.yahoo.com/quote/',
                    ticker,'/analysis?p=',ticker)
  html_ana <- read_html(url_ana) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_ret <- paste0('https://www.officialdata.org/us/stocks/s-p-500/',
                    start,'?amount=100&endYear=',curYear)
  html_ret <- read_html(url_ret) %>% html_node('body') %>% 
    html_text() %>% toString()
  
  print("extracting Revenue")
  
  # extracts Years, gathers all text in the HTML_text between the specified passages
  # and creates a dataframe for the yearly data
  
  Year <- qdapRegex::ex_between(html_fin, "financialsChart\":{\"yearly\":", "quarterly\":[{\"")[[1]]
  Year <- qdapRegex::ex_between(Year, "{\"date\":", ",\"revenue\"")[[1]]
  yearly_data <- data.frame(Year)
  
  # extracts revenue and renames it to Total revnenue
  
  TotalRevenue <- qdapRegex::ex_between(html_fin, ",\"revenue\":{\"raw\":", ",\"fmt\":\"")[[1]]
  TotalRevenue <- TotalRevenue[1:4]
  TotalRevenue <- as.numeric(TotalRevenue)
  TotalRevenue <- data.frame(TotalRevenue)
  
  yearly_data <- bind_cols(yearly_data, TotalRevenue )
  
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
  
  
  # extracts EBIT
  
  print("extractin EBIT")
  
  EBIT <- qdapRegex::ex_between(html_fin, "annualTotalOperatingIncomeAsReported", "annual")[[1]]
  EBIT <- qdapRegex::ex_between(EBIT, "{\"raw\":", ",\"fmt\"")[[1]]
  EBIT <- as.numeric(EBIT)
  EBIT <- data.frame(EBIT)
  yearly_data <- bind_cols(yearly_data, EBIT)
  
  # extracts income tax expense
  
  print("extracting income tax expense")
  
  IncTaxEx <- qdapRegex::ex_between(html_balance, "\"incomeTaxExpense\":{\"raw\":", ",\"fmt\":")[[1]]
  IncTaxEx <- rev(IncTaxEx)
  IncTaxEx <- IncTaxEx[1:4]
  IncTaxEx <- as.numeric(IncTaxEx)
  IncTaxEx <- data.frame(IncTaxEx)
  yearly_data <- bind_cols(yearly_data,IncTaxEx)
  
  # extracts Depreciation & Amortization (D&A)
  
  print("extracting Depreciation & Amortization")
  
  DandA <- qdapRegex::ex_between(html_fin, "annualReconciledDepreciation", "trailing")[[1]]
  DandA <- qdapRegex::ex_between(DandA, "\"raw\":", ",\"fmt\":")[[1]]
  DandA <- as.numeric(DandA)
  DandA <- data.frame(DandA)
  yearly_data <- bind_cols(yearly_data,DandA)
  
  print("extracting Capital Expenditure")
  
  # extracts CAPEX (Capital Expenditure) 
  
  CAPEX <- qdapRegex::ex_between(html_cf, "\"capitalExpenditures\":", "\"fmt\"")[[1]]
  CAPEX <- CAPEX[1:4]
  CAPEX <- gsub("{\"raw\":", "", CAPEX, fixed=T)
  CAPEX <- gsub(",", "", CAPEX, fixed=T)
  CAPEX <- as.numeric(CAPEX)
  CAPEX <- CAPEX*-1
  CAPEX <- rev(CAPEX)
  CAPEX <- data.frame(CAPEX)
  yearly_data <- bind_cols(yearly_data, CAPEX)
  
  # extracts Change in net working capital (NWC)
  
  cNWC <- qdapRegex::ex_between(html_cf, "annualChangeInWorkingCapital", "trailing")[[1]]
  cNWC <- qdapRegex::ex_between(cNWC, "\"raw\":", ",\"fmt\":")[[1]]
  cNWC <-  cNWC[1:4]
  cNWC <- as.numeric(cNWC)
  cNWC <- data.frame(cNWC)
  yearly_data <- bind_cols(yearly_data, cNWC)
  
 
  
  # extracts fiscal year end and calculates the percantage of the year in which 
  # the month lays
  
  # calnderization 
  
  fye <- qdapRegex::ex_between(html_stats, "lastFiscalYearEnd", "heldPercentInstitutions")[[1]]
  fyemonth <- qdapRegex::ex_between(fye, "-", "-")[[1]]
  fyemonth <- as.numeric(fyemonth)
  rateofffye <- fyemonth/12
  T0Data <- data.frame(rateofffye)
  
  
  
  ## aktueller stand der neu bearbeitung ##
  
  
  
  print("extracting Total Cash Flow")

  

  # extracts Total Cash Flow from operating activities 
  
  TCF <- qdapRegex::ex_between(html_cf, "\"totalCashFromOperatingActivities\":", "\"fmt\"")[[1]]
  TCF <- TCF[1:4]
  
  # replaces unwanted components of the vector 
  
  TCF <- gsub("{\"raw\":", "", TCF, fixed=T)
  TCF <- gsub(",", "", TCF, fixed=T)
  TCF <- as.numeric(TCF)
  
  # Yahoo Finance has the Dates in reveresed Ordern sometimes in reveresed order
  # rev() is used to  reverese the order into the needed form
  
  TCF<-rev(TCF)
  
  # TCF is renamed in the Data frame to TotalCashFlow
  
  TCF <- data.frame(TCF)
  TCF <- TCF %>% 
    rename(
      TotalCashFlow = TCF    
    )
  yearly_data <- bind_cols(yearly_data, TCF)    
  

  
  
  # extracts NetIncome 
  
  print("extracting Net Income")
  
  NetIncome <- qdapRegex::ex_between(html_cf, "\"netIncome\":", "\"fmt\"")[[1]]
  NetIncome <- NetIncome[1:4]
  NetIncome <- gsub("{\"raw\":", "", NetIncome, fixed=T)
  NetIncome <- gsub(",", "", NetIncome, fixed=T)
  NetIncome <- as.numeric(NetIncome)
  NetIncome <- rev(NetIncome)
  NetIncome <- data.frame(NetIncome)
  yearly_data <- bind_cols(yearly_data, NetIncome)
  
  #extracts 10 Year Bondrate (R_f) 
  
  print("extracting 10 Year Bond Rates")
  
  BR <- qdapRegex::ex_between(html_bonds, "Yield 10 Years", "0")[[1]]
  BR <- BR[1]
  BR <- as.numeric(BR)/100
  T0Data <- data.frame(T0Data, BR)
  
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
  
  floatshares <- qdapRegex::ex_between(html_stats, "\"floatShares\":{\"raw\":", ",\"fmt\"")[[1]]
  floatshares <- as.numeric(floatshares)
  floatshares <- data.frame(floatshares)
  T0Data <- bind_cols(T0Data,floatshares)
  
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
  LongDebt <- as.numeric(LongDebt)
  LongDebt <- data.frame(LongDebt)
  T0Data <- bind_cols(T0Data,LongDebt)
  
  # extracts current Debt
  
  print("extracting current Debt")
  
  CurrDebt <- qdapRegex::ex_between(html_balance, "CurrentDebtAndCapitalLeaseObligation", "annual")[[1]]
  CurrDebt <- CurrDebt[2]
  CurrDebt <- qdapRegex::ex_between(CurrDebt, "\"raw\":", ",\"fmt\":\"")[[1]]
  CurrDebt <- rev(CurrDebt)
  CurrDebt <- CurrDebt[1]
  CurrDebt <- as.numeric(CurrDebt)
  CurrDebt <- data.frame(CurrDebt)
  T0Data <- bind_cols(T0Data,CurrDebt)
  

  # extracts pre income tax
  
  print("extracting pre income tax")
  
  PreIncTax <- qdapRegex::ex_between(html_fin, "incomeBeforeTax\":{\"raw\":", ",\"fmt\"")[[1]]
  PreIncTax <- PreIncTax[5]
  PreIncTax <- as.numeric(PreIncTax)
  PreIncTax <- data.frame(PreIncTax)
  T0Data <- bind_cols(T0Data,PreIncTax)
  
  # extracts projected revenue and number of Analysts, number of Analysts 
  # can be used to determine the reliability of the forecast
  
 
  
  print(paste("extracting Average Return of S&P 500 over Timeframe of", timeframe, "years"))
  
  # extracts the avr. rate of return for the S$P 500 in the given timeframe
  
  AvRet <- qdapRegex::ex_between(html_ret, ", or", "% per year")[[1]]
  AvRet <- AvRet[1]
  AvRet <- as.numeric(AvRet)/100
  AvRet <- data.frame(AvRet)
  T0Data <- bind_cols(T0Data,AvRet)
  
  T0Data<-data.frame(T0Data,GloGrow)
  
  # extracts Net Debt 
  
  NetBorr <- qdapRegex::ex_between(html_cf, "annualNetIssuancePaymentsOfDebt", "trailing")[[1]]
  NetBorr <- qdapRegex::ex_between(NetBorr, "{\"raw\":", ",\"fmt\"")[[1]]
  NetBorr <- NetBorr[1:4]
  NetBorr <- as.numeric(NetBorr)
  NetBorr <- data.frame(NetBorr)
  yearly_data <- bind_cols(yearly_data, NetBorr)
  
  

  
  out <- list(
    yearly_data = yearly_data,
    T0Data = T0Data,
    ProjRev = ProjRev
  )
  
  return(out)
}

