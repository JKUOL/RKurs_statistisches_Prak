# creates a function to acquire the needed Data for the Calculation of
# Fair Value of Equity with the DCF Method
# the needed Variables are the ticker the expected TGR of the global 
# economy and the timeframe for the average Return of the S%P500 of the past

DCF_data_scraper <- function(ticker, TGR = 0.025, timeframe = 30) {
  
  # ticker <- "VZ"
  # TGR <- 0.025
  # timeframe <- 30
  
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
  
  url_risk_prem<-read_html('https://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/ctryprem.html')
  
  # because qdapRegex::ex_between seems to not work on the out html_text
  # another node was chosen which outputs the table shown on the url
  
  url_risk_prem <- url_risk_prem %>% html_nodes("td") %>%
    html_text()
  
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
  
  EBIT <- qdapRegex::ex_between(html_fin, "annualOperatingIncome", "annual")[[1]]
  EBIT <- qdapRegex::ex_between(EBIT, "{\"raw\":", ",\"fmt\"")[[1]]
  EBIT <- as.numeric(EBIT[1:4])
  EBIT <- data.frame(EBIT)
  yearly_data <- bind_cols(yearly_data, EBIT)
  
  # extracts income tax expense
  
  print("extracting income tax expense")
  
  IncTaxEx <- qdapRegex::ex_between(html_balance, "\"incomeTaxExpense\":{\"raw\":", ",\"fmt\":")[[1]]
  IncTaxEx <- rev(IncTaxEx[5:8])
  IncTaxEx <- as.numeric(IncTaxEx)
  IncTaxEx <- data.frame(IncTaxEx)
  yearly_data <- bind_cols(yearly_data,IncTaxEx)
  
  # extracts Depreciation & Amortization (D&A)
  
  print("extracting Depreciation & Amortization")
  
  DandA <- qdapRegex::ex_between(html_fin, "annualReconciledDepreciation", "trailing")[[1]]
  DandA <- qdapRegex::ex_between(DandA, "\"raw\":", ",\"fmt\":")[[1]]
  DandA <- DandA[1:4]
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
  
  # extracts Market Cap
  
  print("extracting Market Cap")
  
  MarkCap <- qdapRegex::ex_between(html_stats, "trailingMarketCap", "\"fmt\":\"")[[1]]
  MarkCap <- qdapRegex::ex_between(MarkCap, "\"raw\":", ",")[[1]]
  MarkCap <- as.numeric(MarkCap)
  MarkCap <- data.frame(MarkCap)
  T0Data <- bind_cols(T0Data,MarkCap)
  
  #extracts 10 Year Bondrate (R_f (risc free rate))
  
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
  
 # extracts equity risk premium from the usa 
  
  erp <- url_risk_prem[1103]
  erp <- gsub("%", "", erp, fixed=T)
  erp <- as.numeric(erp)/100
  erp <- data.frame(erp)
  T0Data <- bind_cols(T0Data,erp)
  
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
  
  # extracts long term Debt
  
  print("extracting long term Debt")
  
  LongDebt <- qdapRegex::ex_between(html_balance, "longTermDebt\"", "inventory")[[1]]
  LongDebt <- qdapRegex::ex_between(LongDebt, "\"raw\":", ",\"fmt\":\"")[[1]]
  LongDebt <- LongDebt[1]
  LongDebt <- as.numeric(LongDebt)
  LongDebt <- data.frame(LongDebt)
  T0Data <- bind_cols(T0Data,LongDebt)
  
  
  # extracts Interest Expense
  
  print("extracting Interest Expense")
  
  IntExpense <- qdapRegex::ex_between(html_fin, "annualInterestExpense", "annual")[[1]]
  IntExpense <- qdapRegex::ex_between(IntExpense, "{\"raw\":", ",\"fmt\"")[[1]]
  IntExpense <- rev(IntExpense)
  IntExpense <- IntExpense[1]
  IntExpense <- as.numeric(IntExpense)
  IntExpense <- data.frame(IntExpense)
  T0Data <- bind_cols(T0Data,IntExpense)
  
  # extracts outstanding shares
  
  print("extracting Outstanding Shares")
  
  floatshares <- qdapRegex::ex_between(html_stats, "\"floatShares\":{\"raw\":", ",\"fmt\"")[[1]]
  floatshares <- as.numeric(floatshares)
  floatshares <- data.frame(floatshares)
  T0Data <- bind_cols(T0Data,floatshares)

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
  
  T0Data<-data.frame(T0Data,TGR)
  
  # extracts Net Debt 
  
  NetBorr <- qdapRegex::ex_between(html_cf, "annualNetIssuancePaymentsOfDebt", "trailing")[[1]]
  NetBorr <- qdapRegex::ex_between(NetBorr, "{\"raw\":", ",\"fmt\"")[[1]]
  NetBorr <- NetBorr[1:4]
  NetBorr <- as.numeric(NetBorr)
  NetBorr <- data.frame(NetBorr)
  yearly_data <- bind_cols(yearly_data, NetBorr)
  
  cash <- NetBorr <- qdapRegex::ex_between(html_balance, "otherAssets", "total")[[1]]
  cash <- NetBorr <- qdapRegex::ex_between(html_balance, "\"cash\":{\"raw\":", ",\"fmt\":")[[1]]
  cash <- cash[1]
  cash <- as.numeric(cash)
  cash <- data.frame(cash)
  T0Data <- bind_cols(T0Data, cash)
  
  out <- list(
    yearly_data = yearly_data,
    T0Data = T0Data,
    ProjRev = ProjRev
  )
  
  return(out)
}

