#assignes the url to the variable, with the paste0 function the url leads to 
#the url of the ticker
#afterwards the html text gets extracted and assigned to html variables




scrape_data <- function(ticker, GloGrow, timeframe) {
  # ticker <- .GlobalEnv$ticker
  
  # ticker<-"MBG.DE"
  # GlowGrow<-0.029
  # timeframe<-30

  
  curYear<-format(Sys.Date())
  curYear<-year(curYear)
  start<-curYear-timeframe
  
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
  
  curr <- qdapRegex::ex_between(html_fin, "financialCurrency\":\"", "\"},\"price")[[1]]
  
  rev <- sub(".*\"yearly\":*(.*?) *[\n|\r\n|\r]{2}", "\\1", html_fin)
  rev <- head(stringr::str_match_all(rev, "\\[(.*?)\\]")[[1]][, 2],1)
  splityear <- str_split(rev, "\\{\"date\":")
  splityear <- splityear[[1]]
  splityear<- paste("\\{\"date\":", splityear, sep="")
  if(length(splityear)>0){
    yearly_data <- data.frame(curr = curr,
                             year=str_extract(splityear, "\"date\":\"*(.*?) *\""),
                             TotalRevenue=str_extract(splityear, "\"revenue\":\\{\"raw\":*(.*?) *,"))
    yearly_data <- yearly_data[complete.cases(yearly_data), ]
    yearly_data <- data.frame(lapply(yearly_data, as.character), stringsAsFactors=FALSE)
    yearly_data <- yearly_data %>%
      separate(year, c("first", "year"), sep=":") %>% 
      select(-first)
    yearly_data <- yearly_data %>%
      separate(TotalRevenue, c("first", "second", "TotalRevenue"), sep=":") %>%
      select(-first, -second)
    yearly_data <- yearly_data %>%
      mutate(year=gsub("\"", "", year, fixed=T),
             year=gsub(",","",year, fixed=T),
             TotalRevenue=gsub(",", "", TotalRevenue, fixed=T))
  }
  yearly_data$TotalRevenue<-as.numeric(yearly_data$TotalRevenue)
  yearly_data$year<-as.numeric(yearly_data$year)
  
  
  print("extracting Total Cash Flow")
  
  #Entnimmt dem HTML Quelltext alle Textstellen zwischen den Angegeben Parametern
  
  TCF <- qdapRegex::ex_between(html_cf, "\"totalCashFromOperatingActivities\":", "\"fmt\"")[[1]]
  TCF <- TCF[1:4]
  
  #Eresetzt bzw. in diesem Fall löscht unerwünschte Bestandteile des Vektoren und
  #wandelt diesen in numerische Zahlen um
  
  TCF <- gsub("{\"raw\":", "", TCF, fixed=T)
  TCF <- gsub(",", "", TCF, fixed=T)
  TCF <- as.numeric(TCF)
  
  #Da Yahoo Finance die Jahre in einer Umgekehrten Reihenfolge darstellt, werden
  #die Daten folgend immer umgekehrt
  
  TCF<-rev(TCF)
  
  #TCF wird in einen Dataframe umgewandelt und der Column Name wird geändernt 
  #anschließend wird der Datafram yearly_data aus dem Total Rev. und dem TCF erstellt
  #an diesen werden im folgenden weitere Daten angehängt zudem wird TCD im Datensatz
  #zu Total Cash Flow umbenannt
  
  TCF<-data.frame(TCF)
  TCF<-TCF %>% 
    rename(
      TotalCashFlow = TCF    
    )
  yearly_data <- bind_cols(yearly_data, TCF)    
  
  print("extracting Capital Expenditure")
  
  #Die Variable CAPEX (Capital Expenditure) wird der Quelle entnommen und angepasst
  
  CAPEX <- qdapRegex::ex_between(html_cf, "\"capitalExpenditures\":", "\"fmt\"")[[1]]
  CAPEX <- CAPEX[1:4]
  CAPEX <- gsub("{\"raw\":", "", CAPEX, fixed=T)
  CAPEX <- gsub(",", "", CAPEX, fixed=T)
  CAPEX <- as.numeric(CAPEX)
  CAPEX <- rev(CAPEX)
  CAPEX <- data.frame(CAPEX)
  yearly_data <- bind_cols(yearly_data, CAPEX)
  
  #Die Variable NetIncome (Capital Expenditure) wird der Quelle entnommen und angepasst
 
  print("extracting Net Income")
   
  NetIncome <- qdapRegex::ex_between(html_cf, "\"netIncome\":", "\"fmt\"")[[1]]
  NetIncome <- NetIncome[1:4]
  NetIncome <- gsub("{\"raw\":", "", NetIncome, fixed=T)
  NetIncome <- gsub(",", "", NetIncome, fixed=T)
  NetIncome <- as.numeric(NetIncome)
  NetIncome <- rev(NetIncome)
  NetIncome <- data.frame(NetIncome)
  yearly_data  <- bind_cols(yearly_data, NetIncome)
  
  #R_f oder auch 10 Year Bondrate  wird extrahiert, diese ist für alle Aktien gleich
  
  print("extracting 10 Year Bond Rates")
  
  BR<-qdapRegex::ex_between(html_bonds, "Yield 10 Years", "0")[[1]]
  BR<-BR[1]
  BR<-as.numeric(BR)/100
  T0Data<-data.frame(BR)
  
  #Entnimmt der Statistics Seite von Yahoo, den HTML Text und bereinigt diesen bis
  #nur noch der Beta Wert vorhanden ist
  
  print("extracting Beta")
  
  Beta<-qdapRegex::ex_between(html_stats, "(5Y Monthly)", "-")[[1]]
  Beta<-Beta[1]
  Beta<-as.numeric(Beta)
  Beta<-data.frame(Beta)
  T0Data<-bind_cols(T0Data,Beta)
  
  print("extracting Market Cap")
  
  MarkCap<- qdapRegex::ex_between(html_stats, "trailingMarketCap", "\"fmt\":\"")[[1]]
  MarkCap<- qdapRegex::ex_between(MarkCap, "\"raw\":", ",")[[1]]
  MarkCap<-as.numeric(MarkCap)
  MarkCap<-data.frame(MarkCap)
  T0Data<-bind_cols(T0Data,MarkCap)
  
  print("extracting Ttotal Debt")
  
  TotDebt <-qdapRegex::ex_between(html_balance, "annualTotalDebt", "annual")[[1]]
  TotDebt <- str_split(TotDebt, "\"raw\"")
  TotDebt <- TotDebt[[1]]
  TotDebt <- rev(TotDebt)[1]
  TotDebt <- qdapRegex::ex_between(TotDebt, ":", ",\"fmt\"")[[1]]
  TotDebt <- as.numeric(TotDebt)
  TotDebt <- data.frame(TotDebt)
  T0Data<-bind_cols(T0Data,TotDebt)
  
  print("extracting Outstanding Shares")
  
  floatshares<-qdapRegex::ex_between(html_stats, "\"floatShares\":{\"raw\":", ",\"fmt\"")[[1]]
  floatshares<-as.numeric(floatshares)
  floatshares<-data.frame(floatshares)
  T0Data<-bind_cols(T0Data,floatshares)
  
  print("extracting Interest Expense")
  
  IntExpense<-qdapRegex::ex_between(html_fin, "interestExpense\":{\"raw\":", ",\"fmt\"")[[1]]
  IntExpense<-IntExpense[5]
  IntExpense<-as.numeric(IntExpense)
  IntExpense<-data.frame(IntExpense)
  T0Data <-bind_cols(T0Data,IntExpense)
  
  #Da manche Aktien nur drei Jahre im balance Sheet angeben (bsp. MSFT), wurde mit rev LongDebt 
  #umgekehrt und dann die erste Zahl entnommen, um die aktuelen Schulden zu extrahieren
  
  print("extracting Long Termb Debt")
  
   LongDebt<-qdapRegex::ex_between(html_balance, "LongTermDebt\":[{\"", "annual")[[1]]
  LongDebt<-qdapRegex::ex_between(LongDebt, "\"raw\":", ",\"fmt\":\"")[[1]]
  LongDebt<-rev(LongDebt)
  LongDebt<-LongDebt[1]
  LongDebt<-as.numeric(LongDebt)*-1
  LongDebt<-data.frame(LongDebt)
  T0Data <-bind_cols(T0Data,LongDebt)
  
  print("extracting Curent Debt")
  
  CurrDebt<-qdapRegex::ex_between(html_balance, "CurrentDebtAndCapitalLeaseObligation", "annual")[[1]]
  CurrDebt<-CurrDebt[2]
  CurrDebt<-qdapRegex::ex_between(CurrDebt, "\"raw\":", ",\"fmt\":\"")[[1]]
  CurrDebt<-rev(CurrDebt)
  CurrDebt<-CurrDebt[1]
  CurrDebt<-as.numeric(CurrDebt)*-1
  CurrDebt<-data.frame(CurrDebt)
  T0Data <-bind_cols(T0Data,CurrDebt)
  
  print("Income Tax Expense")
  
  IncTaxEx<-qdapRegex::ex_between(html_balance, "\"incomeTaxExpense\":{\"raw\":", ",\"fmt\":")[[1]]
  IncTaxEx<-IncTaxEx[5]
  IncTaxEx<-as.numeric(IncTaxEx)
  IncTaxEx<-data.frame(IncTaxEx)
  T0Data <-bind_cols(T0Data,IncTaxEx)
  
  print("extracting Pre Income Taxt")
  
  PreIncTax<-qdapRegex::ex_between(html_fin, "incomeBeforeTax\":{\"raw\":", ",\"fmt\"")[[1]]
  PreIncTax<-PreIncTax[5]
  PreIncTax<-as.numeric(PreIncTax)
  PreIncTax<-data.frame(PreIncTax)
  T0Data <-bind_cols(T0Data,PreIncTax)
  
  print("extracting Projected Revenue and number of Analysts")
  
  #Entnimmt die erwarteten Umsätze der kommenden zwei Jahre, sowie die Anzahl an 
  #Analysten Achtung akutell wird der niedrige Erwartete Wert genutzt
  
  ProjRev<-qdapRegex::ex_between(html_ana, "revenueEstimate", "numberOfAnalysts")[[1]]
  ProjRev<-ProjRev[3:4]
  ProjRev<-c(qdapRegex::ex_between(ProjRev, "\"low\":{\"raw\":", ",\"fmt")[[1]],
             qdapRegex::ex_between(ProjRev, "\"low\":{\"raw\":", ",\"fmt")[[2]] )
  ProjRev <- as.numeric(ProjRev)
  ProjRev <- data.frame(ProjRev)
  
  ProjAnalysts<-qdapRegex::ex_between(html_ana, "revenueEstimate", "yearAgoRevenue")[[1]]
  ProjAnalysts<-ProjAnalysts[3:4]
  ProjAnalysts<-c(qdapRegex::ex_between(ProjAnalysts, "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[1]],
                  qdapRegex::ex_between(ProjAnalysts, "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[2]] )
  ProjAnalysts<-data.frame(ProjAnalysts)
  ProjRev<-bind_cols(ProjRev,ProjAnalysts)
  
  print(paste("extracting Average Return of S&P 500 over Timeframe of", timeframe))
  
  #Entnimmt der Website den Average Return des S&P 500 der letzen 30 Jahre
  
  AvRet<-qdapRegex::ex_between(html_ret, ", or", "% per year")[[1]]
  AvRet<-AvRet[1]
  AvRet<-as.numeric(AvRet)/100
  AvRet<-data.frame(AvRet)
  T0Data<-bind_cols(T0Data,AvRet)
  
  T0Data<-data.frame(T0Data,GloGrow)
  
  out <- list(
    yearly_data = yearly_data,
    T0Data = T0Data,
    ProjRev = ProjRev
  )
  
  return(out)

}



