# Erstellt eine Funktion, welche die benötigten Daten für die Berechnung des
# Fair Values der Aktien läd die benötigten Variablen sind die Ticker, sowie 
# die TGR (Terminal Growth Rate) diese ist das angenommene konstante Wachstum 
# mit welche das Unternehmen für immer wächst

DCF_data_scraper <- function(ticker, TGR = 0.025) {
  
  # es werden diverese URLs  von yahoo mit dem ticker verknüpft und anschließend
  # die HTML Text zugewiesen
  url_profile <- paste0('https://finance.yahoo.com/quote/', ticker)
  
  # Druckt Statments, damit der User weiß an welchem Punkt sich der scraper 
  # befindet
  print("assingning urls and loading HTML")
  
  url_fin <- paste0('https://finance.yahoo.com/quote/', 
                    ticker, '/financials?p=', ticker)
  html_fin <- read_html(url_fin) %>% html_node('body') %>% 
    html_text() %>% toString()
  url_cf <- paste0('https://finance.yahoo.com/quote/',
                   ticker, '/cash-flow?p=', ticker)
  html_cf <- read_html(url_cf) %>% html_node('body') %>% html_text() %>% 
    toString()
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
  
  # ließt HTML Text der URL für das equity risk premium
  url_risk_prem<-read_html(
    'https://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/ctryprem.html')
  
  # da es bei diesem HTML Text Probleme mit qdapRegex gibt wird das 
  # equity risk premiummit filfe eines HTML Nodes extrahiert, welcher die
  # Tabelle auf der Website ausgibt
  url_risk_prem <- url_risk_prem %>% html_nodes("td") %>%
    html_text()
  
  # extrahiert die Jahre in welcher die Finacel Statments herrausgegeben wurden
  # und erstellt einen data frame daraus
  Year <- qdapRegex::ex_between(html_fin, "financialsChart\":{\"yearly\":", 
                                "quarterly\":[{\"")[[1]]
  Year <- qdapRegex::ex_between(Year, "{\"date\":",
                                ",\"revenue\"")[[1]]
  yearly_data <- data.frame(Year)
  
  # extrahiert den Revenue 
  print("extracting Revenue")
  
  TotalRevenue <- qdapRegex::ex_between(html_fin, ",\"revenue\":{\"raw\":",
                                        ",\"fmt\":\"")[[1]]
  TotalRevenue <- TotalRevenue[1:4]
  TotalRevenue <- as.numeric(TotalRevenue)
  TotalRevenue <- data.frame(TotalRevenue)
  
  yearly_data <- bind_cols(yearly_data, TotalRevenue )
  
  print("extracting Projected Revenue and number of Analysts")
  # extrahiert den avg. prohected revenue für die kommenden 2 Jahre und die 
  # Anzahl der Analysten, welche diese Vorhersage getroffen haben
  
  ProjRev <- qdapRegex::ex_between(html_ana, "revenueEstimate",
                                   "numberOfAnalysts")[[1]]
  ProjRev <- ProjRev[3:4]
  ProjRev <- c(qdapRegex::ex_between(ProjRev, "\"avg\":{\"raw\":",
                                     ",\"fmt")[[1]],
               qdapRegex::ex_between(ProjRev, "\"avg\":{\"raw\":",
                                     ",\"fmt")[[2]] )
  ProjRev <- as.numeric(ProjRev)
  ProjRev <- data.frame(ProjRev)
  
  ProjAnalysts <- qdapRegex::ex_between(html_ana, "revenueEstimate",
                                        "yearAgoRevenue")[[1]]
  ProjAnalysts <- ProjAnalysts[3:4]
  ProjAnalysts <- c(qdapRegex::ex_between(ProjAnalysts,
                                          "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[1]],
                    qdapRegex::ex_between(ProjAnalysts, 
                                          "\"numberOfAnalysts\":{\"raw\":", ",\"fmt")[[2]])
  ProjAnalysts <- as.numeric(ProjAnalysts)
  ProjAnalysts <- data.frame(ProjAnalysts)
  ProjRev <- bind_cols(ProjRev,ProjAnalysts)
  
  # Extrahiert den EBIT
  print("extracting EBIT")
  
  EBIT <- qdapRegex::ex_between(html_fin, "annualOperatingIncome",
                                "annual")[[1]]
  EBIT <- qdapRegex::ex_between(EBIT, "{\"raw\":",
                                ",\"fmt\"")[[1]]
  EBIT <- as.numeric(EBIT[1:4])
  EBIT <- data.frame(EBIT)
  yearly_data <- bind_cols(yearly_data, EBIT)
  
  # Extrahier den Income Tax Expense
  print("extracting income tax expense")
  
  IncTaxEx <- qdapRegex::ex_between(html_balance,
                                    "\"incomeTaxExpense\":{\"raw\":", ",\"fmt\":")[[1]]
  IncTaxEx <- rev(IncTaxEx[5:8])
  IncTaxEx <- as.numeric(IncTaxEx)
  IncTaxEx <- data.frame(IncTaxEx)
  yearly_data <- bind_cols(yearly_data,IncTaxEx)
  
  # extrahiert Depreciation & Amortization (D&A)
  print("extracting Depreciation & Amortization")
  
  DandA <- qdapRegex::ex_between(html_fin, "annualReconciledDepreciation",
                                 "trailing")[[1]]
  DandA <- qdapRegex::ex_between(DandA, "\"raw\":",
                                 ",\"fmt\":")[[1]]
  DandA <- DandA[1:4]
  DandA <- as.numeric(DandA)
  DandA <- data.frame(DandA)
  yearly_data <- bind_cols(yearly_data,DandA)
  
  print("extracting Capital Expenditure")
  
  # extrahiert CAPEX (Capital Expenditure) 
  CAPEX <- qdapRegex::ex_between(html_cf, "\"capitalExpenditures\":",
                                 "\"fmt\"")[[1]]
  CAPEX <- CAPEX[1:4]
  CAPEX <- gsub("{\"raw\":", "", CAPEX, fixed=T)
  CAPEX <- gsub(",", "", CAPEX, fixed=T)
  CAPEX <- as.numeric(CAPEX)
  CAPEX <- CAPEX*-1
  CAPEX <- rev(CAPEX)
  CAPEX <- data.frame(CAPEX)
  yearly_data <- bind_cols(yearly_data, CAPEX)
  
  # extrahiert NWC () Changein net working capital)
  cNWC <- qdapRegex::ex_between(html_cf, "annualChangeInWorkingCapital",
                                "trailing")[[1]]
  cNWC <- qdapRegex::ex_between(cNWC, "\"raw\":", ",\"fmt\":")[[1]]
  cNWC <-  cNWC[1:4]
  cNWC <- as.numeric(cNWC)
  cNWC <- data.frame(cNWC)
  yearly_data <- bind_cols(yearly_data, cNWC)
  
  # extrahiert das ende des Fiskaljahres des Unternehmens und berechnet den 
  # aktuellen Prozensatz für die später folgende calenderization
  fye <- qdapRegex::ex_between(html_stats, "lastFiscalYearEnd",
                               "heldPercentInstitutions")[[1]]
  fyemonth <- qdapRegex::ex_between(fye, "-", "-")[[1]]
  fyemonth <- as.numeric(fyemonth)
  rateofffye <- fyemonth/12
  
  # erstellent den data frame T0Data (für aktuelle nicht wiederholende Daten)
  T0Data <- data.frame(rateofffye)
  
  # extrahiert das Market Cap
  print("extracting Market Cap")
  
  MarkCap <- qdapRegex::ex_between(html_stats, "trailingMarketCap",
                                   "\"fmt\":\"")[[1]]
  MarkCap <- qdapRegex::ex_between(MarkCap, "\"raw\":", ",")[[1]]
  MarkCap <- as.numeric(MarkCap)
  MarkCap <- data.frame(MarkCap)
  T0Data <- bind_cols(T0Data,MarkCap)
  
  # extrahiert die 10 Year Bondrate (R_f (risc free rate))
  print("extracting 10 Year Bond Rates")
  
  BR <- qdapRegex::ex_between(html_bonds, "Yield 10 Years", "0")[[1]]
  BR <- BR[1]
  BR <- as.numeric(BR)/100
  T0Data <- data.frame(T0Data, BR)
  
  # extrahiert die Beta
  print("extracting Beta")
  
  Beta <- qdapRegex::ex_between(html_stats, "(5Y Monthly)", "-")[[1]]
  Beta <- Beta[1]
  Beta <- as.numeric(Beta)
  Beta <- data.frame(Beta)
  T0Data <- bind_cols(T0Data,Beta)
  
  # extrahiert das equity risk premium der USA 
  print("extracting equity risk premium")
  
  erp <- url_risk_prem[1103]
  erp <- gsub("%", "", erp, fixed=T)
  erp <- as.numeric(erp)/100
  erp <- data.frame(erp)
  T0Data <- bind_cols(T0Data,erp)
  
  # extrahiert current Debt
  print("extracting current Debt")
  
  CurrDebt <- qdapRegex::ex_between(html_balance,
                                    "CurrentDebtAndCapitalLeaseObligation", "annual")[[1]]
  CurrDebt <- CurrDebt[2]
  CurrDebt <- qdapRegex::ex_between(CurrDebt, "\"raw\":", ",\"fmt\":\"")[[1]]
  CurrDebt <- rev(CurrDebt)
  CurrDebt <- CurrDebt[1]
  CurrDebt <- as.numeric(CurrDebt)
  CurrDebt <- data.frame(CurrDebt)
  T0Data <- bind_cols(T0Data,CurrDebt)
  
  # extrahiert die long term Debt
  print("extracting long term Debt")
  
  LongDebt <- qdapRegex::ex_between(html_balance, "longTermDebt\"",
                                    "inventory")[[1]]
  LongDebt <- qdapRegex::ex_between(LongDebt, "\"raw\":",
                                    ",\"fmt\":\"")[[1]]
  LongDebt <- LongDebt[1]
  LongDebt <- as.numeric(LongDebt)
  LongDebt <- data.frame(LongDebt)
  T0Data <- bind_cols(T0Data,LongDebt)
  
  
  # extrahiert die Interest Expense
  print("extracting Interest Expense")
  
  # da es bei Yahoo Finances bei unterschiedlichen Unternehmen dazu kommt, 
  # dass unterschiedliche Überschriften, für die gleichen Werte genutzt werden
  # wird hier zunächst nach dem Standard gesucht, falls dieser nicht verfügbar 
  # ist, wird eine if Schleife genutzt.
  # Diese extrahiert von einer anderen Stelle, falls der Wert der ersten
  # extraktion NA ist
  IntExpense <- qdapRegex::ex_between(html_fin, "annualInterestExpense",
                                      "annual")[[1]]
  IntExpense <- qdapRegex::ex_between(IntExpense, "{\"raw\":",
                                      ",\"fmt\"")[[1]]
  IntExpense <- rev(IntExpense)
  IntExpense <- IntExpense[1]
  IntExpense <- as.numeric(IntExpense)
  
  if (is.na(IntExpense) == TRUE) {
    IntExpense <- qdapRegex::ex_between(html_fin,
                                        "NonOperatingInterestIncomeExpense", "annual")[[1]]
    IntExpense <- IntExpense[3]
    IntExpense <- qdapRegex::ex_between(IntExpense, "{\"raw\":",
                                        ",\"fmt\"")[[1]]
    IntExpense <- as.numeric(IntExpense)
    if (IntExpense < 0) {
      IntExpense <- IntExpense*-1
    }
    
  }
  IntExpense <- data.frame(IntExpense)
  T0Data <- bind_cols(T0Data,IntExpense)
  
  # extrahiert die ausstehenden Aktien
  print("extracting Outstanding Shares")
  
  floatshares <- qdapRegex::ex_between(html_stats, "\"floatShares\":{\"raw\":",
                                       ",\"fmt\"")[[1]]
  floatshares <- as.numeric(floatshares)
  floatshares <- data.frame(floatshares)
  T0Data <- bind_cols(T0Data,floatshares)
  
  # extrahiert die Steuern vor Einkommen 
  print("extracting pre income tax")
  
  PreIncTax <- qdapRegex::ex_between(html_fin, "incomeBeforeTax\":{\"raw\":",
                                     ",\"fmt\"")[[1]]
  PreIncTax <- PreIncTax[5]
  PreIncTax <- as.numeric(PreIncTax)
  PreIncTax <- data.frame(PreIncTax)
  T0Data <- bind_cols(T0Data,PreIncTax)
  
  # extrahiert das Cash Vermögen des Unternehmens
  cash <- qdapRegex::ex_between(html_balance, "otherAssets",
                                "total")[[1]]
  cash <- qdapRegex::ex_between(html_balance, "\"cash\":{\"raw\":",
                                ",\"fmt\":")[[1]]
  cash <- cash[1]
  cash <- as.numeric(cash)
  cash <- data.frame(cash)
  T0Data <- bind_cols(T0Data, cash)
  
  # weist dem data frame T0 data, die oben eingegeben TGR zu 
  T0Data<-data.frame(T0Data,TGR)
  
  out <- list(
    yearly_data = yearly_data,
    T0Data = T0Data,
    ProjRev = ProjRev
  )
  return(out)
}