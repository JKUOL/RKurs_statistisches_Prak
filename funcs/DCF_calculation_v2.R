

# scraped_DCF_data <- DCF_data_scraper(ticker = "aapl", GloGrow = 0.029, timeframe = 30)

DCF_calculation <- function(yearly_data = scraped_DCF_data[[1]]$yearly_data, 
                            T0Data = scraped_DCF_data[[1]]$T0Data,
                            ProjRev = scraped_DCF_data[[1]]$ProjRev$ProjRev) {
  
  
  
  
  # yearly_data <- scraped_DCF_data[[1]]$yearly_data
  # T0Data <- scraped_DCF_data[[1]]$T0Data
  # ProjRev <- scraped_DCF_data[[1]]$ProjRev$ProjRev
  
  # yearly_data <- scraped_DCF_data$yearly_data
  # T0Data <- scraped_DCF_data$T0Data
  # ProjRev <- scraped_DCF_data$ProjRev$ProjRev
  
  ProjRev <- ProjRev$ProjRev
  
  
  
  last_statement <- as.numeric(yearly_data$Year[4])
  Year <- c(last_statement, last_statement+1, last_statement+2, last_statement+3,
            last_statement+4, last_statement+5)
  Projection <- data.frame(Year)
  
  # calculates the Revenue growth rate and takes the mean for further 
  # calculations
  
  RevGrowthRate <- c((yearly_data$TotalRevenue[2]/yearly_data$TotalRevenue[1])-1,
                     (yearly_data$TotalRevenue[3]/yearly_data$TotalRevenue[2])-1,
                     (yearly_data$TotalRevenue[4]/yearly_data$TotalRevenue[3])-1,
                     (ProjRev[1]/yearly_data$TotalRevenue[4])-1,
                     (ProjRev[2]/ProjRev[1])-1)
  
  ## needs commentary
  
  meanRevGrowthRate <- mean(RevGrowthRate)
  calc_df <- data.frame(meanRevGrowthRate)
  
  EBITofRev <- c(yearly_data$EBIT[1:4]/yearly_data$TotalRevenue[1:4])
  meanEBITofRev <- mean(EBITofRev)
  meanEBITofRev <- data.frame(meanEBITofRev)
  calc_df <- bind_cols(calc_df,meanEBITofRev)
  
  TaxesofEBIT <- c(yearly_data$IncTaxEx[1:4]/yearly_data$EBIT[1:4])
  meanTaxesofEBIT <- mean(TaxesofEBIT)
  meanTaxesofEBIT <- data.frame(meanTaxesofEBIT)
  calc_df <- bind_cols(calc_df,meanTaxesofEBIT)
  
  DandAofRev <- c(yearly_data$DandA[1:4]/yearly_data$TotalRevenue[1:4])
  meanDandAofRev <- mean(DandAofRev)
  meanDandAofRev <- data.frame(meanDandAofRev)
  calc_df <- bind_cols(calc_df,meanDandAofRev)
  
  CAPEXofRev <- c(yearly_data$CAPEX[1:4]/yearly_data$TotalRevenue[1:4])
  meanCAPEXofRev <- mean(CAPEXofRev)
  meanCAPEXofRev <- data.frame(meanCAPEXofRev)
  calc_df <- bind_cols(calc_df,meanCAPEXofRev)
  
  cNWCofRev <- c(yearly_data$cNWC[1:4]/yearly_data$TotalRevenue[1:4])
  meancNWCofRev <- mean(cNWCofRev)
  meancNWCofRev <- data.frame(meancNWCofRev)
  calc_df <- bind_cols(calc_df,meancNWCofRev)
  
  # takes the Revenue growth rate and projects the future Revenue
  
  rev3 <-  (ProjRev[2]*calc_df$meanRevGrowthRate+ProjRev[2])*calc_df$meanRevGrowthRate+(ProjRev[2]*
                                calc_df$meanRevGrowthRate+ProjRev[2])
  rev4 <- rev3*calc_df$meanRevGrowthRate+rev3
  rev5 <- rev4*calc_df$meanRevGrowthRate+rev4
  
  Rev_Proj <- c(ProjRev,
              (ProjRev[2]*calc_df$meanRevGrowthRate+ProjRev[2]),
              rev3, rev4, rev5)
  rm(rev3,rev4,rev5)
  
  Rev_Proj <- data.frame(Rev_Proj)
  Projection <- bind_cols(Projection,Rev_Proj)
  
  EBIT_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meanEBITofRev)
  EBIT_Proj <- data.frame(EBIT_Proj)
  Projection <- bind_cols(Projection, EBIT_Proj)
  
  Taxes_Proj <- c(Projection$EBIT_Proj[1:6]*calc_df$meanTaxesofEBIT)
  Taxes_Proj <- data.frame(Taxes_Proj)
  Projection <- bind_cols(Projection, Taxes_Proj)
  
  DandA_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meanDandAofRev)
  DandA_Proj <- data.frame(DandA_Proj)
  Projection <- bind_cols(Projection, DandA_Proj)
  
  CAPEX_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meanCAPEXofRev)
  CAPEX_Proj <- data.frame(CAPEX_Proj)
  Projection <- bind_cols(Projection, CAPEX_Proj)
  
  cNWC_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meancNWCofRev)
  cNWC_Proj <- data.frame(cNWC_Proj)
  Projection <- bind_cols(Projection, cNWC_Proj)
  
  #Calenderization of Data.frames
  
  Projection <- Projection %>% 
    rename(
      TotalRevenue = Rev_Proj,
        EBIT = EBIT_Proj,
      IncTaxEx = Taxes_Proj,
      DandA = DandA_Proj,
      CAPEX = CAPEX_Proj,
      cNWC = cNWC_Proj
    )
    
  yearly_and_proj <- bind_rows(yearly_data[2:7], Projection[2:7])
  Year <- c(as.numeric(yearly_data[,1]),Projection[,1])
  Year <- data.frame(Year)
  past_and_proj <- bind_cols(Year, yearly_and_proj)
  
  calended_data <- calenderization(past_and_proj) 
  calended_data <- calended_data$list
  calended_data <- slice(calended_data,1:(nrow(calended_data)-1))
  
  # calculates EBIAT (earnings before interes after taxes)
  # EBIT - Taxes
  
  EBIAT <- c(calended_data$EBIT[1:nrow(calended_data)]-
               calended_data$IncTaxEx[1:nrow(calended_data)])
  EBIAT <- data.frame(EBIAT)
  calended_data <- bind_cols(calended_data, EBIAT)
  
  
  FCF <- c(calended_data$EBIAT[1:9]+calended_data$DandA[1:9]-
             calended_data$CAPEX[1:9]-calended_data$cNWC[1:9])
  FCF <- data.frame(FCF)
  calended_data <- bind_cols(calended_data, FCF)
  
  
  # calculates left over days of the year for mid year convention
  # todays date from Sys.Date
  
  date <- format(Sys.Date())
  
  # defines the last day of the year
  
  end_year <- ceiling_date(Sys.Date() %m-% months(1), 'year') %m-% days(1)

  # calculates the days between currend date and end of the year
  
  days <- difftime(end_year, date)
  days <- as.numeric(t)
  pofyear <- round(days/365,2)
  midyear <- round(pofyear/2,2)
  
  DiscountYear <- c(rep("NA", 5), midyear, pofyear+0.5, pofyear+1.5, pofyear+2.5, pofyear+3.5)
  DiscountYear <- data.frame(DiscountYear)
  past_and_proj <- bind_cols(past_and_proj, DiscountYear)
  
  
  
  # calculates free cash flow to equity for the current and past 3 years 
  # TM stand for T minus year
  
  # FCFE <- c(yearly_data$TotalCashFlow[1]+yearly_data$CAPEX[1],
  #           yearly_data$TotalCashFlow[2]+yearly_data$CAPEX[2],
  #           yearly_data$TotalCashFlow[3]+yearly_data$CAPEX[3],
  #           yearly_data$TotalCashFlow[4]+yearly_data$CAPEX[4])
  
  FCFE <- c(yearly_data$TotalCashFlow[1]+yearly_data$CAPEX[1]+yearly_data$NetBorr[1],
            yearly_data$TotalCashFlow[2]+yearly_data$CAPEX[2]+yearly_data$NetBorr[2],
            yearly_data$TotalCashFlow[3]+yearly_data$CAPEX[3]+yearly_data$NetBorr[3],
            yearly_data$TotalCashFlow[4]+yearly_data$CAPEX[4]+yearly_data$NetBorr[4])
  
  
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
  
  # bis hier rechnungen gut !!!
  
  
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
  return(out)
}

