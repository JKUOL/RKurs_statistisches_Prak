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

