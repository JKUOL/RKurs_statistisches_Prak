
  WACC_calculation <- function(T0Data = scraped_DCF_data[[1]]$T0Data) {
  
  calended_data <- DCF_calc_data$calended_data
    
  coe <- T0Data$BR+T0Data$Beta*T0Data$erp
  coe <- data.frame(coe)
  T0_cal <- bind_cols(coe)
  # calculation of Debt
  
  Debt <- T0Data$CurrDebt+T0Data$LongDebt
  Debt <- data.frame(Debt)
  T0_cal <- bind_cols(T0_cal, Debt)
  
  # calculation of average rate and effective tax rate for cost of debt (r_d)
  
  AvRate <- T0Data$IntExpense/(T0Data$LongDebt+T0Data$CurrDebt)
  AvRate <- data.frame(AvRate)
  T0_cal <- bind_cols(T0_cal,AvRate)
  
  EffTaxRate <- c(calended_data$IncTaxEx[1:5]/calended_data$EBIT[1:5])
  EffTaxRate <- mean(EffTaxRate)
  EffTaxRate <- data.frame(EffTaxRate)
  T0_cal <- bind_cols(T0_cal,EffTaxRate)
  
  # Calculation of total amount of capital
  
  TotCap <- T0Data$MarkCap+T0_cal$Debt
  TotCap <- data.frame(TotCap)  
  T0_cal <- bind_cols(T0_cal,TotCap)
  
  w_d <- T0_cal$Debt/T0_cal$TotCap
  w_d <- data.frame(w_d)
  T0_cal <-bind_cols(T0_cal,w_d)
  
  w_e <- 1-T0_cal$w_d
  w_e <- data.frame(w_e)
  T0_cal <- bind_cols(T0_cal,w_e)
  
  WACC <- (T0_cal$w_e*T0_cal$coe)+(T0_cal$w_d*T0_cal$AvRate*(1-T0_cal$EffTaxRate))
  WACC <- data.frame(WACC)
  T0_cal <- bind_cols(T0_cal, WACC)
  
  if(isTRUE(T0_cal$WACC>0.05)){
    RRR <- T0_cal$WACC
  } else {
    RRR <- 0.075
  }
  RRR <- data.frame(RRR)
  T0_cal <- bind_cols(T0_cal,RRR)
  
  
  # WACC <- data.frame(WACC)
  # T0_cal <- bind_cols(T0_cal, WACC)
  
  out <- list(
    T0_cal = T0_cal
  )
  return(out)
  }


