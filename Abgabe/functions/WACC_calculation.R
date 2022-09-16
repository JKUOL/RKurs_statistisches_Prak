# Erstellt eine Funktion zur Berechnung des WACCs (weighted average cost of
# capital)

WACC_calculation <- function(T0Data = scraped_DCF_data[[1]]$T0Data) {
  
  # weisst der Variablen die benötigten  daten aus der List zu
  calended_data <- DCF_calc_data$calended_data
  
  # berechnet mit der Bondrate, Beta und dem equity risk premium 
  # die cost of equity
  coe <- T0Data$BR+T0Data$Beta*T0Data$erp
  T0_cal <- data.frame(coe)
  
  # berechnung der Schulden
  Debt <- T0Data$CurrDebt+T0Data$LongDebt
  Debt <- data.frame(Debt)
  T0_cal <- bind_cols(T0_cal, Debt)
  
  
  # Berechnung der Schuldenrate
  AvRate <- T0Data$IntExpense/(T0Data$LongDebt+T0Data$CurrDebt)
  AvRate <- data.frame(AvRate)
  T0_cal <- bind_cols(T0_cal,AvRate)
  
  # Berechnung des Effektivsteuersatz
  EffTaxRate <- c(calended_data$IncTaxEx[1:5]/calended_data$EBIT[1:5])
  EffTaxRate <- mean(EffTaxRate)
  EffTaxRate <- data.frame(EffTaxRate)
  T0_cal <- bind_cols(T0_cal,EffTaxRate)
  
  # Berechnung des Kapitals 
  TotCap <- T0Data$MarkCap+T0_cal$Debt
  TotCap <- data.frame(TotCap)  
  T0_cal <- bind_cols(T0_cal,TotCap)
  
  # Berechnung der weight of debt (Gewichtung der Schulden)
  w_d <- T0_cal$Debt/T0_cal$TotCap
  w_d <- data.frame(w_d)
  T0_cal <-bind_cols(T0_cal,w_d)
  
  # Berechnung der weight of equity (Gewichtung des Kapitals)
  w_e <- 1-T0_cal$w_d
  w_e <- data.frame(w_e)
  T0_cal <- bind_cols(T0_cal,w_e)
  
  # Berechnung des WACC
  WACC <- (T0_cal$w_e*T0_cal$coe)+(T0_cal$w_d*T0_cal$AvRate*(1-T0_cal$EffTaxRate))
  WACC <- data.frame(WACC)
  T0_cal <- bind_cols(T0_cal, WACC)
  
  # Da Yahoo Finance nicht immer alle alle Schulden richtig Updated, 
  # (bsp. Snowflake) wird falls sich das WACC durch nicht auftauchende Zahlen 
  # unter einem Wert von 7% befindend, die RRR (required rate of return) auf
  # einen Wert von 0.07 angehoben. Dies passiert auch bei Firmen, welche alle
  # Daten korrekt abgebildet haben, da eine RRR von 7% gewünscht wird.
  if(isTRUE(T0_cal$WACC>0.07)){
    RRR <- T0_cal$WACC
  } else {
    RRR <- 0.07
  }
  RRR <- data.frame(RRR)
  T0_cal <- bind_cols(T0_cal,RRR)
  
  out <- list(
    T0_cal = T0_cal
  )
  return(out)
}