# Erstellt eine Funktion zur Berechnung des implizierten Aktienpreises 
# (implizierten Aktienpreise)

Imp_Share_Price_calculation <- function() {
  
  # weisst der Variablen die benötigten  daten aus der List zu
  T0_cal <- WACC_data$T0_cal
  calended_data <- DCF_calc_data$calended_data
  T0Data <- scraped_DCF_data[[1]]$T0Data
  
  # attach ermöglicht direkt Zugriff auf die Variablen
  attach(T0_cal)
  attach(calended_data)
  
  # berechnet den unlevered free cash flows
  PUnlevFCF <- c(rep("NA", 4), FCF[5:9]/(1+RRR)^as.numeric(DiscountYear[5:9]))
  detach(calended_data)
  
  # berechnet den terminal value
  TermValue <- calended_data$FCF[9]*(1+T0Data$TGR)/(RRR-T0Data$TGR)
  
  # berechnung des aktuellen Wertes des terminal values
  PTermValue <- TermValue/(1+RRR)^as.numeric(calended_data$DiscountYear[9])
  detach(T0_cal)
  
  # berechnet den Enterprise Value
  EntpriVal <- sum(as.numeric(PUnlevFCF[5:9]))+PTermValue
  
  # Da Yahoo Finance nicht immer alle Schulden richtig Updated, und im Fall von 
  # NAs der Equity Value ebenfalls NA ausgegeben wird, ist hier eine if Schleife
  # welche bei Schulden welche geringer als 0 sind die Schulden als 0 annimmt.
  # dies führt zu einer besseren Bewertung des Unternehmens, lässt jedoch
  # wenigstens eine zu
  if(isTRUE(T0Data$Debt<0)){
    Debt <-T0_cal$Debt
  }else {
    Debt <- 0
  }
  
  # Berechnet den Kapitalwert des Unternehmens
  EquValue <- EntpriVal+T0Data$cash-Debt
  
  # Berechnent den Implied Share Price   
  ISP_data<- round(EquValue/T0Data$floatshares,2)
  out <- list(
    ISP = ISP_data,
    EntpriVal = EntpriVal,
    EquValue = EquValue,
    PTermValue = PTermValue,
    PUnlevFCF = PUnlevFCF[5:9]
  )
  return(out)
}