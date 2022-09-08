Imp_Share_Price_calculation <- function() {
  
  T0_cal <- WACC_data$T0_cal
  calended_data <- DCF_calc_data$calended_data
  T0Data <- scraped_DCF_data[[1]]$T0Data
  
  attach(T0_cal)
  attach(calended_data)
  UnlevFCF <- c(rep("NA", 4), FCF[5:9]/(1+RRR)^as.numeric(DiscountYear[5:9]))
  detach(calended_data)
  

  TermValue <- calended_data$FCF[9]*(1+T0Data$TGR)/(RRR-T0Data$TGR)
  
  
  PTermValue <- TermValue/(1+RRR)^as.numeric(calended_data$DiscountYear[9])
 
  detach(T0_cal)
  
  EntpriVal <- sum(as.numeric(UnlevFCF[5:9]))+PTermValue
  
  if(isTRUE(T0Data$Debt<0)){
    Debt <-T0_cal$Debt
  }else {
    Debt <- 0
  }

  EquValue <- EntpriVal+T0Data$cash-Debt
  
  ISP_data<- round(EquValue/T0Data$floatshares)
  out <- list(
    ISP = ISP_data,
    EntpriVal = EntpriVal,
    EquValue = EquValue,
    PTermValue = PTermValue,
    UnlevFCF = UnlevFCF[5:9]
  )
  return(out)
}

