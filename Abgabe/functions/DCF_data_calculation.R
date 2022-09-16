# Funktion für die Berechnung des Free Cash Flows
DCF_calculation <- function(yearly_data = scraped_DCF_data[[1]]$yearly_data, 
                            T0Data = scraped_DCF_data[[1]]$T0Data,
                            ProjRev = scraped_DCF_data[[1]]$ProjRev$ProjRev) {
  
  # weist der Variablen das Jahr des letzten Jahres Financial Updates zu
  last_statement <- as.numeric(yearly_data$Year[4])
  
  # erstellt einen df mit den kommenden Jahren für die folgende Projektion
  # der Geschäftszahlen
  Year <- c(last_statement+1, last_statement+2, last_statement+3, 
            last_statement+4, last_statement+5, last_statement+6)
  Projection <- data.frame(Year)
  
  # berechnet die Wachstumsrate des Revenues über die vergangend 4 Jahre und
  # die von den Analysten Prognostizierten kommenden  2 Jahre
  RevGrowthRate <- c((yearly_data$TotalRevenue[2]/yearly_data$TotalRevenue[1])-1,
                     (yearly_data$TotalRevenue[3]/yearly_data$TotalRevenue[2])-1,
                     (yearly_data$TotalRevenue[4]/yearly_data$TotalRevenue[3])-1,
                     (ProjRev[1]/yearly_data$TotalRevenue[4])-1,
                     (ProjRev[2]/ProjRev[1])-1)
  
  # Bildet den Mittelwert der Wachstumsrate und erstellet einen df
  # welcher die berechneten Daten beinhalten wird
  meanRevGrowthRate <- mean(RevGrowthRate)
  calc_df <- data.frame(meanRevGrowthRate)
  
  # Berechnet den Anteil des EBITS an den vergangened Revenues und bildet den
  # Mittelwert daraus
  EBITofRev <- c(yearly_data$EBIT[1:4]/yearly_data$TotalRevenue[1:4])
  meanEBITofRev <- mean(EBITofRev)
  meanEBITofRev <- data.frame(meanEBITofRev)
  calc_df <- bind_cols(calc_df,meanEBITofRev)
  
  # Berechnet den Anteil der Steuern an den vergangened EBIT und bildet den
  # Mittelwert daraus
  TaxesofEBIT <- c(yearly_data$IncTaxEx[1:4]/yearly_data$EBIT[1:4])
  meanTaxesofEBIT <- mean(TaxesofEBIT)
  meanTaxesofEBIT <- data.frame(meanTaxesofEBIT)
  calc_df <- bind_cols(calc_df,meanTaxesofEBIT)
  
  # Berechnet den Anteil der der D&A (Depreciation and Amortization an den
  # vergangened Revenue Zahlen und bildet den Mittelwert daraus
  DandAofRev <- c(yearly_data$DandA[1:4]/yearly_data$TotalRevenue[1:4])
  meanDandAofRev <- mean(DandAofRev)
  meanDandAofRev <- data.frame(meanDandAofRev)
  calc_df <- bind_cols(calc_df,meanDandAofRev)
  
  # Berechnet den Anteil des CapEx ( Capital Expenditure) an den vergangened 
  # Revenues und bildet den Mittelwert daraus
  CAPEXofRev <- c(yearly_data$CAPEX[1:4]/yearly_data$TotalRevenue[1:4])
  meanCAPEXofRev <- mean(CAPEXofRev)
  meanCAPEXofRev <- data.frame(meanCAPEXofRev)
  calc_df <- bind_cols(calc_df,meanCAPEXofRev)
  
  # Berechnet den Anteil des cNWC( change of net working capital) an den 
  # vergangened Revenues und bildet den Mittelwert daraus
  cNWCofRev <- c(yearly_data$cNWC[1:4]/yearly_data$TotalRevenue[1:4])
  meancNWCofRev <- mean(cNWCofRev)
  meancNWCofRev <- data.frame(meancNWCofRev)
  calc_df <- bind_cols(calc_df,meancNWCofRev)
  
  # Projiziert mit der mittleren revenue growth rate den zukünftigen revnue
  # die Variablen rev3:4 werden der Übersicht halber erstellt und im Anschluss
  # entfernt
  rev3 <-  (ProjRev[2]*calc_df$meanRevGrowthRate+ProjRev[2])*
    calc_df$meanRevGrowthRate+(ProjRev[2]*
                                 calc_df$meanRevGrowthRate+ProjRev[2])
  rev4 <- rev3*calc_df$meanRevGrowthRate+rev3
  rev5 <- rev4*calc_df$meanRevGrowthRate+rev4
  Rev_Proj <- c(ProjRev,
                (ProjRev[2]*calc_df$meanRevGrowthRate+ProjRev[2]),
                rev3, rev4, rev5)
  rm(rev3,rev4,rev5)
  Rev_Proj <- data.frame(Rev_Proj)
  Projection <- bind_cols(Projection,Rev_Proj)
  
  # Projection des EBIT anhand des mittleren Anteils am Revenue
  EBIT_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meanEBITofRev)
  EBIT_Proj <- data.frame(EBIT_Proj)
  Projection <- bind_cols(Projection, EBIT_Proj)
  
  # Projection der Steuern anhand des mittleren Anteils am EBIT
  Taxes_Proj <- c(Projection$EBIT_Proj[1:6]*calc_df$meanTaxesofEBIT)
  Taxes_Proj <- data.frame(Taxes_Proj)
  Projection <- bind_cols(Projection, Taxes_Proj)
  
  # Projection des D&A anhand des mittleren Anteils am Revenue
  DandA_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meanDandAofRev)
  DandA_Proj <- data.frame(DandA_Proj)
  Projection <- bind_cols(Projection, DandA_Proj)
  
  # Projection des CapEx anhand des mittleren Anteils am Revenue
  CAPEX_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meanCAPEXofRev)
  CAPEX_Proj <- data.frame(CAPEX_Proj)
  Projection <- bind_cols(Projection, CAPEX_Proj)
  
  # Projection des cNWC anhand des mittleren Anteils am Revenue
  cNWC_Proj <- c(Projection$Rev_Proj[1:6]*calc_df$meancNWCofRev)
  cNWC_Proj <- data.frame(cNWC_Proj)
  Projection <- bind_cols(Projection, cNWC_Proj)
  
  # Umbenennung der col Names des Projection dfs
  Projection <- Projection %>% 
    rename(
      TotalRevenue = Rev_Proj,
      EBIT = EBIT_Proj,
      IncTaxEx = Taxes_Proj,
      DandA = DandA_Proj,
      CAPEX = CAPEX_Proj,
      cNWC = cNWC_Proj
    )
  
  # erstellt einen df mit den Daten der Projection und der Daten der vergangen
  # 4 Jahre 
  yearly_and_proj <- bind_rows(yearly_data[2:7], Projection[2:7])
  Year <- c(as.numeric(yearly_data[,1]),Projection[,1])
  Year <- data.frame(Year)
  past_and_proj <- bind_cols(Year, yearly_and_proj)
  
  # wendet die Funktion calenderization auf past_and_proj an. um die an das 
  # CYE (Calended Year End) anzupassen und bildet einen neuen df, die letzte
  # reihe wird enfernt, da sie keine daten enthält
  calended_data <- calenderization(past_and_proj) 
  calended_data <- calended_data$list
  calended_data <- slice(calended_data,1:(nrow(calended_data)-1))
  
  # berechnet EBIAT (earnings before interest after taxes)
  EffTaxRate <- c(calended_data$IncTaxEx[1:5]/calended_data$EBIT[1:5])
  EBIAT <- calended_data$EBIT[1:nrow(calended_data)]*(1-EffTaxRate)
  
  # EBIAT <- c(calended_data$EBIT[1:nrow(calended_data)]-
  #              calended_data$IncTaxEx[1:nrow(calended_data)])
  EBIAT <- data.frame(EBIAT)
  calended_data <- bind_cols(calended_data, EBIAT)
  
  # berechnet den uFCF (unlevered Free Cash Flow)
  FCF <- c(calended_data$EBIAT[1:9]+calended_data$DandA[1:9]-
             calended_data$CAPEX[1:9]-calended_data$cNWC[1:9])
  FCF <- data.frame(FCF)
  calended_data <- bind_cols(calended_data, FCF)
  
  # berechnet die übrigen Tage des Jahres für die mid year convention 
  # (berücksichtigt den ständigen Cash Flow)
  # erstellt eine Variavle mit dem aktuellen Datum
  
  date <- format(Sys.Date())
  
  # definiert den letzten Tag des Jahres
  
  end_year <- ceiling_date(Sys.Date() %m-% months(1), 'year') %m-% days(1)
  
  # berechnet die differenz zwischen aktuellen Datum und dem Ende des Jahres
  
  days <- difftime(end_year, date)
  days <- as.numeric(days)
  # berechnet den Anteil am Jahr und anschließen daran. die mid year convention
  pofyear <- round(days/365,2)
  midyear <- round(pofyear/2,2)
  
  # die discount period wird berechnet, da durch das Angebrochene Jahr, werden
  # die discount perioden weniger gewertet
  # eventuell sollte dies noch auf die Industry angepasst werden, da mid year
  # convention bei cyclischen Gütern weniger Sinn macht als bei Firmen mit
  # stätigem Cashflow
  DiscountYear <- c(rep("NA", 4), midyear, pofyear+0.5, pofyear+1.5, 
                    pofyear+2.5, pofyear+3.5)
  DiscountYear <- data.frame(DiscountYear)
  calended_data <- bind_cols(calended_data, DiscountYear)
  
  out <- list(
    calended_data = calended_data,
    calc_df = calc_df
  )
  return(out)
}