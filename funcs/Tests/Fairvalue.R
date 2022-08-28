

#Berechnet Free Cash Flow to Equity für akutelles und die drei vergangenen Jahre
# #Hierbei steht TM für T Minus Jahr
# 
# yearly_data <- scraped_data$yearly_data
# T0Data <- scraped_data$T0Data
# ProjRev <- scraped_data$ProjRev

fair_value_of_equity<- function(yearly_data,T0Data,ProjRev) {
 
ticker <- .GlobalEnv$ticker
  
curYear<-format(Sys.Date())
curYear<-year(curYear)

print("FCFE Calculation")

FCFE<-c(yearly_data$TotalCashFlow[1]+yearly_data$CAPEX[1],
        yearly_data$TotalCashFlow[2]+yearly_data$CAPEX[2],
        yearly_data$TotalCashFlow[3]+yearly_data$CAPEX[3],
        yearly_data$TotalCashFlow[4]+yearly_data$CAPEX[4])

FCFE<-data.frame(FCFE)
yearly_data <-bind_cols(yearly_data,FCFE)
rm(FCFE)

#Berechnet FCFE pro NetIncome, da eine konservative Berechnung für den zukünftigen
#FCFE durchgeführt werden soll, wird der niedrigeste Wert genutzt


FCFEpNetInc <- c(yearly_data$FCFE[1]/yearly_data$NetIncome[1],
               yearly_data$FCFE[2]/yearly_data$NetIncome[2],
               yearly_data$FCFE[3]/yearly_data$NetIncome[3],
               yearly_data$FCFE[4]/yearly_data$NetIncome[4])
FCFEpNetIncmin<-min(FCFEpNetInc)
calc_data<-data.frame(FCFEpNetIncmin)
rm(FCFEpNetInc,FCFEpNetIncmin)

#Erstellt den Datensatz Projection, für berechnete Daten des aktuellen und der 
#kommenden drei Jahre

Year <- c(curYear,curYear+1,curYear+2,curYear+3)
Projection <-data.frame(Year)

RevGrowthRate<-c((yearly_data$TotalRevenue[2]/yearly_data$TotalRevenue[1])-1,
                (yearly_data$TotalRevenue[3]/yearly_data$TotalRevenue[2])-1,
                (yearly_data$TotalRevenue[4]/yearly_data$TotalRevenue[3])-1,
                (ProjRev$ProjRev[1]/yearly_data$TotalRevenue[4])-1,
                (ProjRev$ProjRev[2]/ProjRev$ProjRev[1])-1)

meanRevGrowthRate<-mean(RevGrowthRate)
meanRevGrowthRate<-data.frame(meanRevGrowthRate)

calc_data<-bind_cols(calc_data,meanRevGrowthRate)

Rev_Proj<-c(ProjRev$ProjRev,
                (ProjRev$ProjRev[2]*calc_data$meanRevGrowthRate+ProjRev$ProjRev[2]),
                (ProjRev$ProjRev[2]*calc_data$meanRevGrowthRate+ProjRev$ProjRev[2])
                *calc_data$meanRevGrowthRate+(ProjRev$ProjRev[2]*
                calc_data$meanRevGrowthRate+ProjRev$ProjRev[2]))
Rev_Proj<-data.frame(Rev_Proj)
Projection<-bind_cols(Projection,Rev_Proj)
rm(ProjRev,Rev_Proj)

NetIncMarg<-c(yearly_data$NetIncome[1]/yearly_data$TotalRevenue[1],
              yearly_data$NetIncome[2]/yearly_data$TotalRevenue[2],
              yearly_data$NetIncome[3]/yearly_data$TotalRevenue[3],
              yearly_data$NetIncome[4]/yearly_data$TotalRevenue[4])
NetIncMargmin<-min(NetIncMarg)
NetIncMargmin<-data.frame(NetIncMargmin)
calc_data<-bind_cols(calc_data,NetIncMargmin)
rm(NetIncMargmin,meanRevGrowthRate,NetIncMarg,RevGrowthRate)

NetIncome_Proj<-c(Projection$Rev_Proj[1]*calc_data$NetIncMargmin,
                  Projection$Rev_Proj[2]*calc_data$NetIncMargmin,
                  Projection$Rev_Proj[3]*calc_data$NetIncMargmin,
                  Projection$Rev_Proj[4]*calc_data$NetIncMargmin)
NetIncome_Proj<-data.frame(NetIncome_Proj)
Projection<-bind_cols(Projection,NetIncome_Proj)
rm(NetIncome_Proj)

FCFE_Proj<-c(Projection$NetIncome_Proj[1]*calc_data$FCFEpNetIncmin,
                  Projection$NetIncome_Proj[2]*calc_data$FCFEpNetIncmin,
                  Projection$NetIncome_Proj[3]*calc_data$FCFEpNetIncmin,
                  Projection$NetIncome_Proj[4]*calc_data$FCFEpNetIncmin)
FCFE_Proj<-data.frame(FCFE_Proj)
Projection<-bind_cols(Projection,FCFE_Proj)
rm(FCFE_Proj)

#Berechnung des WACC als Required Rate of Return und den benötigten Daten dafür


#Berechnung der Daten von Cost of Debt r_d



#TEST



AvRate<-T0Data$IntExpense/(T0Data$LongDebt+T0Data$CurrDebt)
AvRate<-data.frame(AvRate)
calc_data<-bind_cols(calc_data,AvRate)

EffTaxRate<-T0Data$IncTaxEx/T0Data$PreIncTax
EffTaxRate<-data.frame(EffTaxRate)
calc_data<-bind_cols(calc_data,EffTaxRate)

#Berechnung Cost of Debt r_d

r_d<-calc_data$AvRate*(1-calc_data$EffTaxRate)
r_d<-data.frame(r_d)
calc_data<-bind_cols(calc_data,r_d)

#Berechnung Capital Asseet Pricing Model R_a

R_a<-T0Data$BR+T0Data$Beta*(T0Data$AvRet-T0Data$BR)
R_a<-data.frame(R_a)
calc_data<-bind_cols(calc_data,R_a)

#Berechnung Total Amount of Capital

TotCap<-T0Data$MarkCap+T0Data$TotDebt
TotCap<-data.frame(TotCap)  
calc_data<-bind_cols(calc_data,TotCap)
#Weighted Debt w_d und Weighted Equity w_e

w_d<-T0Data$TotDebt/calc_data$TotCap
w_d<-data.frame(w_d)
calc_data<-bind_cols(calc_data,w_d)

w_e<-1-calc_data$w_d
w_e<-data.frame(w_e)
calc_data<-bind_cols(calc_data,w_e)

#WACC

WACC<-calc_data$w_d*calc_data$r_d+calc_data$w_e*calc_data$R_a
WACC<-data.frame(WACC)
calc_data<-bind_cols(calc_data,WACC)

TermValue<-(Projection$FCFE_Proj[4]*(1+T0Data$GloGrow))/(calc_data$WACC-T0Data$GloGrow)
TermValue<-data.frame(TermValue)
calc_data<-bind_cols(calc_data,TermValue)

DiscFac<-c((1+calc_data$WACC)^1,(1+calc_data$WACC)^2,(1+calc_data$WACC)^2,(1+calc_data$WACC)^2)
DiscFac<-data.frame(DiscFac)
Projection<-bind_cols(Projection,DiscFac)


PreVoFCF<-c(Projection$FCFE_Proj[1]/Projection$DiscFac[1],
            Projection$FCFE_Proj[2]/Projection$DiscFac[2],
            Projection$FCFE_Proj[3]/Projection$DiscFac[3],
            Projection$FCFE_Proj[4]/Projection$DiscFac[4],
            calc_data$TermValue/Projection$DiscFac[4])

TodayVoFCF<-sum(PreVoFCF)          

FVoEq<-round(TodayVoFCF/T0Data$floatshares)


}    