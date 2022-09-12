
# lag wird überdeckt von dplyr deswegen fehlermeldung deshlabt erst alle packete
# mit dplyr entladen 

detach("package:tidyverse", unload=TRUE)
# detach("package:dbplyr", unload=TRUE)
detach("package:gt", unload=TRUE)
detach("package:broom", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)


getSymbols(c('NKE'))

ticker <- "NKE"

start_date <- ymd(format(Sys.Date())) - years(10)

prices <- tq_get(ticker,
                 from = start_date,
                 to = format(Sys.Date()),
                 get = "stock.prices")




NKE             <- NKE$NKE.Adjusted
NKERet           <- log(NKE) - log(lag(NKE))
NKERet_xts       <- SPYRet
colnames(NKERet) <- c('NKE')

library(broom)
library(gt)
library(tidyverse)
library(tidyr)
library(dplyr)


NKERet           <- tidy(NKERet)

p <- ggplot(NKERet, aes(x = index, y = value, color = series)) + 
  geom_line() + 
  theme_bw() +
  labs(title = "NKE Returns Returns from the last 10 Years", x = "")

library(forecast)
ggAcf(NKERet_xts, lag.max = 10) + theme_bw()
ggPacf(SPYRet_xts, lag.max = 10) + theme_bw()

SPYRet_1   <- lag(SPYRet_xts)
SPYRet_2   <- lag(SPYRet_1)
SPYRet_3   <- lag(SPYRet_2)
SPYRet_4   <- lag(SPYRet_3)
SPYRet_5   <- lag(SPYRet_4)
AR2           <- lm(SPYRet_xts ~  SPYRet_1 + SPYRet_2 + SPYRet_3 + SPYRet_4 + SPYRet_5)











