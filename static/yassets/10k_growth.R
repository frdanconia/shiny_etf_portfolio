library(tidyverse)
library(quantmod)
library(highcharter)
source("generate_10k_growth.R")

emlc_10k <- generate_10k_growth("EMLC", "2015-12-20")
sdem_10k <- generate_10k_growth("SDEM", "2015-12-20")
ogzpy_10k <- generate_10k_growth("OGZPY", "2015-12-20")
drw_10k <- generate_10k_growth("DRW", "2015-12-20")
fm_10k <- generate_10k_growth("FM", "2015-12-20")
pslv_10k <- generate_10k_growth("PSLV", "2015-12-20")
pplt_10k <- generate_10k_growth("PPLT", "2015-12-20")
dgs_10k <- generate_10k_growth("DGS", "2015-12-20")
qqq_10k <- generate_10k_growth("QQQ", "2015-12-20")
lqd_10k <- generate_10k_growth("LQD", "2015-12-20")
xrp_10k <- generate_10k_growth("XRP-USD", "2015-12-20")
usl_10k <- generate_10k_growth("USL", "2015-12-20")
dbc_10k <- generate_10k_growth("DBC", "2015-12-20")
lha_10k <- generate_10k_growth("LHA.DE", "2015-12-20")
rpv_10k <- generate_10k_growth("RPV", "2015-12-20")
ciech_10k <- generate_10k_growth("CHX.F", "2015-12-20")


highchart(type = "stock")  %>%
  hc_add_series(emlc_10k, name = "EMLC") %>%
  hc_add_series(sdem_10k, name = "SDEM") %>%
  hc_add_series(drw_10k, name = "DRW") %>%
  hc_add_series(ogzpy_10k, name = "OGZPY") %>%
  hc_add_series(fm_10k, name = "FM") %>%
  hc_add_series(pslv_10k, name = "PSLV") %>%
  hc_add_series(dgs_10k, name = "DGS") %>%
  hc_add_series(pplt_10k, name = "PPLT") %>%
  hc_add_series(rpv_10k, name = "RPV") %>%
  hc_add_series(lha_10k, name = "LHA") %>%
  hc_add_series(dbc_10k, name = "DBC") %>%
  hc_add_series(usl_10k, name = "USL") %>%
  hc_add_series(ciech_10k, name = "CIECH")


total <- emlc_10k + sdem_10k + drw_10k + ogzpy_10k + fm_10k + pslv_10k + dgs_10k + pplt_10k + rpv_10k + lha_10k + dbc_10k + usl_10k + ciech_10k 
sd(total)[[1]]

highchart(type = "stock")  %>%
  hc_add_series(total, name = "total")
