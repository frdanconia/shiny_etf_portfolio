library(readxl)
library(tidyverse)
library(reshape2)

equity_rt <- function(dataset) {
  df <- data.frame()
  for (i in 2:13) {
    df <- rbind(df, as.numeric(dataset[, i][2:7, ][[1]]))
  }
  returns <- c()
  for (i in 6:1) {
    returns <- c(returns, df[, i])
  }
  returns <- as.numeric(returns)
  return(returns[6:length(returns)][!is.na(returns[6:length(returns)])])
}


PLSV <- readxl::read_excel("PSLV.xlsx")
DRW <- readxl::read_excel("DRW.xlsx")
VNQI <- readxl::read_excel("VNQI.xlsx")
SDEM <- readxl::read_excel("SDEM.xlsx")
EFV <- readxl::read_excel("EFV.xlsx")

PLSV <- equity_rt(PLSV)
DRW <- equity_rt(DRW)
VNQI <- equity_rt(VNQI)
SDEM <- equity_rt(SDEM)
EFV <- equity_rt(EFV)

assets <- data.frame(PLSV,SDEM,VNQI,DRW,EFV)
assets$time <- 1:62

meltdf <- melt(assets,id="time")
ggplot(meltdf,aes(x=time,y=value,colour=variable,group=variable)) + geom_line()


assets_cumsum <- data.frame(PLSV=cumsum(PLSV),SDEM=cumsum(SDEM),VNQI=cumsum(VNQI),DRW=cumsum(DRW))
assets_cumsum$time <- 1:62

meltdf2 <- melt(assets_cumsum,id="time")
ggplot(meltdf2,aes(x=time,y=value,colour=variable,group=variable)) + geom_line()
