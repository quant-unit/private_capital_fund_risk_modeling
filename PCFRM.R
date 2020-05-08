#### Private Capital Fund Risk Modeling ###
###################
## 0.) Prologue (load packages) -----
require(zoo)
require(MASS)
require(grDevices)
require(data.table)
require(dplyr)
require(DataCombine)
require(lubridate)
require(robustbase)
require(quantreg)
require(moments)
require(sgt)
require(ggplot2)
require(LambertW)
require(e1071)
require(lmtest)
require(mixtools) # does not work with R Version 3.2.1
require(copula)
require(VineCopula)
library(plot3D)
###################
## 1.) Load Public Data
###################
##     Public Equity  -----
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors")

MSCI.World <- read.table(file= "Public World.csv", header = TRUE, sep = ";", dec = ".")
MSCI.World$Date <- as.Date(MSCI.World$Date, format= "%m.%d.%y")
MSCI.World$Index <- as.numeric(MSCI.World$Index)
MSCI.World <- change(data= MSCI.World, Var= "Index", NewVar = "PerCha", type="percent")
MSCI.World$TMFVola <- NA          # Twelve Month Forward Volatility
for(i in 1:(nrow(MSCI.World)-12)){
  MSCI.World$TMFVola[i]  <- sd(MSCI.World[i:(i+11), "PerCha"])
}

NASDAQ <- read.table(file= "NASDAQ monthly.csv", header = TRUE, sep = ",", dec = ".")
colnames(NASDAQ) <- c("Date", "Index")
NASDAQ$Date <- as.Date(NASDAQ$Date)
NASDAQ <- NASDAQ[order(NASDAQ$Date),]
NASDAQ <- change(data= NASDAQ, Var= "Index", NewVar = "PerCha", type="percent")
NASDAQ$TMFVola <- NA          # Twelve Month Forward Volatility
for(i in 1:(nrow(NASDAQ)-12)){
  NASDAQ$TMFVola[i]  <- sd(NASDAQ[i:(i+11), "PerCha"])
}

EuroStoxx <- read.table(file= "EuroStoxx 50 Index.csv", header = TRUE, sep = ",", dec = ".")
EuroStoxx$Date <- as.Date(EuroStoxx$Date)
# determine last of month date
EuroStoxx$YeMo <- as.yearmon(EuroStoxx$Date)
EuroStoxx <- EuroStoxx[duplicated(EuroStoxx$YeMo)==F, c("Date","Close")]
colnames(EuroStoxx) <- c("Date", "Index")
EuroStoxx <- EuroStoxx[order(EuroStoxx$Date),]
EuroStoxx <- change(data= EuroStoxx, Var= "Index", NewVar = "PerCha", type="percent")
EuroStoxx$TMFVola <- NA          # Twelve Month Forward Volatility
for(i in 1:(nrow(EuroStoxx)-12)){
  EuroStoxx$TMFVola[i]  <- sd(EuroStoxx[i:(i+11), "PerCha"])
}

SP500 <- read.table(file= "SP500.csv", header = TRUE, sep = ",", dec = ".")
SP500$Date <- as.Date(SP500$Date)
# determine last of month date
SP500$YeMo <- as.yearmon(SP500$Date)
SP500 <- SP500[duplicated(SP500$YeMo)==F, c("Date","Close")]
colnames(SP500) <- c("Date", "Index")
SP500 <- SP500[order(SP500$Date),]
SP500 <- change(data= SP500, Var= "Index", NewVar = "PerCha", type="percent")
SP500$TMFVola <- NA          # Twelve Month Forward Volatility
for(i in 1:(nrow(SP500)-12)){
  SP500$TMFVola[i]  <- sd(SP500[i:(i+11), "PerCha"])
}
##     Yields  --------
## Goverment Bonds
USEuro <- read.table(file= "USEuro.csv", sep = ",", header = T)
USEuro$Date3 <- as.Date(USEuro$DateUK, format= "%m/%d/%Y")
USEuro <- USEuro[!is.na(USEuro$Date3),]

## Plot MSCI World & USEuro
par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
plot(x= MSCI.World$Date, MSCI.World$Index, type = "o", pch=".")
plot(x= MSCI.World$Date, MSCI.World$PerCha, type = "o", pch=".")
plot(x= MSCI.World$Date, MSCI.World$TMFVola, type = "o", pch=".")
plot(x=USEuro$Date3, y=(USEuro$X10Y.USD - USEuro$X1Y.US), type = "o", pch=".")
par(mfrow = c(1,1))

plot(x= MSCI.World$Date, MSCI.World$TMFVola, type = "o", pch=".", col="red", ylim= c(-1,10))
lines(x=USEuro$Date3, y=(USEuro$X10Y.USD - USEuro$X1Y.US), col="blue")

## Merrill Lynch Corporate Bond Yields/Spreads
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors/Bond Indices")

US.AAA <- read.table(file= "US AAA rated Bond Index (yield).csv", sep = ",", header = T)
US.AAA$DATE <- as.Date(US.AAA$DATE)
colnames(US.AAA) <- c("Date", "US.AAA")
US.corp <- read.table(file= "US Corporate Bond Index Yield.csv", sep = ",", header = T)
US.corp$DATE <- as.Date(US.corp$DATE)
colnames(US.corp) <- c("Date", "US.corp")
US.hiyie <- read.table(file= "US High Yield Corporate Bond Index (Yield).csv", sep = ",", header = T)
US.hiyie$DATE <- as.Date(US.hiyie$DATE)
colnames(US.hiyie) <- c("Date", "US.hiyie")
US.hiyie.OAS <- read.table(file= "US High Yield Corporate Bond Index OAS.csv", sep = ",", header = T)
US.hiyie.OAS$DATE <- as.Date(US.hiyie.OAS$DATE)
colnames(US.hiyie.OAS) <- c("Date", "US.hiyie.OAS")

ML.yields <- merge(x= US.AAA, y= US.corp, by= "Date")
ML.yields <- merge(x= ML.yields, y= US.hiyie, by= "Date")
ML.yields <- merge(x= ML.yields, y= US.hiyie.OAS, by= "Date")
# ML.yields$test <- ML.yields$US.AAA + ML.yields$US.hiyie.OAS
rm(US.hiyie.OAS, US.hiyie, US.corp, US.AAA)

plot(x= ML.yields$Date, y= ML.yields$US.AAA, type="s", col="green", ylim=c(0,23), main="US Yields")
lines(x= ML.yields$Date, y= ML.yields$US.corp, col= "blue")
lines(x= ML.yields$Date, y= ML.yields$US.hiyie, col= "red")
points(x= ML.yields$Date, y= (ML.yields$US.AAA + ML.yields$US.hiyie.OAS), col= "orange", pch= ".")
lines(x= USEuro$Date3, y= USEuro$X10Y.USD, col= "black")
lines(x= USEuro$Date3, y= USEuro$X1Y.USD, col= "grey")
##     Bond Indices  --------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors/Bond Indices")

## Corporate
CB.US.AAA <- read.table(file= "US AAA Corporate Bond Total Return Index.csv", header = TRUE, sep = ",", dec = ".")
CB.US.AAA$DATE <- as.Date(CB.US.AAA$DATE)
colnames(CB.US.AAA) <- c("Date", "US.AAA")
CB.US.BBB <- read.table(file= "US Corporate BBB Total Return Index.csv", header = TRUE, sep = ",", dec = ".")
CB.US.BBB$DATE <- as.Date(CB.US.BBB$DATE)
colnames(CB.US.BBB) <- c("Date", "US.BBB")

ML.TRI <- merge(x= CB.US.AAA, y= CB.US.BBB, by="Date")
rm(CB.US.AAA, CB.US.BBB)
# ML.TRI <- change(data= ML.TRI, Var= "US.AAA", NewVar = "PerCha.US.AAA", type="percent")
# ML.TRI <- change(data= ML.TRI, Var= "US.BBB", NewVar = "PerCha.US.BBB", type="percent")

plot(x= ML.TRI$Date, y= ML.TRI$US.BBB, type="s", main="TRI (US corporate bonds: AAA & BBB) ")
lines(x= ML.TRI$Date, y= ML.TRI$US.AAA, col= "grey")

## Vanguard Bond Index Fd Total Bond Mkt Index Fd
VTM <- read.table(file= "Vanguard Bond Index Fd Total Bond Mkt Index Fd.csv", header = TRUE, sep = ",", dec = ".")
VTM$Date <- as.Date(VTM$Date)
VTM <- VTM[order(VTM$Date),]
VTM <- VTM[,c("Date", "Adjusted.Close")]
colnames(VTM) <- c("Date", "Index")

## Weaddell & Reed Advisors Global Bond Fd Cl A
WRA <- read.table(file= "Weaddell & Reed Advisors Global Bond Fd Cl A.csv", header = TRUE, sep = ",", dec = ".")
WRA$Date <- as.Date(WRA$Date)
WRA <- WRA[order(WRA$Date),]
WRA <- WRA[,c("Date", "Adjusted.Close")]
colnames(WRA) <- c("Date", "Index")
##     Real Estate  ---------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors")
RE.US.com <- read.table(file= "Commercial real estate price index.csv", sep = ",", header = T)
RE.US.com$Date <- as.Date(RE.US.com$Date)
RE.US.com <- RE.US.com[order(RE.US.com$Date),]
RE.US.com$Value <- as.numeric(RE.US.com$Value)
colnames(RE.US.com) <- c("Date", "Index")
RE.US.com <- change(data= RE.US.com, Var= "Index", NewVar = "PerCha", type="percent")

RE.EU.res <- read.table(file= "EU Housing Price Index.csv", sep = ";", header = T)
RE.EU.res <- RE.EU.res[,c("TIME", "Value")]
RE.EU.res$Date <- seq.Date(from= as.Date("2005-03-31"), to= as.Date("2015-10-30"), by="3 months")

plot(x= RE.US.com$Date,y= RE.US.com$Index, type="s")
##     Commodities  --------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors")
Comm.BoJ <- read.table(file= "Bank of Japan Overseas Commodity Index.csv", sep = ",", header = T)
Comm.BoJ$Date <- as.Date(Comm.BoJ$Date)
Comm.BoJ <- Comm.BoJ[order(Comm.BoJ$Date),]
Comm.BoJ$Value <- as.numeric(Comm.BoJ$Value)
colnames(Comm.BoJ) <- c("Date", "Index")
Comm.BoJ <- change(data= Comm.BoJ, Var= "Index", NewVar = "PerCha", type="percent")
##     Cleveland Financial Stress Index  ---------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors")
CFSI <- read.table(file= "FRED-CFSI.csv", sep = ",", header = T)
CFSI$DATE <- as.Date(CFSI$DATE)
CFSI <- CFSI[order(CFSI$DATE),]
CFSI$VALUE <- as.numeric(CFSI$VALUE)
colnames(CFSI) <- c("Date", "Index")
CFSI <- change(data= CFSI, Var= "Index", NewVar = "PerCha", type="percent")

CFSIC <- read.table(file= "FRBC-CFSI_COMP.csv", sep = ",", header = T)
CFSIC$DATE <- as.Date(CFSIC$DATE)
CFSIC <- CFSIC[order(CFSIC$DATE),]
CFSIC <- merge(x= CFSIC, y= data.frame(DATE=seq.Date(from= as.Date("1991-09-25"), to= as.Date("2016-05-01"),by=1)), by="DATE", all.y = T)

CFSIC <- na.locf(CFSIC, na.rm = TRUE, fromLast=T, maxgap = 7)
CFSIC$DATE <- as.Date(CFSIC$DATE)

for(i in 2:ncol(CFSIC)){
  CFSIC[,i] <- as.numeric(CFSIC[,i])
}
##     Equity Liquidity (Pastor/Stambaugh 2003)  -------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors")

LIQ <- read.table(file= "liq_data_1962_2015.csv", header = T, sep=";")
LIQ$Date <- as.Date(as.yearmon(as.character(LIQ$Date), "%Y%m"))
for(i in 2:4){LIQ[,i] <- as.numeric(LIQ[,i])}
LIQ$Liq.Trade <- as.numeric(ifelse(LIQ$Liq.Trade == -99, NA, LIQ$Liq.Trade))
LIQ1 <- LIQ
LIQ1$Date <- LIQ1$Date - 1 

plot(x= LIQ1[month(LIQ1$Date)%in% c(3,6,9,12) & rownames(LIQ1) %in% c(330:651), "Date"], 
     y= LIQ1[month(LIQ1$Date)%in% c(3,6,9,12) & rownames(LIQ1) %in% c(330:651), "Liq.Inno"], type="s", lwd=2, main="Liquidity Innovation Change")
lines(x= LIQ1$Date, y= LIQ1$Liq.Inno, col= "orange")
abline(h=0,col="red")

plot(x= LIQ1[month(LIQ1$Date)%in% c(3,6,9,12) & rownames(LIQ1) %in% c(330:651), "Date"], 
     y= LIQ1[month(LIQ1$Date)%in% c(3,6,9,12) & rownames(LIQ1) %in% c(330:651), "Liq.Trade"], type="s", lwd=2, main="Returns for Traded Liq Spread PF")
lines(x= LIQ1$Date, y= LIQ1$Liq.Trade, col= "blue")
abline(h=0,col="red")
##     Fama French  --------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors/Fama French")

FF.glo <- read.csv(file= "Global_5_Factors.csv")
FF.na <- read.csv(file= "North_America_5_Factors.csv")
FF.eu <- read.csv(file= "Europe_5_Factors.csv")

# compute yearly returns
## GLOBAL
FF.glo <- FF.glo[1:309,]
FF.glo$Date <- as.Date(as.yearmon(as.character(FF.glo$X), "%Y%m"))-1
for(i in 2:7){FF.glo[,i] <- as.numeric(as.character(FF.glo[,i]))}

for(k in 2:7){
  for(i in 1: (nrow(FF.glo)-11)){
    FF.glo[i,paste(colnames(FF.glo)[k],".GO.y",sep="")] <- prod(1+(FF.glo[i:(i+11),colnames(FF.glo)[k]]/100))-1
  }
}
FFG <- FF.glo[,8:14]

## EUROPE
FF.eu <- FF.eu[1:309,]
FF.eu$Date <- as.Date(as.yearmon(as.character(FF.eu$X), "%Y%m"))-1
for(i in 2:7){FF.eu[,i] <- as.numeric(as.character(FF.eu[,i]))}

for(k in 2:7){
  for(i in 1: (nrow(FF.eu)-11)){
    FF.eu[i,paste(colnames(FF.eu)[k],".EU.y",sep="")] <- prod(1+(FF.eu[i:(i+11),colnames(FF.eu)[k]]/100))-1
  }
}
FFE <- FF.eu[,8:14]

## NORTH AMERICA
FF.na <- FF.na[1:309,]
FF.na$Date <- as.Date(as.yearmon(as.character(FF.na$X), "%Y%m"))-1
for(i in 2:7){FF.na[,i] <- as.numeric(as.character(FF.na[,i]))}

for(k in 2:7){
  for(i in 1: (nrow(FF.na)-11)){
    FF.na[i,paste(colnames(FF.na)[k],".NA.y",sep="")] <- prod(1+(FF.na[i:(i+11),colnames(FF.na)[k]]/100))-1
  }
}
FFN <- FF.na[,8:14]
##     Expected Infaltion -------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk/Indices Risk Factors")
EI <- read.csv(file= "ExInflation.csv", sep=";")
EI$Date <- as.Date(EI$Date, format="%d.%m.%y")
EI$Date <- EI$Date - 1
##     Current Public Function Frame  ------------

## Find current NAV/NCF/CD/CC function
current.pu <- function(date){
  cur.world <- tail(MSCI.World[MSCI.World$Date <= date, "Index"], n=1)
  cur.world.vola <- tail(MSCI.World[MSCI.World$Date <= date, "TMFVola"], n=1)
  cur.world.date <- as.character(tail(MSCI.World[MSCI.World$Date <= date, "Date"], n=1))
  USEU <- tail(USEuro[USEuro$Date3 <= date ,], n=1)
  cur.y1.US <- USEU$X1Y.USD
  cur.y1.EU <- USEU$X1Y.europe
  cur.y10.US <- USEU$X10Y.USD
  cur.y10.EU <-  USEU$X10Y.europe
  cur.y.date <- as.character(USEU$Date3)
  hys.os <- tail(ML.yields[ML.yields$Date <= date ,], n=1)
  cur.hys <- hys.os$US.hiyie.OAS
  cur.hys.date <- as.character(hys.os$Date)
  EuStoxx <- tail(EuroStoxx[EuroStoxx$Date <= date, ], n=1)
  cur.ES <- EuStoxx$Index
  cur.ES.date <- as.character(EuStoxx$Date)
  Comm <- tail(Comm.BoJ[Comm.BoJ$Date <= date, ], n=1)
  cur.comm <- Comm$Index
  cur.comm.date <- as.character(Comm$Date)
  SnP <- tail(SP500[SP500$Date <= date, ], n=1)
  cur.sp <- SnP$Index
  cur.sp.date <- as.character(SnP$Date)
  NAS <- tail(NASDAQ[NASDAQ$Date <= date, ], n=1)
  cur.nas <- NAS$Index
  cur.nas.date <- as.character(NAS$Date)
  BI1 <- tail(VTM[VTM$Date <= date, ], n=1)
  cur.vtm <- BI1$Index
  cur.vtm.date <- as.character(BI1$Date)
  RE.us.com1 <- tail(RE.US.com[RE.US.com$Date <= date, ], n=1)
  cur.ruc <- RE.us.com1$Index
  cur.ruc.date <- as.character(RE.us.com1$Date)
  wra1 <- tail(WRA[WRA$Date <= date, ], n=1)
  cur.wra <- wra1$Index
  cur.wra.date <- as.character(wra1$Date)
  
  ## Output
  out.list <- list(cur.y1.US, cur.y10.US, cur.y1.EU ,cur.y10.EU, cur.y.date, cur.hys, cur.hys.date, cur.world, cur.world.vola, cur.world.date, cur.ES, cur.ES.date, cur.sp, cur.sp.date, cur.nas, cur.nas.date, cur.comm, cur.comm.date, cur.vtm, cur.vtm.date, cur.wra,cur.wra.date, cur.ruc, cur.ruc.date)
  op <- ifelse(out.list %in% c("integer(0)", "character(0)", "numeric(0)") , NA, out.list)
  names(op) <- c("Yield1.US" , "Yield10.US",  "Yield1.EU" , "Yield10.EU", "Y.US.EU.Date", "HYspread", "HYspread.Date", "MSCI", "MSCI.vola", "MSCI.date", "ES50", "ES50.Date", "SP500", "SP500.Date", "NASDAQ", "NAS.Date", "Comm", "Comm.Date", "VTM", "VTM.Date", "WRA", "WRA.Date", "RE.US.com", "RE.US.com.Date")
  return(op)
}
current.pu("1993-03-03")

## CPFF
CPFF <- data.frame(Date= seq(from= as.Date("1990-03-31"), to= as.Date("2015-12-31"), by = "3 months"),
                   Dummy= rep(c(0,1,1,0),26))
CPFF$Date <- ifelse(CPFF$Dummy == 1, as.character(as.Date(CPFF$Date-1)), as.character(CPFF$Date))
CPFF$Date <- as.Date(CPFF$Date)

## Fill CPFF
for(i in 1:nrow(CPFF)){
  date <- CPFF$Date[i]
  data <- current.pu(date)
  for(j in 1:length(names(data))){
    CPFF[i, names(data)[j]] <- data[j]
  }
}
rm(date, data, i, j)

## lagged data.frame (4 quarters)
mf <- data.frame(Date1= seq(from= as.Date("1990-03-31"), to= as.Date("2014-12-31"), by = "3 months"),
                 Dummy1= rep(c(0,1,1,0),25), CPFF[5:(nrow(CPFF)),])
mf$Date1<- ifelse(mf$Dummy1 == 1, as.character(as.Date(mf$Date1-1)), as.character(mf$Date1))
mf$Date1 <- as.Date(mf$Date1)
names(mf)[names(mf) == "Date"] <- "Fwd.Date"

CPFF <- merge(x= CPFF, y= mf, by.x= "Date", by.y= "Date1", suffixes = c("",".laged"), all.x = T)

## Calculate Returns
CPFF$MSCI.Return <-  CPFF$MSCI.laged / CPFF$MSCI 
CPFF$ES50.Return <-CPFF$ES50.laged /  CPFF$ES50
CPFF$SP500.Return <- CPFF$SP500.laged /CPFF$SP500
CPFF$NASDAQ.Return <-  CPFF$NASDAQ.laged / CPFF$NASDAQ
CPFF$Comm.Return <-  CPFF$Comm.laged / CPFF$Comm
CPFF$VTM.Return <-  CPFF$VTM.laged / CPFF$VTM
CPFF$WRA.Return <- CPFF$WRA.laged / CPFF$WRA
CPFF$RE.US.com.Return <- CPFF$RE.US.com.laged / CPFF$RE.US.com

colnames(CPFF)
keep.public <- colnames(CPFF)[c(1,3:9,54:61,11)]
Public <- CPFF[,keep.public]
Public <- merge(x= Public, y= data.frame(Date= CFSI$Date, CFSI= CFSI$Index), by="Date", all.x=T)
Public <- merge(x= Public, y= CFSIC, by.x="Date", by.y="DATE", all.x=T)
Public <- merge(x= Public, y= LIQ1, by.x="Date", all.x=T)

## World Yield
Public$Yield1.World <- (2/3* Public$Yield1.US + 1/3 * Public$Yield1.EU)/100
Public$Yield10.World <- (2/3* Public$Yield10.US + 1/3 * Public$Yield10.EU)/100
Public$Yield.Diff.World <- Public$Yield10.World - Public$Yield1.World

## Excess Returns
Public$ExMSCI <- Public$MSCI.Return - 1 - Public$Yield1.World/100
Public$ExSP500 <- Public$SP500.Return - 1 - Public$Yield1.US/100
Public$ExNAS <- Public$NASDAQ.Return - 1 - Public$Yield1.US/100
Public$ExES50 <- Public$ES50.Return - 1 - Public$Yield1.EU/100
Public$ExRE <- Public$RE.US.com.Return - 1 - Public$Yield1.US/100
Public$ExVTM <- Public$VTM.Return - 1 - Public$Yield1.World/100

## log High Yield Spread
Public$logHYs <- log(Public$HYspread)

## High Yield Spread Return
Public$HYsReturn <- NA
for(i in 1:(nrow(Public)-4)){
  Public$HYsReturn[i] <- Public$HYspread[i+4]/Public$HYspread[i] - 1
}

##
Public$HYspread <- Public$HYspread/100

## lm for HY spreads
spread.lm <- lm(HYspread ~  I(CORPORATE.BOND.SPREAD^2) + I(CORPORATE.BOND.SPREAD^3) + I(CORPORATE.BOND.SPREAD^4) ,data= Public)
summary(spread.lm)
Public$HYspread.lm <- predict(spread.lm, Public)
Public$HYS.lm <- ifelse(is.na(Public$HYspread), Public$HYspread.lm, Public$HYspread)

## Merge Fama French Factor Returns
Public <- merge(x= Public, y= FFE, by="Date",all.x = T)
Public <- merge(x= Public, y= FFN, by="Date",all.x = T)
Public <- merge(x= Public, y= FFG, by="Date",all.x = T)

## Merge Expected Inflation, Real Risk Premium & Inflation Risk Premium
Public <- merge(x= Public, y= EI, by="Date", all.x = T)

rmExcept("Public")
##############################
## 2.) Load Prequin Data
########################
## 2.a) Prequin: Read.csv; create pooled1 & values1 -----

setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk")

## pooled
pooled <- read.csv2(file= "Pooled.csv", header = TRUE, sep = ";", quote = "\"", dec = ",")
colnames(pooled)
pooled$Coded.fund_id <- as.factor(pooled$Coded.fund_id)
pooled$Coded.firm_id <- as.factor(pooled$Coded.firm_id)
pooled$Vintage <- as.factor(pooled$Vintage)
pooled$Fund.Size..mn.usd. <- as.numeric(pooled$Fund.Size..mn.usd.)
pooled$Transaction.Date <- as.Date(pooled$Transaction.Date, format= "%d.%m.%y")
pooled$Contribution <- ifelse(pooled$Transaction.Category == "Call", pooled$Transaction.Amount, 0)
pooled$Distribution <- ifelse(pooled$Transaction.Category == "Distribution", pooled$Transaction.Amount, 0)
# lapply(pooled, class)
colnames(pooled)
pol.col <- c("Coded.fund_id", 
             "Transaction.Date", "Transaction.Category", "Contribution", "Distribution", "Transaction.Amount", 
             "Cumulative.Contribution", "Cumulative.Distribution", "Net.Cash.Flow",
             "Vintage", "Category.Type", "Fund.Status", "Fund.Size..mn.usd.", "Fund.Focus")
pooled1 <- pooled[,pol.col]
rm(pooled, pol.col)
colnames(pooled1)
nrow(pooled1[duplicated(pooled1$Coded.fund_id)==F & pooled1$Fund.Status == "Liquidated", ])

## values
values <- read.csv2(file= "Values.csv", header = TRUE, sep = ";", quote = "\"", dec = ",")
values$Coded.fund_id <- as.factor(values$Coded.fund_id)
values$Coded.firm_id <- as.factor(values$Coded.firm_id)
values$Vintage <- as.factor(values$Vintage)
values$Fund.Size..mn.usd. <- as.factor(values$Fund.Size..mn.usd.)
values$Transaction.Date <- as.Date(values$Transaction.Date, format= "%d.%m.%y")
# lapply(values, class)
values$NAV <- values$Transaction.Amount
values$NAV.Date <- values$Transaction.Date
val.col <- c("Coded.fund_id", "NAV.Date", "NAV","Vintage", "Category.Type",
             "Fund.Status", "Fund.Size..mn.usd.", "Fund.Focus")
values1 <- values[, val.col]
rm(values, val.col)
colnames(values1)
values1.L <- (values1[duplicated(values1$Coded.fund_id)==F, ])

## Determine Fund Start Date
min.date.pooled1 <- aggregate(pooled1$Transaction.Date, by= list(pooled1$Coded.fund_id), FUN= min)
min.date.values1 <- aggregate(values1$NAV.Date, by= list(values1$Coded.fund_id), FUN= min)
start.date <- merge(x= values1.L[,c("Coded.fund_id", "Vintage")], y= min.date.pooled1, by.x= "Coded.fund_id", by.y= "Group.1",all.x=T)
start.date1 <- merge(x= start.date, y= min.date.values1, by.x = "Coded.fund_id", by.y = "Group.1", all.x = T)
colnames(start.date1) <- c("Coded.fund_id","Vintage","pooled.date", "values.date")
rm(min.date.pooled1, min.date.values1, start.date)
start.date1$min.TA.date <- as.Date(ifelse(start.date1$pooled.date < start.date1$values.date, start.date1$pooled.date, start.date1$values.date))
start.date1$weeks.diff <- round(difftime(start.date1[,"pooled.date"], start.date1[,"values.date"], units = "weeks"), digits = 0)

## Determine Fund Death Date
pooled.dt <- as.data.table(pooled1)
setkey(pooled.dt, Coded.fund_id)
values.dt <- as.data.table(values1)
setkey(values.dt, Coded.fund_id)
# first Date per fund, where NAV == 0
# b <- values.dt[, .SD[which(NAV == 0)[1],.(NAV.Date,NAV)], by = Coded.fund_id]
# AND exclude 0s at the fund start
# b <- values.dt[, .SD[which(NAV == 0 & shift(VarVect= values.dt[,NAV], shiftBy= -1, reminder = F) > 0 & shift(VarVect= values.dt[,NAV], shiftBy= -2, reminder = F) > 1)[1],.(NAV.Date,NAV)], by = Coded.fund_id]

b <- data.frame(Coded.fund_id= values1.L$Coded.fund_id, NAV.Date= values1.L$NAV.Date, NAV= NA)
for(i in values1.L$Coded.fund_id){
  j <- as.character(i)
  x <- (values.dt[j, 
                  .SD[which((NAV == 0 &
                               !(DataCombine::shift(VarVect= values.dt[j,NAV], shiftBy= -1, reminder = F) %in% c(0,NA)) &
                               !(DataCombine::shift(VarVect= values.dt[j,NAV], shiftBy= -2, reminder = F) %in% c(0,NA)) &
                               !(DataCombine::shift(VarVect= values.dt[j,NAV], shiftBy= -3, reminder = F) %in% c(0,NA)) &
                               (DataCombine::shift(VarVect= values.dt[j,NAV], shiftBy= +1, reminder = F) %in% c(0,NA)) &
                               (DataCombine::shift(VarVect= values.dt[j,NAV], shiftBy= +2, reminder = F) %in% c(0,NA)) &
                               (DataCombine::shift(VarVect= values.dt[j,NAV], shiftBy= +3, reminder = F) %in% c(0,NA))) 
                  )[1], .(NAV.Date,NAV)]
                  ])
  b[b$Coded.fund_id == j, "NAV"] <- x[,NAV]
  b[b$Coded.fund_id == j, "NAV.Date"] <- as.Date(x[,NAV.Date])
}

## Merge Start & Death Date
c <- merge(x= start.date1, y= b[,c("Coded.fund_id","NAV.Date")], by= "Coded.fund_id", all = T)
colnames(c) <- c("Coded.fund_id","Vintage","first.p.date","first.v.date","min.TA.date","weeks.diff", "first.0NAV.date")
Fund.Info <- merge(x= c, y= values1.L[,c("Coded.fund_id", "Category.Type","Fund.Status", "Fund.Size..mn.usd.", "Fund.Focus")], by= "Coded.fund_id")
rm(b, c, start.date1, values1.L, i, j, x)
## 2.b) Fund Cleaning & AMT merge ------
## Frames with dubious NAV death dates
# t1 are mostly okay
t1 <- Fund.Info[!is.na(Fund.Info$first.0NAV.date) & Fund.Info$Fund.Status != "Liquidated" ,] # closed
out.t1 <- c("958887")
# t2 have to be excluded
t2 <- Fund.Info[is.na(Fund.Info$first.0NAV.date) & Fund.Info$Fund.Status == "Liquidated" ,]
out.t2 <- as.character(t2[abs(t2$weeks.diff) > 52, "Coded.fund_id"])
good.t2 <- as.character(t2[abs(t2$weeks.diff) < 52, "Coded.fund_id"]) # problems with determining death date

## Data Cleaning    
summary(as.numeric(Fund.Info$weeks.diff))
hist(as.numeric(Fund.Info$weeks.diff), breaks=20, freq = T, ylim=c(0,100), main= "Weeks Diff")
sum(as.numeric(Fund.Info$weeks.diff) > -52)
sum(as.numeric(Fund.Info$weeks.diff) == 0)

for(i in good.t2){
  j <- as.character(i)
  x <- (values.dt[j, 
                  .SD[which((NAV == 0 &
                               !(DataCombine::shift(VarVect= values.dt[j,NAV], shiftBy= -1, reminder = F) %in% c(0,NA)))
                  )[1], .(NAV.Date,NAV)]
                  ])
  Fund.Info[Fund.Info$Coded.fund_id == j, "first.0NAV.date"] <- as.Date(x[,NAV.Date])
}

Fund.Info <- subset(Fund.Info, subset = !(Coded.fund_id %in% c(out.t1,out.t2)))

## merge AMTs
AMTs <- read.csv(file = "MergeCatTypesV04.csv", header = T, sep=";")
Fund.Info <- merge(x= Fund.Info, y= AMTs, by.x="Category.Type", by.y="Category", all.x=T)
rm(AMTs)

## min.TA date CD can not be too big
'    
first.cum <- data.frame(Coded.fund_id= Fund.Info$Coded.fund_id, CC.first=NA, CD.first=NA)

for(i in Fund.Info$Coded.fund_id){
fid <- as.character(i)
first.cum[first.cum$Coded.fund_id == fid,  "CC.first"] <- pooled1[pooled1$Coded.fund_id == fid  & pooled1$Transaction.Date <= Fund.Info[Fund.Info$Coded.fund_id == fid, "min.TA.date"], "Cumulative.Contribution"][1]
first.cum[first.cum$Coded.fund_id == fid,  "CD.first"] <- pooled1[pooled1$Coded.fund_id == fid  & pooled1$Transaction.Date <= Fund.Info[Fund.Info$Coded.fund_id == fid, "min.TA.date"], "Cumulative.Distribution"][1]
}
'
rm(out.t2,out.t1,good.t2,i,j,x,t1,t2)

summary(Fund.Info$Vintage)
summary(Fund.Info$Fund.Focus)
summary(Fund.Info$Category.Type)
summary(Fund.Info$AssetMetrixType)

sum(Fund.Info$Fund.Status == "Liquidated")
sum(!is.na(Fund.Info$first.0NAV.date))
## 2.c) Functions to analyize pooled1 & values1 -------

## Find current NAV/NCF/CD/CC function
current <- function(date, ID.fund){
  cur.nav <- tail(values1[values1$Coded.fund_id == c(ID.fund) & values1$NAV.Date <= date, "NAV"], n=1)
  cur.nav.date <- as.character(tail(values1[values1$Coded.fund_id == c(ID.fund) & values1$NAV.Date <= date, "NAV.Date"], n=1))
  cur.ncf <- tail(pooled1[pooled1$Coded.fund_id == c(ID.fund) & pooled1$Transaction.Date <= date, "Net.Cash.Flow"], n=1)
  cur.cd <- tail(pooled1[pooled1$Coded.fund_id == c(ID.fund) & pooled1$Transaction.Date <= date, "Cumulative.Distribution"], n=1)
  cur.cc <- tail(pooled1[pooled1$Coded.fund_id == c(ID.fund) & pooled1$Transaction.Date <= date, "Cumulative.Contribution"], n=1)
  
  ## Output
  out.list <- list(cur.nav, cur.nav.date, cur.ncf, cur.cd, cur.cc)
  op <- ifelse(out.list %in% c("integer(0)", "character(0)") , NA, out.list)
  names(op) <- c("NAV", "NAV.date" ,"NCF", "CD", "CC")
  return(op)
}

## current.frame function
current.frame <- function(ID, pe= c("eoy", "random"), start.year = NULL, end.year = NULL){
  if(is.null(start.year)) start.year <- as.numeric(format(Fund.Info[Fund.Info$Coded.fund_id == ID ,"min.TA.date"], '%Y'))
  if(is.null(end.year)) end.year <- ifelse(is.na(Fund.Info[Fund.Info$Coded.fund_id == ID ,"first.0NAV.date"]), 2016, 3 + as.numeric(format(Fund.Info[Fund.Info$Coded.fund_id == ID ,"first.0NAV.date"], '%Y')))
  if(pe=="eoy") {Current.Frame <-data.frame(Date= (seq.Date(as.Date(paste(start.year,"/1/1",sep = "")), 
                                                            as.Date(paste(end.year,"/1/1",sep = "")), by = "12 months"))-1)}
  if(pe=="random"){
    Current.Frame <- if(runif(1)>0.5){data.frame(Date= (seq.Date(as.Date(paste(start.year,"/1/1",sep = "")), 
                                                                 as.Date(paste(end.year,"/1/1",sep = "")), by = "12 months"))-1)} else {
                                                                   data.frame(Date= (seq.Date(as.Date(paste(start.year-1,"/7/1",sep = "")), 
                                                                                              as.Date(paste(end.year-1,"/7/1",sep = "")), by = "12 months"))-1)
                                                                 }
  }
  ID <- as.character(ID)
  Current.Frame$Fund.ID <- ID
  for(i in 1:nrow(Current.Frame)){
    for(j in c("NAV","NAV.date", "NCF", "CD", "CC")){
      Current.Frame[i, j] <- current(date=Current.Frame[i, "Date"], ID.fund= ID)[j]
    }
    Current.Frame$Age.week <- round(difftime(Current.Frame$Date, Fund.Info[Fund.Info$Coded.fund_id == ID ,"min.TA.date"], units = "weeks"), digits = 0)
    Current.Frame$NAV.date.diff <- round(difftime(Current.Frame$Date, Current.Frame$NAV.date, units= "weeks"), digits=0)
    Current.Frame$delta.CC <- c(Current.Frame$CC[2:nrow(Current.Frame)],NA) - Current.Frame$CC
    Current.Frame$delta.CD <- c(Current.Frame$CD[2:nrow(Current.Frame)],NA) - Current.Frame$CD
    Current.Frame$delta.NCF <- c(Current.Frame$NCF[2:nrow(Current.Frame)],NA) - Current.Frame$NCF
    Current.Frame$delta.NAV <- c(Current.Frame$NAV[2:nrow(Current.Frame)],NA) - Current.Frame$NAV
    Current.Frame$delta.NAV.NCF <- Current.Frame$delta.NAV + Current.Frame$delta.NCF
    Current.Frame$Return.NAV <- round((Current.Frame$NAV + Current.Frame$delta.NAV.NCF) / (Current.Frame$NAV), digits = 4)
    Current.Frame$Return.EVB <- round((Current.Frame$NAV + Current.Frame$delta.NAV + Current.Frame$delta.CD) / (Current.Frame$NAV + Current.Frame$delta.CC), digits = 4)
    Current.Frame$TVPI <- round((Current.Frame$NAV + Current.Frame$CD) / Current.Frame$CC, digits = 4)
    Current.Frame$RVPI <- round(Current.Frame$NAV / Current.Frame$CC, digits = 4) # RVPI
    Current.Frame$CD2CC <- round(Current.Frame$CD / Current.Frame$CC, digits = 4)
    Current.Frame$NAV2Com <- round(Current.Frame$NAV / ifelse(Current.Frame$CC > 1e+07, Current.Frame$CC, 1e+07), digits = 4)
    # Current.Frame$NAV2minus.NCF <- round(Current.Frame$NAV/(-Current.Frame$NCF), digits = 4)
    cum.end <- "2030-12-31"
    end.cc <- as.numeric(current(cum.end, ID)["CC"])
    Current.Frame$Fuel.Level <- round((Current.Frame$NAV - Current.Frame$CC + end.cc) / end.cc, digits = 4)
  }
  return(Current.Frame)
}

## current.big.frame function
bcf <- function(n=10, use=c("all","dead"), period= c("eoy", "random")){
  if(use=="dead") big.frame.ids <- sample(x= (Fund.Info[Fund.Info$weeks.diff > -52 & !(is.na(Fund.Info$first.0NAV.date)), "Coded.fund_id"]), size=n) else {}
  if(use=="all") big.frame.ids <- sample(x= (Fund.Info[Fund.Info$weeks.diff > -52, "Coded.fund_id"]), size=n) else {}
  
  df.list <- list()
  
  for(i in seq_len(length(big.frame.ids))){
    id <- big.frame.ids[i]
    b <- current.frame(id, pe= period)
    df.list[[i]] <- b
  }
  c <- rbindlist(l= df.list)
  c$Fund.ID <- as.factor(c$Fund.ID)
  return(c)
}

#system.time(test <- bcf(2,"all","random"))
#current(as.Date("2012-02-01"), 797)
#current.frame(797)
##############################################
## 3.) Historical Simulation
############################
## 3.a) Prequin Set Descriptive Analysis & Ten Fund Test Portfolio ---------
#summary(Fund.Info$Vintage) ; summary(Fund.Info$Fund.Focus) 
#summary(Fund.Info$AssetMetrixType) ;summary(Fund.Info$Category.Type)

# VinXAMT frame -> summary of Prequin Data Set
VinXAMT <- summarise(group_by(Fund.Info, Vintage), BO= sum(AssetMetrixType=="BO"), VC=sum(AssetMetrixType=="VC"), other=sum(AssetMetrixType %in% c("FOF","SEC","DD", "MEZZ", "NatRes","Infrastructure","RE","raus")),
                     BO.US= sum(AssetMetrixType=="BO" & Fund.Focus == "US"), BO.EU= sum(AssetMetrixType=="BO" & Fund.Focus == "Europe"),
                     VC.US= sum(AssetMetrixType=="VC" & Fund.Focus == "US"), VC.EU= sum(AssetMetrixType=="VC" & Fund.Focus == "Europe"))
VinXAMT$Total <- VinXAMT$BO + VinXAMT$VC + VinXAMT$other

## Overall Portfolio Allocation (see: e.g. Allianz Group Annual Report 2015, p. 223)
pf <- data.frame(AssetClass= c("Goverment Bond", "Corporate Bond", "Public Equity", "Private Capital"),
                 Allocation= c(209.5/490, 240/490, 33/490, 7.5/490))

## Private Equity Portfolio
pepf <- data.frame(Type= c("BO","FOF","NatRes","VC","BO","RE","DD","Infrastructure","VC","MEZZ"),
                   Focus=c("US","ROW","EU","US","ROW", "EU", "US","ROW","EU","US"),
                   Age.year=c(1,2,3,4,5,6,7,8,9,10),
                   Age.week=c(23,23+52,25+52*2,25+52*3,25+52*4,25+52*5,25+52*6,25+52*7,25+52*8,25+52*9),
                   AgeCat=c("young","young","young","adult","adult","adult", "adult","adult","old","old"),
                   CC=c(1.4,10,5.5,14.7,12,8,1,9.5,10.5,10),
                   CD=c(0,2,5,3,13,2,0,3,9,4),
                   NAV=c(1,8,7,14,2,5,1,11,4,5),
                   Com=c(10,20,5,15,12.5,15,10,10,10,10),
                   RVPI=NA,NAV2Com=NA, TVPI=NA)
pepf$NAV2Com <- pepf$NAV / pepf$Com
pepf$RVPI <- pepf$NAV / pepf$CC
pepf$TVPI <- (pepf$NAV + pepf$CD) / pepf$CC
sum(pepf$NAV)
## 3.b) Similar Portfolio (historical) (scaled)  --------
## SIMULATION
times <- 1
NAV.Ret <- data.frame(AD=rep(NA,times),FD=NA)

system.time(
  for(j in 1:times){
    
    ## Analysis & Future Date
    ay <- sample(seq(from=1995, to=2014, by=1),size=1)
    rq <- sample(c("Q1","Q2","Q3","Q4"), size=1)
    if(rq == "Q4")    ad <- as.Date(paste(as.numeric(ay),"12",31,sep = "-"))
    if(rq == "Q3")    ad <- as.Date(paste(as.numeric(ay),"09",30,sep = "-"))
    if(rq == "Q2")    ad <- as.Date(paste(as.numeric(ay),"06",30,sep = "-"))
    if(rq == "Q1")    ad <- as.Date(paste(as.numeric(ay),"03",31,sep = "-"))
    
    fd <- as.Date(ad+366)
    
    ## Simulation Frame
    sf <- data.frame(fid=rep(NA,nrow(pepf)))
    
    ## Theorectical Vintages
    for(i in 1:nrow(pepf)){ sf$Vin[i] <- as.numeric(ay - pepf[i,"Age.year"]) }
    
    ## Fund IDs
    for(i in 1:nrow(pepf)){
      if(sf$Vin[i] > 1990){
        if(pepf[i,"Age.year"] < 2){
          if(pepf[i, "Type"] == "BO") sf$fid[i] <- sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50/2* pepf[i,"Age.year"]) & Fund.Info$Vintage == sf$Vin[i] & Fund.Info$AssetMetrixType =="BO" , "Coded.fund_id"]),1)
          if(pepf[i, "Type"] == "VC") sf$fid[i] <- sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50/2* pepf[i,"Age.year"]) & Fund.Info$Vintage == sf$Vin[i] & Fund.Info$AssetMetrixType =="VC" , "Coded.fund_id"]),1)
          if(pepf[i, "Type"] %in% c("DD","FOF","Infrastructure","MEZZ","NatRes","raus","RE","SEC")) sf$fid[i] <- sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50/2* pepf[i,"Age.year"]) & Fund.Info$Vintage == sf$Vin[i] & Fund.Info$AssetMetrixType %in% c("DD","FOF","Infrastructure","MEZZ","NatRes","raus","RE","SEC") , "Coded.fund_id"]),1)
        }
        if(pepf[i,"Age.year"] >= 2 & pepf[i,"Age.year"] < 10){
          if(pepf[i, "Type"] == "BO") sf$fid[i] <-  sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50* pepf[i,"Age.year"]) & Fund.Info$Vintage %in% c(sf$Vin[i],sf$Vin[i]-1,sf$Vin[i]+1) & Fund.Info$AssetMetrixType =="BO" , "Coded.fund_id"]),1)
          if(pepf[i, "Type"] == "VC") sf$fid[i] <-  sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50* pepf[i,"Age.year"]) & Fund.Info$Vintage %in% c(sf$Vin[i],sf$Vin[i]-1,sf$Vin[i]+1) & Fund.Info$AssetMetrixType =="VC" , "Coded.fund_id"]),1)
          if(pepf[i, "Type"] %in% c("DD","FOF","Infrastructure","MEZZ","NatRes","raus","RE","SEC")) sf$fid[i] <-  sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50* pepf[i,"Age.year"]) & Fund.Info$Vintage %in% c(sf$Vin[i],sf$Vin[i]-1,sf$Vin[i]+1) & Fund.Info$AssetMetrixType %in% c("DD","FOF","Infrastructure","MEZZ","NatRes","raus","RE","SEC") , "Coded.fund_id"]),1)
        } 
        if(pepf[i,"Age.year"] >= 10) {
          sf$fid[i] <- sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50* pepf[i,"Age.year"]) & Fund.Info$Vintage %in% seq(from=ay-10, to=ay-15, by=-1), "Coded.fund_id"]),1)
        }
      } else {  ## for Vin[i] < 1990
        sf$fid[i] <- sample(as.character(Fund.Info[Fund.Info$weeks.diff > (-50* pepf[i,"Age.year"]) & Fund.Info$Vintage %in% seq(from=ay-1984, to=1989, by=1), "Coded.fund_id"]),1)
      }
    }
    
    ## AMTs / weights / NAV / NCF
    for(i in 1:nrow(pepf)){
      ## AMTs
      sf$AMT[i] <- as.character(Fund.Info[Fund.Info$Coded.fund_id == sf$fid[i], "AssetMetrixType"])
      
      cu.ad <- current(ad,sf$fid[i])
      cu.fd <- current(fd,sf$fid[i])
      
      ## weights
      sf$w1[i] <- ifelse(as.numeric(cu.ad["NAV"]) < 1000, 0, 
                         round(pepf$NAV[i]*1e+06 / as.numeric(cu.ad["NAV"]), digits=1) ) # scale to NAV, i.e. NAV weighting
      sf$w2[i] <- round(pepf$Com[i] / 10, digits=1)  # scale to Commitment, i.e. commitment weighting
      
      ## NAV/NCF ad & fd
      sf$NAV.ad[i] <- cu.ad["NAV"]
      sf$NAV.fd[i] <- cu.fd["NAV"]
      sf$NCF.ad[i] <- cu.ad["NCF"]
      sf$NCF.fd[i] <- cu.fd["NCF"]
    }
    
    ## Scale NAV/NCF to Portfolio NAV
    sf$NAV.ad1 <- as.numeric(sf$NAV.ad) * sf$w1
    sf$NAV.fd1 <- as.numeric(sf$NAV.fd) * sf$w1
    sf$NCF.ad1 <- as.numeric(sf$NCF.ad) * sf$w1
    sf$NCF.fd1 <- as.numeric(sf$NCF.fd) * sf$w1
    
    ## Scale NAV/NCF to Portfolio Commitment
    sf$NAV.ad2 <- as.numeric(sf$NAV.ad) * sf$w2
    sf$NAV.fd2 <- as.numeric(sf$NAV.fd) * sf$w2
    sf$NCF.ad2 <- as.numeric(sf$NCF.ad) * sf$w2
    sf$NCF.fd2 <- as.numeric(sf$NCF.fd) * sf$w2
    
    ## NAV Return
    NAV.Ret$AD[j] <- as.character(ad)
    NAV.Ret$FD[j] <- as.character(fd)
    NAV.Ret$Pri.NAV.scaled[j] <- ( sum(sf$NAV.fd1) + sum(sf$NCF.fd1) - sum(sf$NCF.ad1) ) / sum(sf$NAV.ad1)
    NAV.Ret$Pri.Com.scaled[j] <- ( sum(sf$NAV.fd2) + sum(sf$NCF.fd2) - sum(sf$NCF.ad2) ) / sum(sf$NAV.ad2)
    ## Public Return (put out of loop)  
    #  MSCI <- NAV.Ret$MSCI[j] <- as.numeric(current(fd, sf$fid[1])["World"]) / as.numeric(current(ad, sf$fid[1])["World"])
    #  NAV.Ret$PuPri.NAV.scaled[j] <- (sum(sf$NAV.fd1) + sum(sf$NCF.fd1) - sum(sf$NCF.ad1) + (1/xp.private-1)*sum(sf$NAV.ad1)*(MSCI)) / (sum(sf$NAV.ad1) + (1/xp.private-1) * sum(sf$NAV.ad1))
    #  NAV.Ret$PuPri.Com.scaled[j] <- (sum(sf$NAV.fd2) + sum(sf$NCF.fd2) - sum(sf$NCF.ad2) + (1/xp.private-1)*sum(sf$NAV.ad2)*(MSCI)) / (sum(sf$NAV.ad2) + (1/xp.private-1) * sum(sf$NAV.ad2))
    
    ## put weights in NAV.Return
    NAV.Ret$w1[j] <- paste(as.character(sf$w1), sep="",collapse = "-")
    NAV.Ret$sum.w1[j] <- sum(abs(log(sf$w1)))
    NAV.Ret$w2[j] <- paste(as.character(sf$w2), sep="",collapse = "-")
    NAV.Ret$sum.w2[j] <- sum(abs(log(sf$w2)))
    
    ## put AMT in NAV.Return
    NAV.Ret$AMT[j] <- paste(as.character(sf$AMT), sep="", collapse = "-")
    
    ## put Fund ID in NAV.Return
    NAV.Ret$FID[j] <- paste(as.character(sf$fid), sep="", collapse = "-")
  }
)

rm(rq,ad,ay,fd,i,j,sf,cu.ad,cu.fd)

## write or read CSV
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk")
# write.csv(NAV.Ret, file="NAV.Ret 6 (verstrichen 16489).csv")
#NAV.Ret <- read.csv("NAV.Ret 5 (verstrichen 15505).csv") ; NAV.Ret <- NAV.Ret[, !(colnames(NAV.Ret) %in% "X")]
NAV.Ret <- read.csv("NAV.Ret 6 (verstrichen 16489).csv") ; NAV.Ret <- NAV.Ret[, !(colnames(NAV.Ret) %in% "X")]

NAV.Ret$AD <- as.Date(NAV.Ret$AD)
NAV.Ret$FD <- as.Date(NAV.Ret$FD)
NAV.Ret <- NAV.Ret[NAV.Ret$FD < "2016-01-01", ]
NAV.Ret <- merge(x= NAV.Ret, y= Public, by.x= "AD", by.y="Date", all.x = T)
NAV.Ret$YCS <- ifelse(NAV.Ret$Yield10.World - NAV.Ret$Yield1.World < 0, 1, 0)
Public$YCS <- ifelse(Public$Yield10.World - Public$Yield1.World < 0, 1, 0)

## write or read CSV
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk")
# write.csv(NAV.Ret, file="NAV.Ret xx (verstrichen xxx).csv")
 NAV.Ret2 <- read.csv("NAV.Ret 4 (verstrichen 1745).csv")
 NAV.Ret2$AD <- as.Date(NAV.Ret2$AD)
 NAV.Ret2 <- merge(x= NAV.Ret2[,1:11], y= Public, by.x= "AD", by.y="Date", all.x = T)
 NAV.Ret2 <- NAV.Ret2[, !(colnames(NAV.Ret2) %in% "X")]
 NAV.Ret <- rbind(NAV.Ret,NAV.Ret2)
 rm(NAV.Ret2)

NAV.Ret$ExcessNAV <- NAV.Ret$Pri.Com.scaled - 1 - NAV.Ret$Yield1.World

summary(NAV.Ret$Pri.Com.scaled) ; length(NAV.Ret$Pri.Com.scaled)

## Benchmark ExReturns for Monte Carlo Simulation 12,500 iterations
NRPCS10000 <- list()
NRPCS10000$NAV.Ret  <- NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "Pri.Com.scaled"]-1-NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "Yield1.World"]/100
NRPCS10000 <- as.numeric(unlist(NRPCS10000))
NRPCS10000 <- NRPCS10000[!(is.na(NRPCS10000))]
summary(NRPCS10000)

## Combine Portfolio Returns
x.bo <- pf[pf$AssetClass == "Goverment Bond", "Allocation"] + pf[pf$AssetClass == "Corporate Bond", "Allocation"]
x.pu <- pf[pf$AssetClass == "Public Equity", "Allocation"]
x.pr <- pf[pf$AssetClass == "Private Capital", "Allocation"]

NAV.Ret$PuPri.NAV.scaled <- x.pr/(x.pu+x.pr)*NAV.Ret$Pri.NAV.scaled + x.pu/(x.pu+x.pr)*NAV.Ret$MSCI.Return
NAV.Ret$PuPri.Com.scaled <- x.pr/(x.pu+x.pr)*NAV.Ret$Pri.Com.scaled + x.pu/(x.pu+x.pr)*NAV.Ret$MSCI.Return
rm(x.bo, x.pu, x.pr)

summary(NAV.Ret[,c("Pri.NAV.scaled","Pri.Com.scaled","MSCI.Return", "PuPri.NAV.scaled", "PuPri.Com.scaled")])

summary(NRPCS10000)
all.moments(NRPCS10000, order.max = 4, central = F, absolute = F, na.rm = T)
mean(NRPCS10000, na.rm = T) ; sd(NRPCS10000,na.rm = T) ; skewness(NRPCS10000,na.rm = T) ; kurtosis(NRPCS10000,na.rm = T)
pbs <- c(0,0.001,0.005,0.01,0.02,0.05,0.1,0.25,0.5,0.9,0.95,0.98,0.99,0.995,0.999)

# quantile(NAV.Ret$PuPri.NAV.scaled, probs = pbs, na.rm=T)
quantile(NAV.Ret$MSCI.Return, probs = pbs, na.rm=T)
quantile(NRPCS10000, probs = pbs, na.rm=T)
VaR <- c(VaR = quantile(NRPCS10000, 0.005, na.rm = T), CondVaR = mean(NRPCS10000[NRPCS10000 < quantile(NRPCS10000, 0.005, na.rm = T)], na.rm = T))
VaR

# Historgram        
par(mfrow=c(2,1), mar=c(4,4,2,2))
hist(NRPCS10000, breaks=50, xlab="Excess NAV Returns (historical simulation)", ylim=c(0,3),main=NULL, prob = T)
rug(NRPCS10000, col="darkgreen")
lines(density(NRPCS10000, na.rm = T),col= "darkgreen", lwd=3)
curve(dnorm(x, mean= mean(NRPCS10000,na.rm = T), sd= sd(NRPCS10000,na.rm = T)), col="darkblue",lwd=3,add=T)
abline(v = VaR["VaR.0.5%"], col="orange", lwd=2, lty=2)
abline(v = VaR["CondVaR"], col=77, lwd=2, lty=2)
legend("topright", legend = c("Simulation Kernel","Normal Fit Density","VaR 0.5% (simulated)","CVaR 0.5% (simulated)"), cex=0.7, box.lty=0, lty=c(1,1,2,2),col=c("darkgreen","darkblue","orange",77), lwd=c(3,3,2,2))
#legend("bottomright",legend = c("VaR 0.5% (simulated)","CVaR 0.5% (simulated)"), cex=0.7, box.lty=0, lty=c(2,2),col=c(), lwd=c(2,2))
 
# Historical Scatterplot of NAV returns (time evolution of historical portfolios)
plot(x=NAV.Ret$AD, y=NAV.Ret$ExcessNAV, pch=".", xlab="Date",ylab="NAV Return", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)

resimax <- aggregate(NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "Pri.Com.scaled"]-1-NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "Yield1.World"]/100 ~ NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "AD"], FUN=max)
resimin <- aggregate(NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "Pri.Com.scaled"]-1-NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "Yield1.World"]/100 ~ NAV.Ret[!is.na(NAV.Ret$Pri.Com.scaled), "AD"], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
rm(resimax, resimin)
## 3.c) SGT & log-normal Estimation for overall distribution  ---------------
# start parameters for SGT-distribution
sta = c(mu= mean(NRPCS10000, na.rm = T),
        sigma= sd(NRPCS10000, na.rm = T),
        lambda=0.25,
        p=2,
        q=2)
sta
x <- NRPCS10000[!(is.na(NRPCS10000))]
result <- sgt.mle(X.f = ~ x, start=sta)
result
summary(result)
result$estimate["p"] * result$estimate["q"]
SGT.sim <- rsgt(length(x), mu = result$estimate["mu"], sigma = result$estimate["sigma"], lambda = result$estimate["lambda"], p = result$estimate["p"],q = result$estimate["q"])
SGT.den <- dsgt(seq(-3,5, by=0.1), mu = result$estimate["mu"], sigma = result$estimate["sigma"], lambda = result$estimate["lambda"], p = result$estimate["p"],q = result$estimate["q"])
mean(x) ; mean(SGT.sim) ; sd(x) ; sd(SGT.sim)

# Visu
par(mfrow=c(2,1), mar=c(2,2,1,1), oma=c(0,0,0,0))
layout(mat= matrix(c(1,1,2,2),2,2,byrow = T), heights = c(3,1), widths = c(1,1))
plot(seq(-3,5,by=0.1), SGT.den,main="", lwd=3, type="l", col="grey", xlim=c(-3,5), xlab="Excess NAV Return")
lines(density(SGT.sim), col="red", lty=3, lwd=2) 
lines(density(x), col="darkgreen", lty=3, lwd=3)
legend("topright", cex=1.25, legend=c( "SGT density", "sim. SGT kernel", "empirical kernel"),box.lty=0, lty = c(1,3,2), col=c("grey","red","darkgreen"), lwd=c(3,2,3))

plot(x= SGT.sim, y=rep(1.5, length(x)), main="", col="red", pch="|", xlab="", ylab="", ylim=c(0.5,2), xlim=c(-3,5),yaxt='n')
points(x= x, y=rep(1, length(x)), col="darkgreen", pch="|", xlab="", ylab="")

ggplot(d <- data.frame(x = c(SGT.sim, x), l=c(rep("SGT",length(x)),rep("ENC",length(x)))), 
       aes(x=x, colour=l)) + geom_density(size=1) +
  geom_rug(data=subset(d,l=="SGT"),aes(x=x)) +
  geom_rug(data=subset(d,l=="ENC"),aes(x=x),sides="t")

## log-normal fit
lnx <- fitdistr(x+1, "lognormal")
ln.sim <- rlnorm(length(x),lnx$estimate[1],lnx$estimate[2]) -1
ln.den <- dlnorm(seq(-3,5,by=0.1),lnx$estimate[1],lnx$estimate[2])

par(mfrow=c(2,1), mar=c(2,4,2,2))
layout(mat= matrix(c(1,1,2,2),2,2,byrow = T), heights = c(3,1), widths = c(1,1))
plot(seq(-3-1,5-1,by=0.1), ln.den, main="Log-normal vs. empirical distribution", lwd=3, type="l", col="grey", xlim=c(-3,5), ylim=c(0,3), xlab="Excess NAV Return")
lines(density(ln.sim), col="red", lty=3, lwd=2) 
lines(density(x), col="darkgreen", lty=3, lwd=3)
legend("topright", legend=c( "log-normal density", "sim. log-normal", "empirical"),box.lty=0, lty = c(1,3,2), col=c("grey","red","darkgreen"), lwd=c(3,2,3))
plot(x= ln.sim, y=rep(1.5, length(x)), main="Rugs", col="red", pch="|", xlab="", ylab="", ylim=c(0.5,2), xlim=c(-3,5),yaxt='n')
points(x= x, y=rep(1, length(x)), col="darkgreen", pch="|", xlab="", ylab="")
## 3.d) Historical NAV.Ret worst case scaling  ---------
NAV.Ret$PuPri.wc.scaled <- ifelse(is.na(NAV.Ret$PuPri.NAV.scaled), NAV.Ret$PuPri.Com.scaled, 
                                  ifelse(NAV.Ret$PuPri.NAV.scaled > NAV.Ret$PuPri.Com.scaled, NAV.Ret$PuPri.Com.scaled, NAV.Ret$PuPri.NAV.scaled))
NAV.Ret$Pri.wc.scaled <- ifelse(is.na(NAV.Ret$Pri.NAV.scaled), NAV.Ret$Pri.Com.scaled,
                                ifelse(NAV.Ret$Pri.NAV.scaled > NAV.Ret$Pri.Com.scaled, NAV.Ret$Pri.Com.scaled, NAV.Ret$Pri.NAV.scaled))
par(mfrow = c(1,1), mar=c(4,4,2,2))
hist(NAV.Ret$PuPri.wc.scaled-1, breaks=50, xlab="PF Returns", main="Histogram of PF Returns (worst case)")
rug(NAV.Ret$PuPri.wc.scaled-1, col="red")
summary(NAV.Ret$PuPri.wc.scaled-1)
quantile(NAV.Ret$PuPri.wc.scaled-1, probs = pbs, na.rm=T)
summary(NAV.Ret[!(is.na(NAV.Ret$PuPri.wc.scaled)), "MSCI.Return"]-1)
quantile(NAV.Ret[!(is.na(NAV.Ret$PuPri.wc.scaled)), "MSCI.Return"]-1, probs = pbs, na.rm=T)

## Average Return per year (private & wc scaling)
Pri.Ret <- "Pri.Com.scaled"
#Pri.Ret <- "Pri.wc.scaled"
AR <- data.frame(Date=NAV.Ret[duplicated(NAV.Ret$AD) == F, "AD"],Mean.Ret=NA,SD.Ret=NA,Med.Ret=NA,Min=NA,Max=NA,Count=NA)
for(i in 1:nrow(AR)){
  AR$Mean.Ret[i] <- mean(NAV.Ret[NAV.Ret$AD == AR$Date[i], Pri.Ret],na.rm=T)
  AR$SD.Ret[i] <- sd(NAV.Ret[NAV.Ret$AD == AR$Date[i], Pri.Ret],na.rm=T)
  AR$Med.Ret[i] <- median(NAV.Ret[NAV.Ret$AD == AR$Date[i], Pri.Ret],na.rm=T)
  AR$Min[i] <- min(NAV.Ret[NAV.Ret$AD == AR$Date[i], Pri.Ret],na.rm=T)  
  AR$Max[i] <- max(NAV.Ret[NAV.Ret$AD == AR$Date[i], Pri.Ret],na.rm=T)
  AR$Count[i] <- nrow(NAV.Ret[NAV.Ret$AD == AR$Date[i],])
}
AR$Date <- as.Date(AR$Date)
AR <- merge(AR, Public[, c("Date","Yield.Diff.World","HYspread")], by="Date", all.x = T)

lm(SD.Ret ~ HYspread, data= AR)

## No Overlapping
NAV.Ret$Month <- as.factor( lubridate::month(NAV.Ret$AD) )
quantile(NAV.Ret[NAV.Ret$Month == 12 , "Pri.Com.scaled"], probs = pbs, na.rm = TRUE)
## 3.e) Stochastic Modelling based on NAV.Ret   -------------- 
# >>> (here: with commitment scaling) <<< 

NAV.Ret[!is.na(NAV.Ret$ExcessNAV), "ENC.trans"] <- Gaussianize(NAV.Ret[!is.na(NAV.Ret$ExcessNAV), "ExcessNAV"], type="hh")
tm <-  Gaussianize(NAV.Ret[!is.na(NAV.Ret$ENC), "ExcessNAV"], type="hh",return.tau.mat=TRUE)$tau.mat
Gaussianize(0.154 + 0.33 * 0.5 - 1.9 * 0.02 + 0.26 * 0.1 + 0.116 * 2.35, tau.mat = tm, inverse = TRUE)
Gaussianize(0.154 + 0.33 *  - 0.5 - 1.9 * 0.16 + 0.26 * - 0.1 - 0.116 * 1.64, tau.mat = tm, inverse = TRUE)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(x = seq(-.25,.3,by=.025), y= Gaussianize(seq(-.25,.3, by=.025), tau.mat = tm, inverse = TRUE), type="b")
abline(h=0, col="red")
plot(x = seq(-.5,.6,by=.05), y= Gaussianize(seq(-.5,.6, by=.05), tau.mat = tm, inverse = TRUE), type="b")
abline(h=0, col="red")
summary(NAV.Ret$ENC.trans)
plot(density(NAV.Ret$ENC.trans, na.rm = T))

## Correlation Check for basic OLS
test <- Public[,c("MSCI.Return", "Yield1.World", "HYspread","Liq.Trade","Yield.Diff.World")]
test$ExMSCI <- test$MSCI.Return - 1 - test$Yield1.World 
cor(test[,c("ExMSCI","HYspread","Liq.Trade","Yield.Diff.World")], use="pairwise.complete.obs")
rm(test)

# Single Factor OLS models
NAV.Ret$EMR <- NAV.Ret$ExMSCI ; NAV.Ret$HYS <- NAV.Ret$HYspread ; NAV.Ret$IML <- NAV.Ret$Liq.Trade
lmEMR <- lm(ExcessNAV ~ EMR, data = NAV.Ret)
lmHYS <- lm(ExcessNAV ~ HYS, data = NAV.Ret)
lmIML <- lm(ExcessNAV ~ IML, data = NAV.Ret)

## Scatterplots
par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(NAV.Ret$ExMSCI,NAV.Ret$ExcessNAV, pch=".", xlab="EMR", ylab="Excess NAV Return")
abline(h=0, col="red")
abline(a= lmEMR[1], b=lmEMR[2], lwd=2, col="blue")
plot(NAV.Ret$HYspread,NAV.Ret$ExcessNAV, pch=".", xlab="HYS", ylab="Excess NAV Return")
abline(h=0, col="red")
abline(a= lmHYS[1], b=lmHYS[2], lwd=2, col="blue")
plot(NAV.Ret$Liq.Trade,NAV.Ret$ExcessNAV, pch=".", xlab="IML", ylab="Excess NAV Return")
abline(h=0, col="red")
abline(a= lmIML[1], b=lmIML[2], lwd=2, col="blue")


## Multi-Factor Model lm() OLS    
#    ((summary(svm(I((Pri.Com.scaled-Yield1.World)) ~ I(MSCI.Return-1-Yield1.World) + I((HYspread)) + Liq.Trade + YCS, data = NAV.Ret, cost=2,epsilon=0.05,tolerance=0.01)))["fitted"])
#    lm.pf <- summary(lm(I(Pri.wc.scaled-1-Yield1.World) ~ I(MSCI.Return-1-Yield1.World) + I((HYspread)) + Liq.Trade, data = NAV.Ret))
#    lm.pf <- summary(lm(I(Pri.Com.scaled-1-Yield1.World) ~ I((HYspread)) + YCS, data = NAV.Ret))
lm.pf <- lm(I(log(ExcessNAV+1)) ~ EMR + HYS + IML, data = NAV.Ret, na.action = "na.exclude")
lm.pf <- lm(ENC.trans ~ EMR + HYS + IML, data = NAV.Ret, na.action = "na.exclude")
lm.pf <- lm(ExcessNAV ~ EMR + HYS.lm + IML, data = NAV.Ret[NAV.Ret$AD > as.Date("2001-06-01"),], na.action = "na.exclude")#, subset =  month(NAV.Ret$AD) %in% c(6))
lm.pf <- lm(ExcessNAV ~ EMR + HYS.lm + IML, data = NAV.Ret, na.action = "na.exclude")#, subset =  month(NAV.Ret$AD) %in% c(6))
#    lm.pf <- lm(ExcessNAV ~ ., data = NAV.Ret)
summary(lm.pf)
stepAIC(lm.pf)

# Durbin-Watson Test
dwtest(lm.pf) # not meaningful for cross-sectional data

## transformation, if necessary
## Gaussianize(predict(lm.pf), tau.mat = tm, inverse = TRUE)
NAV.Ret$predict.lm <- predict(lm.pf)
NAV.Ret$residuals.lm <- NAV.Ret$ExcessNAV - NAV.Ret$predict.lm
summary(NAV.Ret$residuals.lm)
#par(mfrow=c(2,1), mar=c(2,4,1,1))
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(density(NAV.Ret$residuals.lm, na.rm = TRUE), main="")
abline(v=mean(NAV.Ret$residuals.lm,na.rm=T), col="red")

## plot residuals
residuals.lm <- Gaussianize(lm.pf$residuals, tau.mat = tm, inverse = TRUE)
residuals.lm <- lm.pf$residuals
par(mfrow=c(1,1), mar=c(4,4,2,2))
hist(residuals.lm, breaks=50, freq = F,main=NULL,xlab="OLS Residuals")
rug(residuals.lm, col="darkgreen")
lines(density(residuals.lm), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(residuals.lm), sd= sd(residuals.lm)), col="darkblue",lwd=3,add=T)
legend("topright", x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal N(0,RSE) Density"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=NAV.Ret$AD, y=NAV.Ret$residuals.lm, pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(NAV.Ret$residuals.lm ~ NAV.Ret$AD, FUN=max)
resimin <- aggregate(NAV.Ret$residuals.lm ~ NAV.Ret$AD, FUN=min)
lines(resimin$`NAV.Ret$AD`, resimin$`NAV.Ret$residuals.lm`, col="red")
lines(resimax$`NAV.Ret$AD`, resimax$`NAV.Ret$residuals.lm`, col="blue")
legend("topright", inset = 0.01, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)

'   
## BOX COX transformation    
a <- boxcox( lm(I((Pri.Com.scaled-Yield1.World)) ~ ExcessMSCI + HYspread + Liq.Trade + YCS, data = NAV.Ret) )
b <- NAV.Ret$Pri.Com.scaled-NAV.Ret$Yield1.World
c <- (b^a$x[which.max(a$y)]-1)/a$x[which.max(a$y)]
hist(c) ; rm(a,b,c)

plot(density(lm.pf$residuals, na.rm = T), main="Residuals (OLS log Transformation)")
lines(density(rnorm(100000,mean(lm.pf$residuals, na.rm = T),sd(lm.pf$residuals, na.rm = T))), col="blue")

lm.pf <- summary(lm(ENC.trans ~ I(MSCI.Return-1-Yield1.World) + I((HYspread)) + Liq.Trade + YCS, data = NAV.Ret))
lm.pf#$coefficients
plot(density(lm.pf$residuals, na.rm = T), main="Residuals (OLS with Gaussianized ExNavRet)")
lines(density(rnorm(100000,mean(lm.pf$residuals, na.rm = T),sd(lm.pf$residuals, na.rm = T))), col="blue")
'
lmrob.pf <- summary(lmrob(I(Pri.wc.scaled-1-Yield1.World) ~ I(MSCI.Return-1-Yield1.World) + I(HYspread)+Liq.Trade, data = NAV.Ret))
lmrob.pf <- summary(lmrob(I(Pri.Com.scaled-1-Yield1.World) ~ I(MSCI.Return-1-Yield1.World) + I(HYspread)+Liq.Trade+YCS, data = NAV.Ret))
#    lmrob.pf <- summary(lmrob(ENC.trans ~ I(MSCI.Return-1-Yield1.World) + I(HYspread)+Liq.Trade+YCS, data = NAV.Ret))
lmrob.pf#$coefficients
svm.pf <- summary(svm(ENC.trans ~ I(MSCI.Return-1-Yield1.World) + I((HYspread)) + Liq.Trade + YCS, data = NAV.Ret, type="nu-regression"))
svm.pf # too many support vectors
# summary(lmrob(I(Pri.wc.scaled-1-Yield1.World) ~ I(MSCI.Return-1-Yield1.World) + I(WRA.Return-1-Yield10.US) + I(HYspread), data = NAV.Ret))
summary(lm(I(Pri.Com.scaled-1-Yield1.World) ~ Mkt.RF.GO.y + I((HYspread)) + Liq.Trade + CMA.GO.y, data = NAV.Ret))
summary(lm(I(Pri.Com.scaled-1-Yield1.World) ~ I(Yield10.World-Yield1.World) , data = NAV.Ret))
summary(lm.pf$residuals) # OLS regression has median -0.035
summary(lmrob.pf$residuals) # robust regression is biased

summary(NAV.Ret[, c("Pri.wc.scaled", "MSCI.Return", "HYspread", "Liq.Trade")])
cor(NAV.Ret[, c("Pri.wc.scaled", "MSCI.Return", "HYspread", "Liq.Trade")], use = "pairwise.complete.obs")

## SGT Estimation for Residuals
sta = c(mu= as.double(mean(lmrob.pf$residuals)),
        sigma= as.double(sd(lmrob.pf$residuals)),
        lambda=0.5,
        p=2,
        q=2)
sta
x <- lmrob.pf$residuals
x <- lm.pf$residuals[!is.na(lm.pf$residuals)]
result <- sgt.mle(X.f = ~ x, start=sta)
result
summary(result)
result$estimate["p"] * result$estimate["q"]
par(mfrow=c(2,1))
SGT.sim <- rsgt(8263, mu = result$estimate["mu"], sigma = result$estimate["sigma"], lambda = result$estimate["lambda"], p = result$estimate["p"],q = result$estimate["q"])
plot(density(SGT.sim),main="SGT Simulation", xlim=c(-2,2))
rug(SGT.sim, col="red")
lines(density(x), col="blue", lty=3, lwd=2)
plot(density(x), main= "observed residuals", xlim=c(-2,2))
rug(x, col="red")
mean(x) ; mean(SGT.sim) ; sd(x) ; sd(SGT.sim)

## Visualisation
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1), lwd=1)

plot(x= Public$Date,y= (Public$ExMSCI), type="s", col="black", ylim=c(-1.5,2), lwd=2, xlab="Date", ylab="Return (One Year)") # main= "Estimated One-Year Returns by Multi-Risk-Factor Model")
abline(h=c(-0.1,0.1,-0.2,0.2,-0.3,0.3,-0.4,0.4), col="grey", lty=2)
abline(h=0, col="grey", lwd=2)
lines(x= Public$Date, y= (Public$NASDAQ.Return-1-Public$Yield1.World/100), lwd=1, type = "s", col="grey")

lines(x= AR$Date, y= AR$Mean.Ret-1, col="orange", lwd = 1, type= "h")
lines(x= AR$Date, y= AR$Mean.Ret-1, col="orange", lwd = 2, type= "p")
lines(x= AR$Date, y= AR$Med.Ret-1, col="red", lwd = 2, type= "p")

lines(x= AR$Date, y= AR$Min-1, col="pink", lwd = 2, type= "p", pch=4)
lines(x= AR$Date, y= AR$Max-1, col="green", lwd = 2, type= "p", pch=4)

lines(x= Public$Date, col="blue", lwd = 2, type= "b",
      y= (lm.pf$coefficients[1] + Public$ExMSCI * lm.pf$coefficients[2] + Public$HYspread * lm.pf$coefficients[3] + Public$Liq.Trade * lm.pf$coefficients[4] + Public$Yield.Diff.World * lm.pf$coefficients[5]))
lines(x= Public$Date, col="darkgreen", lwd = 2, type= "b",
      y= (lmrob.pf$coefficients[1] + (Public$MSCI.Return-1-Public$Yield1.World/100) * lmrob.pf$coefficients[2] + Public$HYspread/100 * lmrob.pf$coefficients[3] + Public$Liq.Trade * lmrob.pf$coefficients[4] + Public$YCS * lmrob.pf$coefficients[5]))

legend("bottomleft", y.intersp=0.5, bty="o", box.lty=0, cex=1, legend=c("Robust LM", "OLS LM", paste("Hist. Median",Pri.Ret," "), paste("Hist. Mean",Pri.Ret," "),"Excess MSCI", "Excess Nasdaq"), col=c("darkgreen", "blue", "red", "orange","black","grey"),lty=c(1,1,1,1,1,1), pch=c(1,1,1,1,NA,NA), lwd=c(2,2,2,2,2,2)) 
legend("bottomright", y.intersp=0.5, box.lty=0, cex=1.3, legend=c("Max","Min"), pch=c(4,4), pt.cex= c(2,2), col=c("green","pink"))
########################################################
## 4.) AMT-factor models
########################
## 4.1) New Random Samples  ------
setwd("~/Dropbox/MaArbeit/PE Risk/R PE Risk")
rmExcept(c("Public", "Fund.Info", "pepf", "VinXAMT", "pooled1", "values1", "pooled.dt", 
         "lm.pf", "lmrob.pf", "NRPCS10000", "bcf", "current", "current.frame"))

## load (or create) new random samples
# system.time(nu.rs <- bcf(10,"all","random"))
#write.csv(nu.rs, file="randomsample13 (verstrichen ).csv")
rs7 <- read.csv(file = "randomsample7 (verstrichen 1354).csv", header = T)
rs8 <- read.csv(file = "randomsample8 (verstrichen 1369).csv", header = T)
rs9 <- read.csv(file = "randomsample9 (verstrichen 760).csv", header = T)
rs10 <- read.csv(file = "randomsample10 (verstrichen 763).csv", header = T)
rs11 <- read.csv(file = "randomsample11 (verstrichen 2527).csv", header = T)
rs12 <- read.csv(file = "randomsample12 (verstrichen 2673).csv", header = T)

nu.rs <- rbind(rs7,rs8,rs9,rs10,rs11,rs12)

nu.rs1 <- merge(x= nu.rs, y= Fund.Info, by.x = "Fund.ID", by.y = "Coded.fund_id", all.x=T)
nu.rs1$Date <- as.Date(nu.rs1$Date)
nu.rs1b <- merge(x= nu.rs1, y= Public, by="Date", all.x=T)
nu.rs2 <- subset(nu.rs1b, subset = NAV.date.diff < 12 & (Return.NAV > 0 | Return.NAV <= 0) & !(is.infinite(Return.NAV)))

rm(nu.rs,nu.rs1,nu.rs1b,rs7,rs8,rs9,rs10,rs11,rs12)

length(levels(as.factor(nu.rs2$Fund.ID)))
length(levels(Fund.Info$Coded.fund_id))

# Yield 1y
nu.rs2$Yield1 <- ifelse(nu.rs2$Fund.Focus == "US", nu.rs2$Yield1.US,
                        ifelse(nu.rs2$Fund.Focus == "Europe", nu.rs2$Yield1.EU,
                               1/3* nu.rs2$Yield1.EU + 2/3* nu.rs2$Yield1.US))
nu.rs2$Yield1 <- as.numeric(nu.rs2$Yield1)/100
# Yield 10y
nu.rs2$Yield10 <- ifelse(nu.rs2$Fund.Focus == "US", nu.rs2$Yield10.US,
                         ifelse(nu.rs2$Fund.Focus == "Europe", nu.rs2$Yield10.EU,
                                1/3* nu.rs2$Yield10.EU + 2/3* nu.rs2$Yield10.US))
nu.rs2$Yield10 <- as.numeric(nu.rs2$Yield10)/100

# Negative Yield Diff Indicator
nu.rs2$NYD <- ifelse(nu.rs2$Yield10-nu.rs2$Yield1 < 0, 1, 0)
nu.rs2$PYD <- ifelse(nu.rs2$Yield10-nu.rs2$Yield1 > 0.02, 1, 0)
summary(nu.rs2$PYD)

# Equity Region Return
nu.rs2$SP.Reg.Ret <- ifelse(nu.rs2$Fund.Focus == "US", nu.rs2$SP500.Return,
                            ifelse(nu.rs2$Fund.Focus == "Europe", nu.rs2$ES50.Return,
                                   nu.rs2$MSCI.Return))
nu.rs2$NAS.Reg.Ret <- ifelse(nu.rs2$Fund.Focus == "US", nu.rs2$NASDAQ.Return,
                             ifelse(nu.rs2$Fund.Focus == "Europe", nu.rs2$ES50.Return,
                                    nu.rs2$MSCI.Return))
nu.rs2 <- as.data.table(nu.rs2)

# Excess NAV Return
nu.rs2$XNavR <- nu.rs2$Return.NAV - 1 - nu.rs2$Yield1
## 4.2) Single Fund Modeling (based on ? threshold)  ---------

## define threshold
threshold <- "none"
# GFRM.level <- 0.05 ; 
thres.RVPI <- 0.3 ; thres.CC <- 3.0e+06 ; thres.NAV <- 2.5e+06 ; th.NAV.ra <- 0.5e+06


## Robust & OLS Linear Models
##    a) RE  ------------------
Fund.Type <- c("RE")
## ROBUST RE
if(threshold == "hard"){
  r.re <- (lmrob(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type & RVPI > thres.RVPI & CC > thres.CC))
}
if(threshold == "soft"){
  r.re <- (lmrob(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.re <- (lmrob(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.re <- (lmrob(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
lr.re <- summary(r.re)
lr.re
summary(lr.re$residuals)

## OLS RE
if(threshold == "hard"){
  m.re <- (lm(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type & RVPI > thres.RVPI & CC > thres.CC, na.action = "na.exclude"))
}
if(threshold == "soft"){
  m.re <- (lm(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type, na.action = "na.exclude"))
}
if(threshold == "NAV"){
  m.re <- (lm(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type, na.action = "na.exclude"))
}
if(threshold == "none"){
  m.re <- (lm(I((Return.NAV-1-Yield1)) ~ I(RE.US.com.Return-1-Yield1)+Liq.Trade+0, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type, na.action = "na.exclude"))
}
lm.re <- summary(m.re)
lm.re
nu.rs2[,Resi.lm := NULL]
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.re, nu.rs2[AssetMetrixType %in% Fund.Type,]) 
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.re$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.re$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.re$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals")
rug(lm.re$residuals, col="darkgreen")
lines(density(lm.re$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.re$residuals), sd= sd(lm.re$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.re$sigma), col="orange",lwd=2,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)

## plot residuals vs. NAV (pseudo-heteroscedasticity)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, NAV], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="NAV",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
##    b) FOF  -----------
Fund.Type <- c("FOF")
if(threshold == "hard"){
  r.fof <- (lmrob(I((Return.NAV-1-Yield1)) ~ I(SP.Reg.Ret-1-Yield1)+HYspread+Liq.Trade, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  r.fof <- (lmrob(I((Return.NAV-1-Yield1)) ~ I(SP.Reg.Ret-1-Yield1)+HYspread+Liq.Trade, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.fof <- (lmrob(I((Return.NAV-1-Yield1)) ~ I(SP.Reg.Ret-1-Yield1)+HYspread+Liq.Trade, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.fof <- (lmrob(I((Return.NAV-1-Yield1)) ~ Mkt.RF.GO.y+HYspread+Liq.Trade, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}

lr.fof <- summary(r.fof)
lr.fof

## LM
if(threshold == "hard"){
  m.fof <- (lm(I((Return.NAV-1-Yield1)) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  m.fof <- (lm(I((Return.NAV-1-Yield1)) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  m.fof <- (lm(I((Return.NAV-1-Yield1)) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  m.fof <- (lm(I((Return.NAV-1-Yield1)) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
lm.fof <- summary(m.fof)
lm.fof
nu.rs2$Resi.lm <- NA
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.fof, nu.rs2[AssetMetrixType %in% Fund.Type,])
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.fof$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.fof$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.fof$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals")
rug(lm.fof$residuals, col="darkgreen")
lines(density(lm.fof$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.fof$residuals), sd= sd(lm.fof$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.fof$sigma), col="orange",lwd=2,add=T)
# legend(x=1.,y=3.5, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)

## plot residuals vs. NAV (pseudo-heteroscedasticity)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, NAV], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="NAV",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
##    c) BO  ----------
Fund.Type <- c("BO")
if(threshold == "hard"){
  r.bo <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+I(HYspread)+Liq.Trade, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  r.bo <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+I(HYspread)+Liq.Trade, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.bo <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+I(HYspread)+Liq.Trade, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.bo <- (lmrob(I(Return.NAV-1-Yield1) ~ Mkt.RF.GO.y +HYspread+Liq.Trade, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}

lr.bo <- summary(r.bo)
lr.bo

if(threshold == "hard"){
  m.bo <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  m.bo <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  m.bo <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  m.bo <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}

lm.bo <- summary(m.bo)
lm.bo
nu.rs2$Resi.lm <- NA
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.bo, nu.rs2[AssetMetrixType %in% Fund.Type,])
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.bo$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.bo$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.bo$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals", ylim=c(0,3))
rug(lm.bo$residuals, col="darkgreen")
lines(density(lm.bo$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.bo$residuals), sd= sd(lm.bo$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.bo$sigma), col="orange",lwd=2,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)

## plot residuals vs. NAV (pseudo-heteroscedasticity)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, NAV], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="NAV",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
##    d) VC -------------
Fund.Type <- c("VC")
if(threshold == "hard"){
  r.vc <- (lmrob(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  r.vc <- (lmrob(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.vc <- (lmrob(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV.Range"){
  r.vc <- (lmrob(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = NAV %between% c(8e+06 - th.NAV.ra, 8e+06 + th.NAV.ra) & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.vc <- (lmrob(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
if(threshold == "weights"){
  r.vc <- (lmrob(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type, weights = NAV))
}
lr.vc <- summary(r.vc)
lr.vc

if(threshold == "hard"){
  m.vc <- (lm(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  m.vc <- (lm(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  m.vc <- (lm(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  m.vc <- (lm(I(Return.NAV-1-Yield1) ~ I(NAS.Reg.Ret-1-Yield1)+HYS.lm+Liq.Trade, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
lm.vc <- summary(m.vc)
lm.vc

nu.rs2$Resi.lm <- NA
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.vc, nu.rs2[AssetMetrixType %in% Fund.Type,])
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm2"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.vc, nu.rs2[AssetMetrixType %in% Fund.Type,])
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.vc$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.vc$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.vc$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals", ylim=c(0,2))
rug(lm.vc$residuals, col="darkgreen")
lines(density(lm.vc$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.vc$residuals), sd= sd(lm.vc$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.vc$sigma), col="orange",lwd=2,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)

## plot residuals vs. NAV (pseudo-heteroscedasticity)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, NAV], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm2], 
     pch=".", xlab="NAV",ylab="Residuals", cex=3, col="darkgreen", ylim=c(-10,10))
abline(h=0,col="black", lwd=2)
abline(v=thres.NAV)

# residuals vector
summary(nu.rs2[!is.na(Resi.lm2),Resi.lm2])
nv <- 0.5e+06
range <- 0.25e+06
revec <- nu.rs2[NAV %between% c(nv - range, nv + range), Resi.lm2]
summary(revec) ; length(revec) ; length(revec) - summary(revec)["NA's"]
summary(nu.rs2[NAV < 2.5e+05,Resi.lm2])
rm(nv,range,revec)
##    e) DD  -----------
Fund.Type <- c("DD")
if(threshold == "hard"){
  r.dd <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+0, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  r.dd <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+0, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.dd <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+0, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.dd <- (lmrob(I(Return.NAV-1-Yield1) ~Mkt.RF.GO.y+HYS.lm+0, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}

lr.dd <- summary(r.dd)
lr.dd

if(threshold == "hard"){
  m.dd <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+0, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  m.dd <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+0, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  m.dd <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+0, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  m.dd <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm+0, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}

lm.dd <- summary(m.dd)
lm.dd
nu.rs2$Resi.lm <- NA
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.dd, nu.rs2[AssetMetrixType %in% Fund.Type,])
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.dd$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.dd$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.dd$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals")
rug(lm.dd$residuals, col="darkgreen")
lines(density(lm.dd$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.dd$residuals), sd= sd(lm.dd$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.dd$sigma), col="orange",lwd=2,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)
##    f) MEZZ ---------
Fund.Type <- c("MEZZ")
if(threshold == "hard"){
  r.mezz <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  r.mezz <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.mezz <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.mezz <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1) + HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
  r.mezz <- (lmrob(I(Return.NAV-1-Yield1) ~ Mkt.RF.GO.y + HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}

lr.mezz <- summary(r.mezz)
lr.mezz

if(threshold == "hard"){
  m.mezz <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  m.mezz <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  m.mezz <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  m.mezz <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
lm.mezz <- summary(m.mezz)
lm.mezz
nu.rs2$Resi.lm <- NA
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.mezz, nu.rs2[AssetMetrixType %in% Fund.Type,])
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.mezz$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.mezz$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.mezz$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals")
rug(lm.mezz$residuals, col="darkgreen")
lines(density(lm.mezz$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.mezz$residuals), sd= sd(lm.mezz$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.mezz$sigma), col="orange",lwd=2,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)
##    g) NatRes ----------
Fund.Type <- c("NatRes")
if(threshold == "hard"){
  r.nr <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  r.nr <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.nr <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.nr <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
  r.nr <- (lmrob(I(Return.NAV-1-Yield1) ~ Mkt.RF.GO.y +HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}

lr.nr <- summary(r.nr)
lr.nr

if(threshold == "hard"){
  m.nr <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  m.nr <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  m.nr <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  m.nr <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
lm.nr <- summary(m.nr)
lm.nr
nu.rs2$Resi.lm <- NA
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.nr, nu.rs2[AssetMetrixType %in% Fund.Type,])
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.nr$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.nr$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.nr$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals")
rug(lm.nr$residuals, col="darkgreen")
lines(density(lm.nr$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.nr$residuals), sd= sd(lm.nr$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.nr$sigma), col="orange",lwd=2,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)
##    h) Infrastructure  ---------
Fund.Type <- c("Infrastructure")
if(threshold == "hard"){
  r.is <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  r.is <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  r.is <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  r.is <- (lmrob(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
  r.is <- (lmrob(I(Return.NAV-1-Yield1) ~ Mkt.RF.GO.y + HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
lr.is <- summary(r.is)
lr.is

if(threshold == "hard"){
  m.is <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = RVPI > thres.RVPI & CC > thres.CC & AssetMetrixType %in% Fund.Type))
}
if(threshold == "soft"){
  m.is <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = GRM < GFRM.level & AssetMetrixType %in% Fund.Type))
}
if(threshold == "NAV"){
  m.is <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = NAV > thres.NAV & AssetMetrixType %in% Fund.Type))
}
if(threshold == "none"){
  m.is <- (lm(I(Return.NAV-1-Yield1) ~ I(SP.Reg.Ret-1-Yield1)+HYS.lm, data= nu.rs2, subset = AssetMetrixType %in% Fund.Type))
}
lm.is <- summary(m.is)
lm.is
nu.rs2$Resi.lm <- NA
nu.rs2[AssetMetrixType %in% Fund.Type, "Resi.lm"] <- nu.rs2[AssetMetrixType %in% Fund.Type, XNavR] - predict(m.is, nu.rs2[AssetMetrixType %in% Fund.Type,])
if(threshold == "hard"){
  nu.rs2[RVPI <= thres.RVPI | CC <= thres.CC, "Resi.lm"] <- NA
}
if(threshold == "soft"){
  nu.rs2[GRM >= GFRM.level, "Resi.lm"] <- NA
}
if(threshold == "NAV"){
  nu.rs2[NAV <= thres.NAV, "Resi.lm"] <- NA
}
summary(lm.is$residuals) ; summary(nu.rs2$Resi.lm)
length(lm.is$residuals) ; nrow(nu.rs2[!is.na(Resi.lm),])

## plot residuals
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(lm.is$residuals, breaks=50, freq = F,main=NULL,xlab="Residuals")
rug(lm.is$residuals, col="darkgreen")
lines(density(lm.is$residuals), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(lm.is$residuals), sd= sd(lm.is$residuals)), col="darkblue",lwd=3,add=T)
curve(dnorm(x, mean= 0, sd= lr.is$sigma), col="orange",lwd=2,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x=nu.rs2[AssetMetrixType ==  Fund.Type, Date], 
     y=nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm], 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=max)
resimin <- aggregate(nu.rs2[AssetMetrixType ==  Fund.Type,Resi.lm] ~ nu.rs2[AssetMetrixType ==  Fund.Type,Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)
##    xxx) List of Linear Models  ------------------

MR.list <- list(
  M = list(BO= m.bo,DD= m.dd,FOF= m.fof,IS= m.is,MEZZ= m.mezz,NR= m.nr,RE=m.re,VC= m.vc),
  R = list(BO= r.bo,DD= r.dd,FOF= r.fof,IS= r.is,MEZZ= r.mezz,NR= r.nr,RE=r.re,VC= r.vc)
)

rmExcept(c("Fund.Info","nu.rs2","Public","MR.list", "pepf", "lm.pf", "lmrob.pf","NRPCS10000"))
## 4.3) AMT Index Building  -------------
'
AMT <- levels(Fund.Info$AssetMetrixType)[10]
AMTindex <- data.frame(Date=seq(from= as.Date("1985-12-31"), to=as.Date("2016-12-31"), by="quarter"), 
AggNAV.ad=NA,AggNCF.ad=NA,AMT=AMT)
AMTindex$Date <- as.Date(ifelse(month(AMTindex$Date) %in% c(7,10), as.character(AMTindex$Date-1),as.character(AMTindex$Date)))
fi <- Fund.Info[Fund.Info$AssetMetrixType==AMT,"Coded.fund_id"]
for(m in 1:nrow(AMTindex)){
nav <- vector() ; ncf.ad <- vector() ; ncf.fd <- vector()
ad <- as.Date(AMTindex$Date[m])

for(i in 1:length(fi)){
nav[i] <- as.numeric( current(ad ,as.character( fi[i] ) )["NAV"] )
ncf.ad[i] <-  as.numeric( current(ad ,as.character( fi[i] ) )["NCF"] )
}

AMTindex$AggNAV.ad[m] <- sum(nav, na.rm = T)
AMTindex$AggNCF.ad[m] <- sum(ncf.ad, na.rm = T)

}

AMTindex$AggNAV.fd.q <- c(AMTindex$AggNAV.ad[2:nrow(AMTindex)] , NA)
AMTindex$AggNCF.fd.q <- c(AMTindex$AggNCF.ad[2:nrow(AMTindex)] , NA)
AMTindex$NAVret.q <- (AMTindex$AggNAV.fd.q + AMTindex$AggNCF.fd.q - AMTindex$AggNCF.ad) / AMTindex$AggNAV.ad

AMTindex$AggNAV.fd.y <- c(AMTindex$AggNAV.ad[5:nrow(AMTindex)] , rep(NA,4))
AMTindex$AggNCF.fd.y <- c(AMTindex$AggNCF.ad[5:nrow(AMTindex)] , rep(NA,4))
AMTindex$NAVret.y <- (AMTindex$AggNAV.fd.y + AMTindex$AggNCF.fd.y - AMTindex$AggNCF.ad) / AMTindex$AggNAV.ad

# write.csv(AMTindex, file="AMTindexVC.csv")
'


# Regressions (Beta Quagging: quarterly aggregating)
QR.summaries <- list() ; QR.s <- 1
QR.list <- list()
for(Reg in c("lmrob","lm")){
  Qualm.list <- list()
  for(type in c("BO","VC","FOF","DD","RE")){
    
    Regression <- Reg
    AMT <- type
    AMTindex <- read.csv(file=paste("AMTindex",AMT,".csv",sep=""))
    AMTindex$Date <- as.Date(AMTindex$Date)
    
    for(i in c(3,6,9,12)){
      
      PCFindex <- AMTindex[month(AMTindex$Date) == i & is.finite(AMTindex$NAVret.y) & AMTindex$Date < as.Date("2015-03-31"),]
      PCFindex <- merge(x=PCFindex, y= Public[,c("Date","Mkt.RF.GO.y","ExMSCI","ExNAS","ExSP500", "RE.US.com.Return", "CFSI", "VTM.Return",
                                                 "HYspread","HYS.lm","Liq.Trade","Yield1.World")], by="Date",all.x = T)
      if(Regression == "lmrob"){
        if(AMT == "VC"){
          limo <- lmrob(I(NAVret.y-1-Yield1.World) ~ ExNAS + HYS.lm, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "RE"){
          limo <- lmrob(I(NAVret.y-1-Yield1.World) ~ I(RE.US.com.Return-1-Yield1.World) + HYspread, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "BO"){
          limo <- lmrob(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y + HYS.lm, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "DD"){
          limo <- lmrob(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y + HYS.lm + 0, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "FOF"){
          limo <- lmrob(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y, data=PCFindex)
          qualm <- summary(limo)
        }
      }  
      if(Regression == "lm"){
        if(AMT == "VC"){
          limo <- lm(I(NAVret.y-1-Yield1.World) ~ ExNAS + HYS.lm, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "RE"){
          limo <- lm(I(NAVret.y-1-Yield1.World) ~ I(RE.US.com.Return-1-Yield1.World) + HYspread, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "BO"){
          limo <- lm(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y + HYS.lm, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "DD"){
          limo <- lm(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y + HYS.lm + 0, data=PCFindex)
          qualm <-  summary(limo)
        }
        if(AMT == "FOF"){
          limo <- lm(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y, data=PCFindex)
          qualm <- summary(limo)
        }
      }  
      
      assign(paste("m",month(PCFindex$Date)[1],sep=""), qualm)
      
      QR.summaries[[paste("limo",QR.s,sep="")]] <- limo
      QR.s <- QR.s + 1
    }
    
    # assign(paste("Qualm.",AMT,sep=""), as.data.frame(rbind(m3$coefficients,m6$coefficients,m9$coefficients,m12$coefficients)) )
    Qualm.list[[AMT]] <- data.frame(
      as.data.frame(rbind(m3$coefficients,m6$coefficients,m9$coefficients,m12$coefficients), 
                    row.names = make.names(
                      row.names(as.data.frame(rbind(m3$coefficients,m6$coefficients,m9$coefficients,m12$coefficients))), unique = TRUE
                    )),
      coefname = row.names(as.data.frame(rbind(m3$coefficients,m6$coefficients,m9$coefficients,m12$coefficients)))
    )
    
    for(l  in  levels(Qualm.list[[AMT]][, "coefname"])  ){
      Qualm.list[[AMT]][paste("mean",l,sep=""),"Estimate"] <- mean(Qualm.list[[AMT]][Qualm.list[[AMT]]$coefname == l, "Estimate"], na.rm = TRUE)
    }
    
    
    rm(m3,m6,m9,m12)
  }
  QR.list[[Reg]] <- Qualm.list
}

summary(QR.summaries$limo40)

QR.list$lm$BO
QR.list$lmrob$BO
QR.list$lm$VC
QR.list$lmrob$VC
QR.list$lm$FOF
QR.list$lmrob$FOF
QR.list$lm$DD
QR.list$lmrob$DD
QR.list$lm$RE
QR.list$lmrob$RE

## NO Models found 
# NatRes (history since 1992)
summary(lmrob(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y + 0, data=PCFindex))
# Infra (history since 2003)
summary(lm(I(NAVret.y-1-Yield1.World) ~ ExMSCI + 0, data=PCFindex))
# MEZZ (history since 1987)
summary(lmrob(I(NAVret.y-1-Yield1.World) ~ Mkt.RF.GO.y, data=PCFindex))

rm(qualm, type, Regression, Reg, AMTindex, PCFindex,l, QR.s, i, Qualm.list, limo, AMT)
## 4.4) Residuals Calculation & Listing & Visu ---------------

## BQ predict function (conditional Asset Class Mean return)
BQ <- function(AMT, Fund.Focus, HYs, XGO, XNAS, XREI, Y1,Liq,IQM){
  #  AMT <- as.character(AMT) ; Fund.Focus <- as.character(Fund.Focus)
  
  ca1 <- ca2 <- ca3 <- ca4 <- ca0 <- ufs <- NA_integer_
  
  # Index Quagging (+ USF)
  if(IQM == "lmrob"){
    if(AMT == "RE"){
      X <- QR.list$lmrob$RE
      ca1 <- X["X.Intercept.", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World.", "Estimate"] + HYs * X["HYspread", "Estimate"] 
      ca2 <- X["X.Intercept..1", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World..1", "Estimate"] + HYs * X["HYspread.1", "Estimate"] 
      ca3 <- X["X.Intercept..2", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World..2", "Estimate"] + HYs * X["HYspread.2", "Estimate"] 
      ca4 <- X["X.Intercept..3", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World..3", "Estimate"] + HYs * X["HYspread.3", "Estimate"] 
      ca0 <- X["mean(Intercept)", "Estimate"] + XREI * X["meanI(RE.US.com.Return - 1 - Yield1.World)", "Estimate"] + HYs * X["meanHYspread", "Estimate"] 
      ufs <- XREI * MR.list$R$RE$coefficients["I(RE.US.com.Return - 1 - Yield1)"] + Liq * MR.list$R$RE$coefficients["Liq.Trade"]
    }
    if(AMT == "BO"){
      X <- QR.list$lmrob$BO
      ca1 <- X["X.Intercept.", "Estimate"] + XGO * X["Mkt.RF.GO.y","Estimate"] + HYs * X["HYS.lm","Estimate"]
      ca2 <- X["X.Intercept..1", "Estimate"] + XGO * X["Mkt.RF.GO.y.1","Estimate"] + HYs * X["HYS.lm.1","Estimate"]
      ca3 <- X["X.Intercept..2", "Estimate"] + XGO * X["Mkt.RF.GO.y.2","Estimate"] + HYs * X["HYS.lm.2","Estimate"]
      ca4 <- X["X.Intercept..3", "Estimate"] + XGO * X["Mkt.RF.GO.y.3","Estimate"] + HYs * X["HYS.lm.3","Estimate"]
      ca0 <- X["mean(Intercept)", "Estimate"] + XGO * X["meanMkt.RF.GO.y","Estimate"] + HYs * X["meanHYS.lm","Estimate"]
      ufs <- MR.list$R$BO$coefficients["(Intercept)"] + XGO * MR.list$R$BO$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$BO$coefficients["HYspread"] + Liq * MR.list$R$BO$coefficients["Liq.Trade"]
    }
    if(AMT == "VC"){
      X <- QR.list$lmrob$VC
      ca1 <- X["X.Intercept.","Estimate"] + XNAS * X["ExNAS","Estimate"] + HYs * X["HYS.lm","Estimate"]
      ca2 <- X["X.Intercept..1","Estimate"] + XNAS * X["ExNAS.1","Estimate"] + HYs * X["HYS.lm.1","Estimate"]
      ca3 <- X["X.Intercept..2","Estimate"] + XNAS * X["ExNAS.2","Estimate"] + HYs * X["HYS.lm.2","Estimate"]
      ca4 <- X["X.Intercept..3","Estimate"] + XNAS * X["ExNAS.3","Estimate"] + HYs * X["HYS.lm.3","Estimate"]
      ca0 <- X["mean(Intercept)","Estimate"] + XNAS * X["meanExNAS","Estimate"] + HYs * X["meanHYS.lm","Estimate"]
      ufs <- MR.list$R$VC$coefficients["(Intercept)"] + XNAS * MR.list$R$VC$coefficients["I(NAS.Reg.Ret - 1 - Yield1)"] + HYs * MR.list$R$VC$coefficients["HYS.lm"] + Liq * MR.list$R$VC$coefficients["Liq.Trade"]
    }
    if(AMT == "DD"){
      X <- QR.list$lmrob$DD
      ca1 <- XGO * X["Mkt.RF.GO.y","Estimate"] + HYs * X["HYS.lm","Estimate"]
      ca2 <- XGO * X["Mkt.RF.GO.y.1","Estimate"] + HYs * X["HYS.lm.1","Estimate"]
      ca3 <- XGO * X["Mkt.RF.GO.y.2","Estimate"] + HYs * X["HYS.lm.2","Estimate"]
      ca4 <- XGO * X["Mkt.RF.GO.y.3","Estimate"] + HYs * X["HYS.lm.3","Estimate"]
      ca0 <- XGO * X["meanMkt.RF.GO.y","Estimate"] + HYs * X["meanHYS.lm","Estimate"]
      ufs <- XGO * MR.list$R$DD$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$DD$coefficients["HYS.lm"]
    }
    if(AMT == "FOF"){
      X <- QR.list$lmrob$FOF
      ca1 <- X["X.Intercept.","Estimate"] + XGO * X["Mkt.RF.GO.y","Estimate"]
      ca2 <- X["X.Intercept..1","Estimate"] + XGO * X["Mkt.RF.GO.y.1","Estimate"]    
      ca3 <- X["X.Intercept..2","Estimate"] + XGO * X["Mkt.RF.GO.y.2","Estimate"]
      ca4 <- X["X.Intercept..3","Estimate"] + XGO * X["Mkt.RF.GO.y.3","Estimate"]
      ca0 <- X["mean(Intercept)","Estimate"] + XGO * X["meanMkt.RF.GO.y","Estimate"]
      ufs <- MR.list$R$FOF$coefficients["(Intercept)"] + XGO * MR.list$R$FOF$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$FOF$coefficients["HYspread"] + Liq * MR.list$R$FOF$coefficients["Liq.Trade"]
    }
  }
  if(IQM == "lm"){
    if(AMT == "RE"){
      X <- QR.list$lm$RE
      ca1 <- X["X.Intercept.", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World.", "Estimate"] + HYs * X["HYspread", "Estimate"] 
      ca2 <- X["X.Intercept..1", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World..1", "Estimate"] + HYs * X["HYspread.1", "Estimate"] 
      ca3 <- X["X.Intercept..2", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World..2", "Estimate"] + HYs * X["HYspread.2", "Estimate"] 
      ca4 <- X["X.Intercept..3", "Estimate"] + XREI * X["I.RE.US.com.Return...1...Yield1.World..3", "Estimate"] + HYs * X["HYspread.3", "Estimate"] 
      ca0 <- X["mean(Intercept)", "Estimate"] + XREI * X["meanI(RE.US.com.Return - 1 - Yield1.World)", "Estimate"] + HYs * X["meanHYspread", "Estimate"] 
      ufs <- XREI * MR.list$R$RE$coefficients["I(RE.US.com.Return - 1 - Yield1)"] + Liq * MR.list$R$RE$coefficients["Liq.Trade"]
    }
    if(AMT == "BO"){
      X <- QR.list$lm$BO
      ca1 <- X["X.Intercept.", "Estimate"] + XGO * X["Mkt.RF.GO.y","Estimate"] + HYs * X["HYS.lm","Estimate"]
      ca2 <- X["X.Intercept..1", "Estimate"] + XGO * X["Mkt.RF.GO.y.1","Estimate"] + HYs * X["HYS.lm.1","Estimate"]
      ca3 <- X["X.Intercept..2", "Estimate"] + XGO * X["Mkt.RF.GO.y.2","Estimate"] + HYs * X["HYS.lm.2","Estimate"]
      ca4 <- X["X.Intercept..3", "Estimate"] + XGO * X["Mkt.RF.GO.y.3","Estimate"] + HYs * X["HYS.lm.3","Estimate"]
      ca0 <- X["mean(Intercept)", "Estimate"] + XGO * X["meanMkt.RF.GO.y","Estimate"] + HYs * X["meanHYS.lm","Estimate"]
      ufs <- MR.list$R$BO$coefficients["(Intercept)"] + XGO * MR.list$R$BO$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$BO$coefficients["HYspread"] + Liq * MR.list$R$BO$coefficients["Liq.Trade"]
    }
    if(AMT == "VC"){
      X <- QR.list$lm$VC
      ca1 <- X["X.Intercept.","Estimate"] + XNAS * X["ExNAS","Estimate"] + HYs * X["HYS.lm","Estimate"]
      ca2 <- X["X.Intercept..1","Estimate"] + XNAS * X["ExNAS.1","Estimate"] + HYs * X["HYS.lm.1","Estimate"]
      ca3 <- X["X.Intercept..2","Estimate"] + XNAS * X["ExNAS.2","Estimate"] + HYs * X["HYS.lm.2","Estimate"]
      ca4 <- X["X.Intercept..3","Estimate"] + XNAS * X["ExNAS.3","Estimate"] + HYs * X["HYS.lm.3","Estimate"]
      ca0 <- X["mean(Intercept)","Estimate"] + XNAS * X["meanExNAS","Estimate"] + HYs * X["meanHYS.lm","Estimate"]
      ufs <- MR.list$R$VC$coefficients["(Intercept)"] + XNAS * MR.list$R$VC$coefficients["I(NAS.Reg.Ret - 1 - Yield1)"] + HYs * MR.list$R$VC$coefficients["HYS.lm"] + Liq * MR.list$R$VC$coefficients["Liq.Trade"]
    }
    if(AMT == "DD"){
      X <- QR.list$lm$DD
      ca1 <- XGO * X["Mkt.RF.GO.y","Estimate"] + HYs * X["HYS.lm","Estimate"]
      ca2 <- XGO * X["Mkt.RF.GO.y.1","Estimate"] + HYs * X["HYS.lm.1","Estimate"]
      ca3 <- XGO * X["Mkt.RF.GO.y.2","Estimate"] + HYs * X["HYS.lm.2","Estimate"]
      ca4 <- XGO * X["Mkt.RF.GO.y.3","Estimate"] + HYs * X["HYS.lm.3","Estimate"]
      ca0 <- XGO * X["meanMkt.RF.GO.y","Estimate"] + HYs * X["meanHYS.lm","Estimate"]
      ufs <- XGO * MR.list$R$DD$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$DD$coefficients["HYS.lm"]
    }
    if(AMT == "FOF"){
      X <- QR.list$lm$FOF
      ca1 <- X["X.Intercept.","Estimate"] + XGO * X["Mkt.RF.GO.y","Estimate"]
      ca2 <- X["X.Intercept..1","Estimate"] + XGO * X["Mkt.RF.GO.y.1","Estimate"]    
      ca3 <- X["X.Intercept..2","Estimate"] + XGO * X["Mkt.RF.GO.y.2","Estimate"]
      ca4 <- X["X.Intercept..3","Estimate"] + XGO * X["Mkt.RF.GO.y.3","Estimate"]
      ca0 <- X["mean(Intercept)","Estimate"] + XGO * X["meanMkt.RF.GO.y","Estimate"]
      ufs <- MR.list$R$FOF$coefficients["(Intercept)"] + XGO * MR.list$R$FOF$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$FOF$coefficients["HYspread"] + Liq * MR.list$R$FOF$coefficients["Liq.Trade"]
    }
  }
  # Unfiltered Single Fund
  if(AMT =="MEZZ"){
    ufs <- MR.list$R$MEZZ$coefficients["(Intercept)"] + XGO * MR.list$R$MEZZ$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$MEZZ$coefficients["HYS.lm"]
  }
  if(AMT =="NatRes"){
    ufs <- MR.list$R$NR$coefficients["(Intercept)"] + XGO * MR.list$R$NR$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$NR$coefficients["HYS.lm"]
  }
  if(AMT =="Infrastructure"){
    ufs <- MR.list$R$IS$coefficients["(Intercept)"] + XGO * MR.list$R$IS$coefficients["Mkt.RF.GO.y"] + HYs * MR.list$R$IS$coefficients["HYS.lm"]
  }
  
  op <- list(ca1,ca2,ca3,ca4,ca0, ufs)
  names(op) <- c("ACM1","ACM2","ACM3","ACM4","ACM.Q", "UFS")
  op
}

# test BQ function
BQ("VC","US", HYs = 0.06, XGO = 0.5, XNAS = 0.3,XREI = 0.3, Y1 = 0.02, Liq=0, "lmrob")

# Calculate all Residuals
bqp <- data.frame()
system.time(
for(i in 1:nrow(nu.rs2)){
  XX <- BQ(nu.rs2$AssetMetrixType[i], nu.rs2$Fund.Focus[i], nu.rs2$HYS.lm[i], nu.rs2$Mkt.RF.GO.y[i], nu.rs2$ExNAS[i], nu.rs2$ExRE[i], nu.rs2$Yield1[i], nu.rs2$Liq.Trade[i], "lmrob")
  for(j in c("ACM1","ACM2","ACM3","ACM4","ACM.Q","UFS")){
    bqp[i,j] <- XX[j]
  }
})
rm(i,j,XX)

# Filter Residuals by AMT & NAV
nu.rs3 <- cbind(nu.rs2, bqp)
nu.rs3$ACR1 <- nu.rs3$XNavR - nu.rs3$ACM1
nu.rs3$ACR2 <- nu.rs3$XNavR - nu.rs3$ACM2
nu.rs3$ACR3 <- nu.rs3$XNavR - nu.rs3$ACM3
nu.rs3$ACR4 <- nu.rs3$XNavR - nu.rs3$ACM4
nu.rs3$ACR.Q <- nu.rs3$XNavR - nu.rs3$ACM.Q
nu.rs3$UFR <- nu.rs3$XNavR - nu.rs3$UFS
summary(nu.rs3[,.(ACR1,ACR2,ACR3,ACR4,ACR.Q,UFR)])

# Residuals List (overall)
CutOff <- 0.2 ; split <- (1-CutOff)/2 + CutOff
ResiList2 <- list()
for(AMT in c("BO","VC","FOF","DD","RE","MEZZ","Infrastructure","NatRes")){
  AA <- nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],CutOff),]
  if(AMT %in% c("BO","VC","FOF","DD","RE")){
    IQ.list <- list()
    for(IQ in c("ACR1","ACR2","ACR3","ACR4","ACR.Q")){
      IQ.list[[IQ]] <- AA[,get(IQ)]
    }
    ResiList2[[AMT]][["IQ"]] <- IQ.list
  }
  ResiList2[[AMT]][["SF"]] <- list(as.numeric(AA[,UFR]))
}
# Residual list (bottom 40%)
ResiList3 <- list()
for(AMT in c("BO","VC","FOF","DD","RE","MEZZ","Infrastructure","NatRes")){
  AA <- nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],CutOff) & NAV < quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],split),]
  if(AMT %in% c("BO","VC","FOF","DD","RE")){
    IQ.list <- list()
    for(IQ in c("ACR1","ACR2","ACR3","ACR4","ACR.Q")){
      IQ.list[[IQ]] <- AA[,get(IQ)]
    }
    ResiList3[[AMT]][["IQ"]] <- IQ.list
  }
  ResiList3[[AMT]][["SF"]] <- list(as.numeric(AA[,UFR]))
}
# Residual list (top 40%)
ResiList4 <- list()
for(AMT in c("BO","VC","FOF","DD","RE","MEZZ","Infrastructure","NatRes")){
  AA <- nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],split),]
  if(AMT %in% c("BO","VC","FOF","DD","RE")){
    IQ.list <- list()
    for(IQ in c("ACR1","ACR2","ACR3","ACR4","ACR.Q")){
      IQ.list[[IQ]] <- AA[,get(IQ)]
    }
    ResiList4[[AMT]][["IQ"]] <- IQ.list
  }
  ResiList4[[AMT]][["SF"]] <- list(as.numeric(AA[,UFR]))
}

rm(IQ.list,IQ,CutOff, split)
density(ResiList2$BO$IQ$ACR1, na.rm = TRUE)

## plot residuals
AMT <- "BO"
NRet <- nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),ACR.Q]
Dates <- nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),Date]
par(mfrow=c(2,1), mar=c(4,4,1,1))
hist(NRet,freq = F, breaks= 50, main=NULL,xlab="Residuals", xlim = c(-2,5), ylim=c(0,2.5))
rug(NRet, col="darkgreen")
lines(density(NRet,na.rm = T), col="darkgreen", lwd=3)
curve(dnorm(x, mean= mean(NRet), sd= sd(NRet)), col="darkblue",lwd=3,add=T)
#legend(x=1.25,y=1, x.intersp= 1, y.intersp = 1.25, legend = c("Residuals' Kernel","Normal Fit Kernel"), box.lty=0, lty=c(1,1),col=c("darkgreen","darkblue"), lwd=c(3,3))

# Time evolution of residuals
plot(x= Dates,
     y= NRet, 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),ACR.Q] ~ nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),Date], FUN=max)
resimin <- aggregate(nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),ACR.Q] ~ nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)
rm(resimin,resimax)

## plot residuals vs. NAV (pseudo-heteroscedasticity)
par(mfrow=c(2,2), mar=c(3,3,1,1),oma=c(2,2,0,0))
for(AMT in c("BO","VC","FOF","RE")){
  plot(x=nu.rs3[AssetMetrixType ==  AMT, NAV], 
       y=nu.rs3[AssetMetrixType ==  AMT, ACR.Q], ylim=c(-20,20), xlim=c(0,1.5e+07),
       pch=".", xlab="Net Asset Value",ylab="Index Quagging Residuals", cex=3, col="darkgreen")
  abline(h=0,col="black", lwd=2) ; abline(h=c(-1,1),col="red")
  # abline(v=c(4.5e+06,5.5e+06),col="grey",lwd=2)
  abline(v=quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2), col="blue",lwd=2,lty=3) 
  # abline(v=1e+06,col="blue",lty=3, lwd=2)
  legend(x=6e+06,y=19,legend=AMT, box.lty = 0, cex=1.5)
}
mtext("Net Asset Value", side = 1, line= 1, outer = T, cex=1.5) ;     mtext("Index Quagging Residuals", side = 2, line= 0, outer = T,cex=1.5)

# Time evolution of residuals (multi)
AMT <- "BO"
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(x= Dates,
     y= NRet, 
     pch=".", xlab="Date",ylab="Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2)
resimax <- aggregate(nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),ACR.Q] ~ nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),Date], FUN=max)
resimin <- aggregate(nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),ACR.Q] ~ nu.rs3[AssetMetrixType ==  AMT & NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.2),Date], FUN=min)
lines(resimin[,1], resimin[,2], col="red")
lines(resimax[,1], resimax[,2], col="blue")
#legend(x=as.Date("1994-03-01"), y=-0.25, legend=c("max(Residuals)","Residuals","min(Residuals)"), box.lty = 0, lty=c(1,0,1),col=c("blue","darkgreen","red"), pch=c(NULL,".",NULL), pt.cex = 6)
rm(resimin,resimax)

## plot residuals vs. Age.week 
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(x=nu.rs3[AssetMetrixType ==  AMT, Age.week], 
     y=nu.rs3[AssetMetrixType ==  AMT, ACR.Q], ylim=c(-10,10), 
     pch=".", xlab="Age.week",ylab="Index Quagging Residuals", cex=3, col="darkgreen")
abline(h=0,col="black", lwd=2) ; abline(h=c(-1,1),col="red")
# abline(v=c(4.5e+06,5.5e+06),col="grey",lwd=2)
abline(v=quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],c(0.1,0.2,0.3,0.4,0.5)), col="grey",lwd=2) ; abline(v=2e+06,col="blue",lty=3)

rm(AMT, Dates, NRet, AA, bqp)
## 4.5) Residuals Modeling  ----------

par(mfrow=c(1,1), mar=c(4,4,1,1))
hist(ResiList4$VC$IQ$ACR.Q)
rug(ResiList4$VC$IQ$ACR.Q)

# Example Mixture Distribution Modeling with VC index quagging residuals
x <- as.numeric(ResiList2$VC$IQ$ACR.Q) ; x <- x[!is.na(x)]

# Parametric Normal Mixture
MixMo <- normalmixEM(x,k=3, verb=FALSE) ; 
summary(MixMo)
plot(MixMo, which=2)
plot(MixMo, which=1)
MixMo$lambda ; MixMo$mu ; MixMo$sigma
MixMo$lambda %*% MixMo$mu

# MixMoSim (2-component)
MixMoSim <- ifelse(runif(length(x)) < MixMo$lambda[1],
                   rnorm(length(x), mean= MixMo$mu[1], sd= MixMo$sigma[1]),
                   rnorm(length(x), mean= MixMo$mu[2], sd= MixMo$sigma[2]))

# MixMoSim (3-component)
lam <- runif(length(x))
MixMoSim3 <- ifelse(lam < MixMo$lambda[1],
                    rnorm(length(x), mean= MixMo$mu[1], sd= MixMo$sigma[1]),
                    ifelse(lam < MixMo$lambda[1] + MixMo$lambda[2],
                           rnorm(length(x), mean= MixMo$mu[2], sd= MixMo$sigma[2]),
                           rnorm(length(x), mean= MixMo$mu[3], sd= MixMo$sigma[3])))
mean(MixMoSim3)

# Visu: Normal Mixture vs. empirical
par(mfrow=c(2,1), mar=c(2,2,1,1), oma=c(0,0,0,0))
layout(mat= matrix(c(1,1,2,2),2,2,byrow = T), heights = c(3,1), widths = c(1,1))
hist(x, breaks=50, freq=F, xlim = c(-4,8), ylim=c(0,1.75), main="")
lines(density(x), col="darkgreen", lty=1, lwd=2)
lines(density(MixMoSim), col="red", lty=3, lwd=3)
lines(density(MixMoSim3), col="orange", lty=3, lwd=3) 
legend("topright", cex=1.3,legend=c("2-comp. normal kernel","3-comp. normal kernel", "empirical kernel"),box.lty=0, lty = c(3,3,1), col=c("red", "orange","darkgreen"), lwd=c(3,3,2))

plot(x= MixMoSim, y=rep(3, length(x)), main="", col="red", pch="|", xlab="", ylab="", ylim=c(0.5,3.5), xlim=c(-4,8),yaxt='n')
points(x= MixMoSim3, y=rep(2, length(x)), main="", col="orange", pch="|", xlab="")
points(x= x, y=rep(1, length(x)), col="darkgreen", pch="|", xlab="", ylab="")

rm(x, MixMoSim3, MixMoSim, MixMo, lam)

# Non-parametrix Mixture
#MixMo <- npEM(x, 2, verb=FALSE)
#MixMo$lambdahat ; summary(MixMo) ; par(mfrow=c(1,1), mar=c(3,3,1,1)) ; plot(MixMo)

# SGT Residuals Modeling (optional)
'
start = c(b0 = QR.list$lmrob$BO["mean(Intercept)",1], b1 = QR.list$lmrob$BO["meanHYS.lm",1], b2 = QR.list$lmrob$BO["meanMkt.RF.GO.y",1],
g0 = 1, g1 = 1, g2 = 1, 
d0 = 1, d1 = 1, d2 = 1, 
p = 2, q = 10)

start1 = c(b0 = QR.list$lmrob$BO["mean(Intercept)",1], b1 = QR.list$lmrob$BO["meanHYS.lm",1], b2 = QR.list$lmrob$BO["meanMkt.RF.GO.y",1],
sigma = 0.5, lambda= 0,  p = 2, q = 10)

start2 = c(b0 = QR.list$lmrob$BO["mean(Intercept)",1], b1 = QR.list$lmrob$BO["meanHYS.lm",1], b2 = QR.list$lmrob$BO["meanMkt.RF.GO.y",1],
mu=0, sigma = 0.5, lambda= 0,  p = 2, q = 10)

start3 = c(mu=0, sigma = 0.5, lambda= 0,  p = 2, q = 1000)

X.f = X ~ XNavR - (b0 + b1*HYS.lm + b2*Mkt.RF.GO.y) # Calculate Residuals
mu.f = mu ~ 0
# sigma.f = sigma ~ exp(g0 + g1*HYS.lm + g2*Mkt.RF.GO.y)
# lambda.f = lambda ~ (exp(d0 + d1*HYS.lm + d2*Mkt.RF.GO.y)-1)/(exp(d0 + d1*HYS.lm + d2*Mkt.RF.GO.y)+1)
bw <- c(1.0e+06,20.0e+06)

result = sgt.mle(X.f = X.f, mu.f = mu.f,
data = nu.rs2[nu.rs2$AssetMetrixType == AMT & nu.rs2$NAV %between%  bw,], 
start = start1, mean.cent = TRUE, var.adj = TRUE, method = "nlm")

result = sgt.mle(X.f = X.f,
data = nu.rs2[nu.rs2$AssetMetrixType == AMT & nu.rs2$NAV > 5.5e+06,], 
start = start2, mean.cent = TRUE, var.adj = TRUE, method = "nlm")

result = sgt.mle(X.f = ~ ACR.Q,
data = nu.rs3[nu.rs3$AssetMetrixType == AMT & nu.rs3$NAV > quantile(nu.rs3[AssetMetrixType ==  AMT, NAV],0.5),], 
start = start3, mean.cent = TRUE, var.adj = TRUE, method = "nlm")

print(summary(result))

sim.sgt <- rsgt(100000,mu=result$estimate["mu"],sigma = result$estimate["sigma"],lambda = result$estimate["lambda"],
p = result$estimate["p"], q= result$estimate["q"], var.adj = T, mean.cent = T)

plot(density(sim.sgt),main="") ; rug(sim.sgt, col="red") ; summary(sim.sgt) ; sd(sim.sgt)
quantile(sim.sgt, c(0.005,0.01,0.02,0.99)) ; quantile(NRet, c(0.005,0.01,0.02,0.99), na.rm = T)


rm(start3,start2,start1,start,X.f,mu.f,sgt.sim,result)
'
## 4.6) Visulalize Quagging Factor Models  --------
amti <- list()
for(AMT in c("BO","VC","FOF","DD","RE")){
  AMTindex <- read.csv(file=paste("AMTindex",AMT,".csv",sep=""))
  AMTindex$Date <- as.Date(AMTindex$Date)
  AMTindex <- merge(x=AMTindex, y= Public[,c("Date","Mkt.RF.GO.y","ExMSCI","ExNAS","ExSP500", "RE.US.com.Return", "CFSI", "VTM.Return",
                                             "HYspread","HYS.lm","Liq.Trade","Yield1.World")], by="Date",all.x = T)
  for(i in 1:nrow(AMTindex)){
    AMTindex$IQlmrob[i] <- BQ(AMTindex$AMT[i], NULL, AMTindex$HYS.lm[i], AMTindex$Mkt.RF.GO.y[i], AMTindex$ExNAS[i], AMTindex$RE.US.com.Return[i] - 1- AMTindex$Yield1.World[i],
                              AMTindex$Yield1.World[i], AMTindex$Liq.Trade[i],"lmrob")
    AMTindex$IQlm[i] <- BQ(AMTindex$AMT[i], NULL, AMTindex$HYS.lm[i], AMTindex$Mkt.RF.GO.y[i], AMTindex$ExNAS[i], AMTindex$RE.US.com.Return[i] - 1- AMTindex$Yield1.World[i],
                           AMTindex$Yield1.World[i], AMTindex$Liq.Trade[i],"lm")
  }
  amti[[AMT]] <- AMTindex
}
rm(AMTindex)

par(mfrow = c(3, 1), mar = c(3, 3, 1, 1), oma=c(2,2,0,0), lwd=1)
# Historical Returns
plot(x= amti$BO$Date,y= amti$BO$NAVret.y - 1 - amti$BO$Yield1.World, xlim=c(as.Date("1990-01-01"),as.Date("2016-01-01")),
     type="s", col="black", ylim=c(-1,3), xlab="Date", ylab="ExNAVRet (hist)", main="")
abline(h=c(-0.1,0.1,-0.2,0.2,-0.3,0.3,-0.4,0.4), col="grey", lty=3) ; abline(h=0, col="grey", lwd=2)
lines(x= amti$VC$Date, y= amti$VC$NAVret.y - 1 - amti$BO$Yield1.World, lwd=1, type = "s", col=2)
lines(x= amti$FOF$Date, y= amti$FOF$NAVret.y - 1 - amti$BO$Yield1.World, lwd=1, type = "s", col=3)
lines(x= amti$RE$Date, y= amti$RE$NAVret.y - 1 - amti$BO$Yield1.World, lwd=1, type = "s", col=4)
lines(x= amti$DD$Date, y= amti$DD$NAVret.y - 1 - amti$BO$Yield1.World, lwd=1, type = "s", col=5)
legend(x= as.Date("2012-01-01"), y=3, y.intersp=1.3, bty="o", box.lty=0, cex=1, legend=c("BO","VC","FOF","RE","DD"), col=c(1,2,3,4,5),lty=c(1,1,1,1,1), pch=c(NA,NA,NA,NA,NA), lwd=c(2,2,2,2,2)) 
# MM-estimates
plot(x= amti$BO$Date,y= amti$BO$IQlmrob, xlim=c(as.Date("1990-01-01"),as.Date("2016-01-01")),
     type="s", col="black", ylim=c(-1,1), xlab="Date", ylab="ExNAVRet (MM)", main="")
abline(h=c(-0.1,0.1,-0.2,0.2,-0.3,0.3,-0.4,0.4), col="grey", lty=3) ; abline(h=0, col="grey", lwd=2)
#lines(x= amti$BO$Date, y= amti$BO$IQlmrob, type = "s", lty=1, col=1)
lines(x= amti$VC$Date, y= amti$VC$IQlmrob, type = "s", lty=1, col=2)
lines(x= amti$FOF$Date, y= amti$FOF$IQlmrob, type = "s", lty=1, col=3)
lines(x= amti$RE$Date, y= amti$RE$IQlmrob, type = "s", lty=1, col=4)
lines(x= amti$DD$Date, y= amti$DD$IQlmrob, type = "s", lty=1, col=5)
# OLS-estimates
plot(x= amti$BO$Date,y= amti$BO$IQlm, xlim=c(as.Date("1990-01-01"),as.Date("2016-01-01")),
     type="s", col="black", ylim=c(-1,1), xlab="Date", ylab="ExNAVRet (OLS)", main="")
abline(h=c(-0.1,0.1,-0.2,0.2,-0.3,0.3,-0.4,0.4), col="grey", lty=3) ; abline(h=0, col="grey", lwd=2)
#lines(x= amti$BO$Date, y= amti$BO$IQlm, type = "s", lty=1, col=1)
lines(x= amti$VC$Date, y= amti$VC$IQlm, type = "s", lty=1, col=2)
lines(x= amti$FOF$Date, y= amti$FOF$IQlm, type = "s", lty=1, col=3)
lines(x= amti$RE$Date, y= amti$RE$IQlm, type = "s", lty=1, col=4)
lines(x= amti$DD$Date, y= amti$DD$IQlm, type = "s", lty=1, col=5)
## Outer Text
mtext("Date",side=1, line=1, outer = T, cex=1.5) ; mtext("Excess NAV Return",side=2, line=0, outer = T, cex=1.5)

rm(AMT, amti, i)
#################################################
## 5.) Fund-2-Portfolio Aggregation (Monte Carlo)
#################################################
##    a) X-Factor Generation Models  ------

# CoVarianz Matrix for Market Data
Pub.sub <- Public[, c("Mkt.RF.GO.y","ExNAS" ,"ExRE", "ExVTM", "logHYs", "Liq.Trade", "Yield1.World")]
summary(Pub.sub)

pairs(Pub.sub, lower.panel = panel.smooth, gap=0)
sd <- apply(Pub.sub, 2, function(x)sd(x,na.rm=T))
cor <- cor(Pub.sub, use = "pairwise.complete.obs",method = "kendall")
# covma <- cov(Pub.sub, use = "pairwise.complete.obs")
covma <- diag(sd) %*% cor %*% diag(sd)
colnames(covma) <- rownames(covma) <- colnames(cov(Pub.sub, use = "pairwise.complete.obs"))
covma

# t-Copula for Market Data Generation
tCo <- tCopula(dim=ncol(Pub.sub))
tCo.fit <- fitCopula(tCo, pobs(Pub.sub[28:100,]), method = "mpl")
TC1 <- mvdc(copula= tCopula(param = coef(tCo.fit)["rho.1"], df= coef(tCo.fit)["df"], dim=ncol(Pub.sub)),
            margins = c(rep.int("norm",ncol(Pub.sub))),
            paramMargins = list(list(mean= 0.1, sd= sd["Mkt.RF.GO.y"]),
                                list(mean= 0.15, sd= sd["ExNAS"]),
                                list(mean= 0.05, sd= sd["ExRE"]),
                                list(mean= 1.7, sd= sd["logHYs"]),
                                list(mean= 0, sd= sd["Liq.Trade"]),
                                list(mean= 0.36, sd= sd["Liq.Trade"]),
                                list(mean= 0.03624, sd= sd["Yield1.World"])
            ))

# Vine Copula Model
PSC <- as.copuladata(pobs(Pub.sub[28:100,]))
RVSS <- RVineStructureSelect(PSC)
# RVineCopSelect(PSC, Matrix = RVSS$Matrix)
# RVineSeqEst(PSC, RVSS)

# Uniforms // R-vine copula
pairs(RVineSim(1000, RVSS), lower.panel = panel.smooth, gap=0, pch=".")

# Marginals
mafa <- list()
mafa$xEGO <- fitdistr(Pub.sub[28:100, "Mkt.RF.GO.y"], densfun = "t")
hist(Pub.sub[28:100, "Mkt.RF.GO.y"], freq = F) ; lines(density(0.06554464 + 0.18526301 * rt(100000, df=19.38798039)))

mafa$xNAS <- fitdistr(Pub.sub[28:100, "ExNAS"], densfun = "t")
hist(Pub.sub[28:100, "ExNAS"], freq = F) ; lines(density(0.09827290 + 0.24948839 * rt(100000, df=6.25487783)))

x <- Pub.sub[28:100, "ExRE"]
sta = c(mu= mean(x),
        sigma= sd(x),
        lambda=0.1,
        p=2,
        q=2)
sta

result <- sgt.mle(X.f = ~ x, start=sta)
mafa$xRE <- result
SGT.sim <- rsgt(10000, mu = result$estimate["mu"], sigma = result$estimate["sigma"], lambda = result$estimate["lambda"], p = result$estimate["p"],q = result$estimate["q"])
hist(Pub.sub[28:100, "ExRE"],freq = F) ; lines(density(SGT.sim))

fitdistr(Pub.sub[28:100, "ExRE"]+1, densfun = "gamma")
hist(Pub.sub[28:100, "ExRE"]+1, xlim=c(0.5,1.5),freq = F) ; lines(density(rgamma(10000,shape = 76.45425, rate = 73.82486 )))

mafa$logHYs <- fitdistr(Pub.sub[28:100, "logHYs"], densfun = "lognormal")
hist(Pub.sub[28:100, "logHYs"], freq = F) ; lines(density(rlnorm(100000, 0.48016506, 0.25971003)))
fitdistr(Pub.sub[28:100, "logHYs"], densfun = "normal")

mafa$Liq <- fitdistr(Pub.sub[28:100, "Liq.Trade"], densfun = "normal")
hist(Pub.sub[28:100, "Liq.Trade"], freq = F) ; lines(density(rnorm(100000, 0.01379, 0.0469)))

fitdistr(Pub.sub[28:100, "Yield1.World"], densfun = "lognormal")
hist(Pub.sub[28:100, "Yield1.World"], freq = F) ; lines(density(rlnorm(100000, -3.79887812, 0.81274355)))

rm(sta, SGT.sim, tCo, tCo.fit,x, cor,result)
##    b) Simulation Function   ----------
fusi <- function(n, m, data, SDinf=1, Pub= c("Mnorm","tCop","Vine1","Vine2","Empirical"), 
                 Reg=c("SF","Q1", "Q2"),Resi=c("80","top40","bottom40"), hys="known"){
  Simlist <- list()
  for(k in seq_len(n)){
    ## Public Input  
    if(Pub == "Mnorm") {
      mvn <- mvrnorm(1, mu= colMeans(Pub.sub, na.rm=T), Sigma= covma)  # empirical
      L <- mvn["Liq.Trade"]
      H <- exp(mvn["logHYs"])/100
      W <- mvn["Mkt.RF.GO.y"]
      N <- mvn["ExNAS"]
      R <- mvn["ExRE"]
    }
    if(Pub == "tCop"){
      rT1 <- rMvdc(1, TC1)
      colnames(rT1) <- colnames(cov(Pub.sub, use = "pairwise.complete.obs"))
      L <- rT1[1,"Liq.Trade"]
      H <- exp(rT1[1,"logHYs"])/100
      W <- rT1[1,"Mkt.RF.GO.y"]
      N <- rT1[1,"ExNAS"]
      R <- rT1[1,"ExRE"]
    }
    if(Pub == "Empirical"){
      r <- sample(28:100,1)                  # rows 28 - 100 are filled completely
      p <- rep(0,8)                   
      L <- Public$Liq.Trade[r] + p[1]
      H <- Public$HYspread[r]/100 + abs(p[2])
      W <- Public$Mkt.RF.GO.y[r] + p[3]
      N <- Public$NASDAQ.Return[r] - 1 - Public$Yield1.US[r]/100 + p[5]
      R <- Public$RE.US.com.Return[r] - 1 - Public$Yield1.US[r]/100 + p[6]
    }
    if(Pub == "Vine1"){
      RVS <- RVineSim(1, RVSS)
      # stochastic factors    
      W <- qnorm(RVS["Mkt.RF.GO.y"], 0.1, sd["Mkt.RF.GO.y"])
      N <- qnorm(RVS["ExNAS"], 0.15, sd["ExNAS"])
      R <- qnorm(RVS["ExRE"], 0.05, sd["ExRE"])
      L <- qnorm(RVS["Liq.Trade"], 0, sd["Liq.Trade"])
      # deterministic factors    
      H <- exp(qnorm(RVS["logHYs"], 1.7, sd["logHYs"]))/100 # 0.064
    }
    if(Pub == "Vine2"){
      RVS <- RVineSim(1, RVSS)
      # stochastic factors    
      W <- mafa$xEGO$estimate["m"] + mafa$xEGO$estimate["s"] * qt(RVS["Mkt.RF.GO.y"], df= mafa$xEGO$estimate["df"])
      N <- mafa$xNAS$estimate["m"] + mafa$xNAS$estimate["s"] * qt(RVS["ExNAS"], df= mafa$xNAS$estimate["df"])
      R <- qsgt(RVS["ExRE"], mu= mafa$xRE$estimate["mu"], sigma= mafa$xRE$estimate["sigma"],
                lambda = mafa$xRE$estimate["lambda"], p = mafa$xRE$estimate["p"], 
                q= mafa$xRE$estimate["q"])
      L <- qnorm(RVS["Liq.Trade"], mean= mafa$Liq$estimate["mean"], sd= mafa$Liq$estimate["sd"])
      # deterministic factors    
      #H <- exp(qlnorm(RVS["logHYs"],meanlog = mafa$logHYs$estimate["meanlog"], sdlog= mafa$logHYs$estimate["sdlog"]))/100 # 0.064
      H <- exp(qnorm(RVS["logHYs"], 1.67098830, 0.42921988))/100
    }
    
    if(hys == "known"){
      H <- 0.064 # non-stochastic high-yield spreads  
    }
    
    ## Estimated Mue & Sigma
    ms <- data.frame(data)
    ms$Iteration <- k
    Type <-as.character(ms$Type)
    Focus <- ms$Focus
    ms$Liq <- L ; ms$HYs <- H ; ms$ExWo <- W ; ms$ExNas <- N ; ms$ExRE <- R
    if(Resi == "80") ResiList <- ResiList2
    if(Resi == "bottom40") ResiList <- ResiList3
    if(Resi == "top40") ResiList <- ResiList4
    Q <- base::sample(c(1,2,3,4),1)
    
    for(i in 1:length(Type)){
      
      x <- BQ(AMT = Type[i], Fund.Focus = Focus[i], HYs = H, XGO = W, XNAS = N, XREI = R, Y1 = NULL,Liq = L,IQM = "lmrob")
      # MM Single Fund Regressions
      if(Reg == "SF"){
        ms$Mue[i] <- x$UFS
        RES <- unlist(ResiList[[Type[i]]]["SF"])
        ms$BiasCor[i] <- mean(RES, na.rm = T)
        for(j in 1:m){
          ms[i,paste("Return",j,sep="")] <- base::sample(RES[!is.na(RES)], size = 1) + ms$Mue[i]
        }
      }
      # MM 4 Quarters Index Sampling
      if(Reg == "Q1"){
        if(Type[i] %in% c("BO","VC","FOF","DD","RE")){
          ms$Mue[i] <- as.numeric( x[paste("ACM",Q,sep="")] )
          RES <- as.numeric( unlist( ResiList[[ Type[i] ]][["IQ"]][paste("ACR",Q,sep="")] ) ) 
          RES <- as.character( RES[!is.na(RES)] )
          ms$BiasCor[i] <- mean(as.numeric(RES), na.rm = T)
          for(j in 1:m){
            ms[i,paste("Return",j,sep="")] <- ms$Mue[i] + as.numeric( sample(x= RES, size = 1) )
          }
        }
        if(Type[i] %in% c("MEZZ","Infrastructure","NatRes")){
          ms$Mue[i] <- x$UFS
          RES <- unlist(ResiList[[Type[i]]]["SF"])
          RES <- as.character( RES[!is.na(RES)] )
          ms$BiasCor[i] <- mean(as.numeric(RES), na.rm = T)
          for(j in 1:m){
            ms[i,paste("Return",j,sep="")] <-  ms$Mue[i] + as.numeric(base::sample(RES, size = 1))
          }
        }
        
      }
      # MM Index Quagging
      if(Reg == "Q2"){
        if(Type[i] %in% c("BO","VC","FOF","DD","RE")){
          ms$Mue[i] <- as.numeric( x[paste("ACM",Q,sep="")] )
          RES <- as.numeric( unlist( ResiList[[ Type[i] ]][["IQ"]][paste("ACR",Q,sep="")] ) ) 
          #              RES <- as.numeric( unlist( ResiList[[ "BO" ]][["IQ"]][paste("ACR",Q,sep="")] ) ) 
          RES <- as.character( RES[!is.na(RES)] )
          ms$BiasCor[i] <- mean(as.numeric(RES), na.rm = T)
          for(j in 1:m){
            ms[i,paste("Return",j,sep="")] <- ms$Mue[i] + as.numeric( sample(x= RES, size = 1) )
          }
        }
        if(Type[i] %in% c("MEZZ","Infrastructure","NatRes")){
          ms$Mue[i] <- x$UFS
          RES <- as.numeric( unlist(ResiList[[Type[i]]]["SF"]) )
          ms$BiasCor[i] <- mean(RES, na.rm = T)
          for(j in 1:m){
            ms[i,paste("Return",j,sep="")] <-  ms$Mue[i] # base::sample(RES, size = 1)
          }
        }
        
      }          
      # Old MaxFit via Mixture Distribution
      if(Reg == "MaxFit"){
        if(runif(1) > 1/3) {ms$Mue[i] <- x$Mue.RO} else {ms$Mue[i] <- x$Mue.LM} # fit mean
        # if(ms$Mue[i] < -0.06 | ms$Mue[i] > 0.2) SDinf <- 2  # approx. 10% & 90% quantile of Mue
        if(ms$Mue[i] < -0.1) SDinf <- 1.5  # approx. 5% quantile of Mue
        if(ms$Mue[i] > 0.25) SDinf <- 1.5  # approx. 95% quantile of Mue
        #quantile(fs$Mue,pbs)
        # if(runif(1) > 0.95) SDinf <- 3
        # if(H > 10) SDinf <- 2
        if(VariVar < 1/2) {ms$Sigma[i] <- x$Sigma.RO} else {ms$Sigma[i] <- x$Sigma.LM * SDinf} # leptokurtosis
      }
      
      # weights
      # ms$weight[i] <- ms$Com[i] / sum(ms$Com)
      ms$weight[i] <- ms$NAV[i] / sum(ms$NAV)
    } 
    
    Simlist[[k]] <- ms
  }
  of <- rbindlist(l=Simlist)
  of
}
##    c) Monte Carlo Simulations ------
a <- 20000  # How many macro conditions
b <- 100    # How many returns per macro scenario
#pepf <- rbind(pepf,pepf,pepf,pepf,pepf)
# Buy Out pepfs different size
fsize <- 25
pepfBO <- data.frame(Type= rep("BO",fsize),
                     Focus= sample(c("US","EU","ROW"),fsize, replace = T),
                     NAV= 10* runif(fsize),
                     Com= rep(10,fsize))

MonteCarloInput <- pepf

system.time(
  fs <- fusi(n=a, m=b, data= MonteCarloInput, SDinf = 1, Pub = "Vine2", Reg= "Q1", Resi = "80", hys="known") # create macro conditions
)
##    d) Portfolio Return Calculation -------
pere1 <- data.frame(matrix(NA, nrow = a, ncol = b))
fs1 <- as.data.frame(fs)
system.time(
  for(k in 1:b){
    for(i in 1:a){
      pere1[i,k] <- as.numeric(fs1[fs1$Iteration == i, "weight"]) %*% as.numeric(fs1[fs1$Iteration == i, paste("Return",k,sep="")])
    }
  }
)
pereret1 <- as.numeric(unlist(pere1))
pbs <- c(0,0.005,0.01,0.02,0.05,0.1,0.5,0.9,0.95,0.99,0.995)
summary(pereret1)
quantile(pereret1, probs=pbs)
##    e) Visualization (unconditional, hist. vs. MC comparison) --------
Vizu <- "not" # "compare"
ploting <- "no"
if(ploting == "yes"){
  png(filename = "MCvsHist2060stochastic2.png", width= 480*1.5, height = 480, pointsize = 12, res=100)
}
if(Vizu == "compare") {
  par(mfrow = c(2,1), mar = c(2, 4, 1, 1), oma=c(2,0,0,0))
} else {
  par(mfrow = c(1,1), mar = c(2, 4, 1, 1), oma=c(2,0,0,0))
  layout(mat= matrix(c(1,1,2,2),2,2,byrow = T), heights = c(3,1), widths = c(1,1))
}
## Overlapping Historgrams
#hi1 <- hist(pereret1, breaks=100, freq=F)
pereret2 <- pereret1
if(length(pereret1) > length(NRPCS10000)) pereret2 <- base::sample(pereret1, size=length(NRPCS10000), replace = FALSE)
if(length(pereret1) < length(NRPCS10000)) pereret2 <- base::sample(NRPCS10000, size=length(pereret1), replace = FALSE)

if(Vizu == "compare"){
  hi1 <- hist(pereret2, breaks=100, freq=F)
  hi2 <- hist(NRPCS10000, breaks=100, freq=F)
  plot( hi1, col=rgb(0,0,1,2/4), ylim=c(0,1500), xlim=c(-1,2), main="")  # Monte Carlo
  plot( hi2, col=rgb(1,0,0,2/4), add=T)  # Historical
  rug(pereret2, col=rgb(0,0,1,2/4))
  abline(v=quantile(pereret1, 0.005), col="blue", lty=3, lwd=2)
  abline(v=quantile(NRPCS10000, 0.005), col="red", lty=2, lwd=2)
  rm(hi1, hi2)
  legend(x=0.6, y= 1300, legend = 
           c(paste("MC  VaR 99.5%: ", round(quantile(100*pereret1, 0.005),2), "%", sep=""), 
             paste("Hist. VaR 99.5%: ", round(quantile(100*NRPCS10000, 0.005),2),"%",sep="")), 
         cex=0.8, box.lty = 0)
}
## Density & Normal Approx. Plots
plot(density(pereret1), ylim=c(0,4), col="blue",xlim=c(-1,2), lwd=2, main="")  # first histogram
lines(density(NRPCS10000, adjust = 2), col="red",lwd=2)  # second
#lines(density(c(NRPCS10000, pereret2), adjust = 2), col="black", lty=2, lwd=2)  # combination
lines(density(fs$Mue + fs$BiasCor, adjust = 2), col="grey", lty=1, lwd=2)    # before errors / just beta

rug(NRPCS10000, col="red")
abline(v=quantile(pereret1, 0.005), col="blue", lty= 3, lwd=2)
abline(v=quantile(NRPCS10000, 0.005), col="red", lty=2, lwd=2)
#abline(v=quantile(c(NRPCS10000, pereret2), 0.005), col="black", lty=4, lwd=2)
abline(v=quantile(fs$Mue + fs$BiasCor, 0.005), col="grey", lty=4, lwd=2)
abline(v=mean(pereret1), col="blue") ; abline(v=mean(NRPCS10000), col="red") ; abline(v=mean(fs$Mue + fs$BiasCor),col="grey", lty=3)
legend("topright", legend=c("Monte Carlo","MC Mean","Historical"), lty=c(1,1,1), 
       box.lty = 0, lwd=c(2,2,2), cex = 1.3, col=c("blue","grey","red"))
mtext("Excess NAV Return", side = 1, line= 1, outer = T, cex=1)

if(!(Vizu == "compare")){
  plot(x= pereret2, y=rep(2, length(pereret2)), main="", col="blue", pch="|", xlab="", ylab="", ylim=c(0.5,2.5), xlim=c(-1,2),yaxt='n')
  points(x= NRPCS10000, y=rep(1, length(NRPCS10000)), main="", col="red", pch="|", xlab="")
  abline(v=quantile(pereret1, 0.005), col="blue", lty= 3, lwd=2)
  abline(v=quantile(NRPCS10000, 0.005), col="red", lty=2, lwd=2)
  abline(v=mean(pereret1), col="blue") ; abline(v=mean(NRPCS10000), col="red") ; abline(v=mean(fs$Mue + fs$BiasCor),col="grey", lty=3)
}
if(ploting == "yes"){
  dev.off()
}


## Summary Statistics & Benchmark
pbs <- c(0,0.005,0.01,0.02,0.05,0.1,0.5,0.9,0.95,0.99,0.995)
summary(pereret1)
quantile(pereret1, probs=pbs)
mean(pereret1[pereret1< quantile(pereret1, probs=0.005)]) # CVaR

summary(NRPCS10000) # hist. simulation benchmark
quantile(NRPCS10000, probs = pbs, na.rm=T)
mean(NRPCS10000[NRPCS10000< quantile(NRPCS10000, probs=0.005)]) # CVaR
##    f) Visualization (conditional, bottom/top 40, 80 NAV-errors)  ----------
'
p2060 # 10 BO
p40 # 1 BO
p80 # 25 BO

#colo <- c("darkblue","darkviolet","cyan")
colo <- c("gold1","orangered","blueviolet")

par(mfrow = c(1,1), mar = c(2, 4, 1, 1), oma=c(2,0,0,0))
layout(mat= matrix(c(1,1,2,2),2,2,byrow = T), heights = c(3,1), widths = c(1,1))

plot(density(p2060), ylim=c(0,3), col=colo[1],xlim=c(-1,2), lwd=2, main="")
lines(density(p40, adjust = 2), col=colo[2],lwd=2)  
lines(density(p80 , adjust = 2), col=colo[3], lwd=2) 

# 0.5% Quantile
abline(v=quantile(p2060, 0.005), col=colo[1], lty= 3, lwd=2)
abline(v=quantile(p40, 0.005), col=colo[2], lty= 3, lwd=2)
abline(v=quantile(p80, 0.005), col=colo[3], lty= 3, lwd=2)

# Mean
abline(v=mean(p2060), col=colo[1], lty= 1, lwd=1)
abline(v=mean(p40), col=colo[2], lty= 1, lwd=1)
abline(v=mean(p80), col=colo[3], lty= 1, lwd=1)

# Text
legend(x=0.8, y=2.9, legend=c("1  BO fund","10 BO funds","25 BO funds"), lty=c(1,1,1), 
       box.lty = 0, lwd=c(3,3,3), cex = 1.3, col=c(colo[2],colo[1],colo[3]))
mtext("Excess NAV Return", side = 1, line= 1, outer = T, cex=1)

#Rugs
plot(x= p40, y=rep(3, length(p40)), main="", col=colo[2], pch="|", xlab="", ylab="", ylim=c(0.5,3.5), xlim=c(-1,2),yaxt="n")
points(x= p2060, y=rep(2, length(p2060)), main="", col=colo[1], pch="|", xlab="")
points(x= p80, y=rep(1, length(p80)), main="", col=colo[3], pch="|", xlab="")

# 0.5% Quantile
abline(v=quantile(p2060, 0.005), col=colo[1], lty= 3, lwd=2)
abline(v=quantile(p40, 0.005), col=colo[2], lty= 3, lwd=2)
abline(v=quantile(p80, 0.005), col=colo[3], lty= 3, lwd=2)


## Summary Statistics & Benchmark
pbs <- c(0,0.005,0.01,0.02,0.05,0.1,0.5,0.9,0.95,0.99,0.995)
summary(p80)
quantile(p80, probs=pbs)
mean(p80[p80< quantile(p80, probs=0.005)]) # CVaR
'
##    g) Bootstrap Confis  ----------

c <- 50 ; aa <- c(10, 100)
BooSt <- data.frame(Iteration=1:c)

system.time(
  for(q in 1:length(aa)){
    booli <- list() 
      for(l in 1:c){
      a <- aa[q]
      b <- 10
      fs <- fusi(n=a, m=b, data= pepf, SDinf = 1, Pub = "Vine2", Reg= "Q1", Resi = "80", hys="known")
      
      ## PF Return Calc
      pere1 <- data.frame(matrix(NA, nrow = a, ncol = b))
      fs1 <- as.data.frame(fs)
      system.time(
        for(k in 1:b){
          for(i in 1:a){
            pere1[i,k] <- as.numeric(fs1[fs1$Iteration == i, "weight"]) %*% as.numeric(fs1[fs1$Iteration == i, paste("Return",k,sep="")])
          }
        }
      )
      pereret1 <- as.numeric(unlist(pere1))
      booli[[l]] <- quantile(pereret1, probs=0.005)
    }
    BooSt[, paste("VaR",aa[q],sep="")] <- as.numeric(unlist(booli))
})

summary(BooSt)

par(mfrow = c(1,1), mar = c(4, 4, 1, 1), oma=c(0,0,0,0))
plot(ecdf(BooSt[,2]), col="cornflowerblue", main="",
     xlab="0.5% Quantiles of Excess NAV Returns", ylab= "Empirical CDF")
lines(ecdf(BooSt[,3]), col="lightcoral")
abline(v=mean(BooSt[,3]),col="lightcoral",lwd=2) ; abline(v=mean(BooSt[,2]),col="cornflowerblue",lwd=2)
abline(v=median(BooSt[,3]),col="lightcoral", lty=2,lwd=2) ; abline(v=median(BooSt[,2]),col="cornflowerblue",lty=3,lwd=2)
legend("topleft", cex=0.8, inset=0.05, pch=c(1,1), lwd=c(3,3), box.lty = 0,
       legend = c(paste(aa[1]*b,"simulations"), paste(aa[2]*b,"simulations")), col=c("cornflowerblue","lightcoral"))
##    h) VaR Evolution-over-time/iterations ---------
a ; b ; a * b
# saveRDS(pereret1, "pereret1.rds")
tmt80 <- readRDS("pereret2000.rds")
tmf <- data.frame(Iteration= seq(1000,a*b,b*10), VaR= NA, VaR2=NA)

system.time(
for(i in 1:nrow(tmf)){
  tmf$VaR[i] <- quantile(tmt80[1:tmf[i,"Iteration"]], probs=0.005)
})

system.time(
  for(i in nrow(tmf):2){
    tmf$VaR2[i] <- quantile(tmt80[tmf[i-1,"Iteration"] : (a*b)], probs=0.005)
  })

last(tmf[,"VaR"])

# Visu
par(mfrow = c(1,1), mar = c(4, 4, 1, 1), oma=c(0,0,0,0), cex=1.4)
plot(tmf$Iteration, tmf$VaR, type="b", pch="-", ylim=c(-0.34,-0.295), col="red",
     xlab="Number of Simulated Returns", ylab="0.5% Quantile")
lines(tmf$Iteration, tmf$VaR2, type="b", pch="-", col="green")

lines(tmf$Iteration, tmf$VaR, col="red")
lines(tmf$Iteration, tmf$VaR2, col="green")

abline(h=last(tmf[,"VaR"]), col="black")
abline(v=c(0,2e+06)) ; abline(v=c(1e+05,19e+05), col="blue", lty=2)

legend("top", inset=0.01, legend = c("forward evolution", "backward evolution"), col=c("red","green"),
       bty="n", pch=c("-","-"), lwd=3)

# mtext("Number of Simulated Returns", side = 1, line= 1, outer = T, cex=1)
# mtext("0.5% quantile", side = 2, line= 1, outer = T, cex=1)
########## >> THE END << ##################
## 6.) Additional Illustrations -----------
###############################
## a) Beta Error Model Cubes -----------
par(mfrow = c(1,2), oma=c(0,0,0,0), mar=c(0,1,0,1), cex=1.5)

x <- seq(1,11)
y <- seq(1,6)
z <- seq(1,8)
grid   <- mesh(x, y, z)
colvar <- with(grid, y*exp(-x^2 - y^3 - z^4))

slice3D(x, y, z, 
        xs = 4, ys= NULL, zs = 3,
        xlab= "Beta-Methods",ylab="Beta-Loadings",zlab="AMTs",
        #       xs = c(-1, -0.5, 0.5), ys = c(-1, 0, 1), zs = c(-1, 0), 
        colvar = colvar, col= "pink", bty= "b2",
        theta = 45, phi = 25,
        lighting = TRUE, shade=100,
        ltheta = 135, lphi = -100)

slice3D(x, y, z, 
        xs = 4, ys= 3, zs = 3,
        xlab= "Beta-Methods",ylab="Error-Methods",zlab="AMTs",
        #       xs = c(-1, -0.5, 0.5), ys = c(-1, 0, 1), zs = c(-1, 0), 
        colvar = colvar, col= "cyan", bty= "b2",
        theta = 45, phi = 25,
        lighting = TRUE, shade=100,
        ltheta = 135, lphi = -100)