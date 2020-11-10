rm(list = ls())
library(fpp2)
library(gridExtra)
library(GGally)
library(Holidays)
library(TimeWarp)

df = read.csv("DIX.csv", header = TRUE)
df$date = as.Date(df$date)

# Options expirations dates between 5/2011:12/2020
d1 <- dateSeq('2011-05-01', len=115, by='months')
d2 <- dateAlign(d1, by='months', dir=-1)
d3 <- dateAlign(d2, by='weeks', week.align=5)
d4 <- dateWarp(d3, 14, by='days')
d5 <- dateAlign(d4, by='bizdays@NYSEC', dir=-1)

OpExDates <- as.Date(d5)

for(i in 1:length(df$date)){
  df$daysUntilOpEx[i] <- as.numeric(max((df$date[i]-OpExDates)[df$date[i]-OpExDates <= 0]))
}
#remove inf values and negative values
df$daysUntilOpEx <- df$daysUntilOpEx[!is.infinite(df$daysUntilOpEx)]
df$daysUntilOpEx <- abs(df$daysUntilOpEx)

sapply(df, class)
summary(df)

spx_plot = ggplot(df, aes(date, price)) +
  geom_line(na.rm=TRUE) +
  ggtitle("SPX Performance") +
  xlab("Date") + ylab("Price")
spx_plot

#find optimal log to normalize variance
df$log_price = BoxCox(df$price,BoxCox.lambda(df$price))

spx_log = ggplot(df, aes(date, log_price)) +
  geom_line(na.rm = TRUE) + 
  ggtitle("SPX Performance (Log Scale)")+
  xlab("Date") + ylab("Price")
spx_log

gridExtra::grid.arrange(spx_plot, spx_log)
#Find percent change day to day
df$pct_change = diff(df$price)/df$price * 100
df$pct_change = data.table::shift(df$pct_change, n=1, fill=NA, type="lag")
df$log_pct_change = diff(df$log_price)/df$log_price * 100
df$log_pct_change = data.table::shift(df$log_pct_change, n=1, fill=NA, type="lag")
df$vix_change = diff(df$vix)/df$vix * 100
df$vix_change = data.table::shift(df$vix_change, n=1, fill=NA, type="lag")

spx_pct = ggplot(df, aes(date, pct_change)) +
  geom_line(na.rm = TRUE) + 
  ggtitle("Percent Change of SPX from Prior Close")+
  xlab("Date") + ylab("Percent Change")
spx_pct

ggAcf(df$pct_change)
ggAcf(df$log_pct_change)
ggAcf(diff(df$vix))
ggAcf(diff(diff(df$vix)))

ggAcf(df$vix_change)

#check distribution of returns
gghistogram(df$pct_change, bins=250) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept=median(df$pct_change), na.rm = TRUE) + 
  ggtitle("Distribution of Daily SPX Returns") + 
  xlab("Percent Change")

#compare daily returns vs vix values
spx_vix = ggplot(df, aes(date)) + 
  geom_point(aes(y=pct_change), colour='blue', alpha=I(0.25)) +
  geom_line(aes(y=vix/12), colour='orange', size=1) +
  scale_y_continuous(
    name = "Percent Change of SPX from Prior Close",
    sec.axis = sec_axis(~.*12, name="Volatility Index")) +
  ggtitle("Daily Volatility Index Reading & Returns of the SPX Index")

spx_vix

#compare daily returns vs vix pct change values
spx_vixchange = ggplot(df, aes(date)) + 
  geom_point(aes(y=pct_change), colour='blue', alpha=I(0.25)) +
  geom_line(aes(y=vix_change/12), colour='orange', size=1, alpha=I(0.5)) +
  ggtitle("Daily Volatility Index Reading & Returns of the SPX Index")

spx_vixchange

#view secondary market activity and gamma exposure
dix = ggplot(df, aes(date, dix)) +
  geom_line(na.rm=TRUE) + 
  geom_hline(yintercept = median(df$dix)) +
  geom_hline(yintercept = .39) +
  geom_hline(yintercept = .45) +
  ggtitle("Dark Index History") +
  xlab("Date") + ylab("Dark Index Activity")

gex = ggplot(df, aes(date, gex)) +
  geom_line(na.rm=TRUE) +
  geom_hline(yintercept = median(df$gex)) +
  geom_hline(yintercept = 0) +
  ggtitle("Gamma Exposure to SPX") +
  xlab("Date") + ylab("Gamma Activity in Options Market")

gridExtra::grid.arrange(spx_log, dix, gex)

#view returns by gamma level
ggplot(df, aes(gex, pct_change)) + 
  geom_point(na.rm = TRUE) +
  geom_smooth(method = 'loess', level=0.99) +
  ggtitle("SPX Returns by Gamma Level") + 
  xlab("Gamma Exposure (in Billions $USD)") + ylab("% Return")

#develop covariates to measure seasonality
df$weekday = as.POSIXlt(df$date)$wday

#create dummy variables for M-F
for(i in 1:length(df$date))
  if (df$weekday[i]==1) {
    df$Monday[i]=1
  } else {
      df$Monday[i]=0
  }
for(i in 1:length(df$date))
  if (df$weekday[i]==2) {
    df$Tuesday[i]=1
  } else {
    df$Tuesday[i]=0
  }
for(i in 1:length(df$date))
  if (df$weekday[i]==3) {
    df$Wednesday[i]=1
  } else {
    df$Wednesday[i]=0
  }
for(i in 1:length(df$date))
  if (df$weekday[i]==4) {
    df$Thursday[i]=1
  } else {
    df$Thursday[i]=0
  }
for(i in 1:length(df$date))
  if (df$weekday[i]==5) {
    df$Friday[i]=1
  } else {
    df$Friday[i]=0
  }
ggplot(df, aes(x=weekday, y=pct_change, group=weekday)) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) + 
  scale_y_continuous(limits = quantile(df$pct_change, c(0.1, 0.9), na.rm=TRUE)) +
  labs(title = "% Returns of SPX by Weekday",
       subtitle = "Outliers are ignored") +
  xlab("Day of Week (M-F)") + ylab("Percent Return")

ggplot(df, aes(x=weekday, y=vix, group=weekday)) +
  geom_boxplot() + 
  scale_y_continuous(limits = quantile(df$vix, c(0, 1), na.rm=TRUE)) +
  labs(title = "VIX by Weekday",
       subtitle = "Implied Volatility of SPX") +
  xlab("Day of Week (M-F)") + ylab("VIX Value")

#remove outliers to see quartiles better
ggplot(df, aes(x=weekday, y=vix, group=weekday)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = quantile(df$vix, c(0.1, .9), na.rm=TRUE)) +
  labs(title = "VIX by Weekday",
       subtitle = "Implied Volatility of SPX: Outliers Removed") +
  xlab("Day of Week (M-F)") + ylab("VIX Value")

ggplot(df, aes(x=daysUntilOpEx, y=gex, group=daysUntilOpEx)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_smooth() +
  labs(title = "Days until Options Expiration Effect on GEX")+
  xlab("Days until OpEx Date") + ylab("GEX Value")

ggplot(df, aes(x=daysUntilOpEx, y=vix_change, group=daysUntilOpEx)) +
  geom_point() + 
  labs(title = "Days until Options Expiration Effect on VIX")+
  xlab("Days until OpEx Date") + ylab("VIX Percent Change")

#create time series for modeling 
projts<-ts(data.frame(df$vix_change,df$dix,df$gex,df$daysUntilOpEx,df$Monday,df$Tuesday,df$Wednesday,df$Thursday,df$Friday), names = c("VIX_CHANGE","DIX","GEX","daysUntilOpEx","MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY" ), frequency = 1)
head(projts)
#create target time series
VIXts<-ts(projts[29:2382,"VIX_CHANGE"], names = c("VIX_chg"))
head(VIXts)
#create lags of GEX and DIX for modeling
GEXLag <- ts(cbind(
  GEXLag1 = lag(projts[,"GEX"],-1),
  GEXLag2 = lag(projts[,"GEX"],-2),
  GEXLag3 = lag(projts[,"GEX"],-3),
  GEXLag4 = lag(projts[,"GEX"],-4),  
  GEXLag5 = lag(projts[,"GEX"],-5),
  GEXLag6 = lag(projts[,"GEX"],-6),
  GEXLag7 = lag(projts[,"GEX"],-7),
  GEXLag8 = lag(projts[,"GEX"],-8),
  GEXLag9 = lag(projts[,"GEX"],-9),
  GEXLag10 = lag(projts[,"GEX"],-10),  
  GEXLag11 = lag(projts[,"GEX"],-11),
  GEXLag12 = lag(projts[,"GEX"],-12),
  GEXLag13 = lag(projts[,"GEX"],-13),
  GEXLag14 = lag(projts[,"GEX"],-14),
  GEXLag15 = lag(projts[,"GEX"],-15),
  GEXLag16 = lag(projts[,"GEX"],-16),
  GEXLag17 = lag(projts[,"GEX"],-17),  
  GEXLag18 = lag(projts[,"GEX"],-18),
  GEXLag19 = lag(projts[,"GEX"],-19),
  GEXLag20 = lag(projts[,"GEX"],-20),
  GEXLag21 = lag(projts[,"GEX"],-21),
  GEXLag22 = lag(projts[,"GEX"],-22),
  GEXLag23 = lag(projts[,"GEX"],-23),  
  GEXLag24 = lag(projts[,"GEX"],-24),
  GEXLag25 = lag(projts[,"GEX"],-25),
  GEXLag26 = lag(projts[,"GEX"],-26),
  GEXLag27 = lag(projts[,"GEX"],-27),
  GEXLag28 = lag(projts[,"GEX"],-28)))
head(GEXLag)
GEXlagts <- GEXLag[29:2382,]
DIXLag <- cbind(
  DIXLag1 = lag(projts[,"DIX"],-1),
  DIXLag2 = lag(projts[,"DIX"],-2),
  DIXLag3 = lag(projts[,"DIX"],-3),
  DIXLag4 = lag(projts[,"DIX"],-4),  
  DIXLag5 = lag(projts[,"DIX"],-5),
  DIXLag6 = lag(projts[,"DIX"],-6),
  DIXLag7 = lag(projts[,"DIX"],-7),
  DIXLag8 = lag(projts[,"DIX"],-8),
  DIXLag9 = lag(projts[,"DIX"],-9),
  DIXLag10 = lag(projts[,"DIX"],-10),  
  DIXLag11 = lag(projts[,"DIX"],-11),
  DIXLag12 = lag(projts[,"DIX"],-12),
  DIXLag13 = lag(projts[,"DIX"],-13),
  DIXLag14 = lag(projts[,"DIX"],-14),
  DIXLag15 = lag(projts[,"DIX"],-15),
  DIXLag16 = lag(projts[,"DIX"],-16),
  DIXLag17 = lag(projts[,"DIX"],-17),  
  DIXLag18 = lag(projts[,"DIX"],-18),
  DIXLag19 = lag(projts[,"DIX"],-19),
  DIXLag20 = lag(projts[,"DIX"],-20),
  DIXLag21 = lag(projts[,"DIX"],-21),
  DIXLag22 = lag(projts[,"DIX"],-22),
  DIXLag23 = lag(projts[,"DIX"],-23),  
  DIXLag24 = lag(projts[,"DIX"],-24),
  DIXLag25 = lag(projts[,"DIX"],-25),
  DIXLag26 = lag(projts[,"DIX"],-26),
  DIXLag27 = lag(projts[,"DIX"],-27),
  DIXLag28 = lag(projts[,"DIX"],-28))
head(DIXLag)
DIXlagts <- DIXLag[29:2382,]
head(DIXlagts)
#create other variables ts
Timets <- ts(cbind(
  OpExts = projts[29:2382,"daysUntilOpEx"],
  Mondayts = projts[29:2382,"MONDAY"],
  Tuesdayts = projts[29:2382,"TUESDAY"],
  Wednesdayts = projts[29:2382,"WEDNESDAY"],
  Thursdayts = projts[29:2382,"THURSDAY"],
  Fridayts = projts[29:2382,"FRIDAY"]))
head(Timets)

#model with dix and gex lags
dixgexcolnames<-colnames(dixgex)
dixgexcolnames
dixgex<-cbind(VIXts, GEXlagts, DIXlagts)
head(dixgex)
dgfit<-lm(VIXts~., data=dixgex)
summary(dgfit)
dgfit2<-lm(VIXts~GEXlagts.GEXLag1+GEXlagts.GEXLag2+GEXlagts.GEXLag3+GEXlagts.GEXLag6+GEXlagts.GEXLag12+
           GEXlagts.GEXLag14+GEXlagts.GEXLag16+GEXlagts.GEXLag22+DIXlagts.DIXLag1+DIXlagts.DIXLag2+
           DIXlagts.DIXLag3+DIXlagts.DIXLag7+DIXlagts.DIXLag10+DIXlagts.DIXLag19+DIXlagts.DIXLag20, data = dixgex)
summary(dgfit2)
dgfit3<-lm(VIXts~GEXlagts.GEXLag1+GEXlagts.GEXLag2+GEXlagts.GEXLag3+GEXlagts.GEXLag6+GEXlagts.GEXLag12+
             GEXlagts.GEXLag14+GEXlagts.GEXLag16+DIXlagts.DIXLag1+DIXlagts.DIXLag2+
             DIXlagts.DIXLag19+DIXlagts.DIXLag20, data = dixgex)
summary(dgfit3)
dgfit4<-lm(VIXts~GEXlagts.GEXLag1+GEXlagts.GEXLag2+GEXlagts.GEXLag3+GEXlagts.GEXLag6+GEXlagts.GEXLag12+
             GEXlagts.GEXLag14+DIXlagts.DIXLag1+DIXlagts.DIXLag2+
             DIXlagts.DIXLag20, data = dixgex)
summary(dgfit4)
dgfit5<-lm(VIXts~GEXlagts.GEXLag1+GEXlagts.GEXLag2+GEXlagts.GEXLag3+
             GEXlagts.GEXLag14+DIXlagts.DIXLag1+DIXlagts.DIXLag2+
             DIXlagts.DIXLag20, data = dixgex)
summary(dgfit5)
dgfit5<-lm(VIXts~GEXlagts.GEXLag1+GEXlagts.GEXLag2+GEXlagts.GEXLag3+
             GEXlagts.GEXLag14+DIXlagts.DIXLag1+DIXlagts.DIXLag2+
             DIXlagts.DIXLag20, data = dixgex)
summary(dgfit5)
dgfit6<-lm(VIXts~GEXlagts.GEXLag1+GEXlagts.GEXLag2+GEXlagts.GEXLag3+
             GEXlagts.GEXLag14+DIXlagts.DIXLag1+DIXlagts.DIXLag2, data = dixgex)
summary(dgfit6)
#arima model on dixgex
regex<-cbind(GEXlagts, DIXlagts)
head(regex)
dgfitaa<-auto.arima(VIXts, xreg = regex)
summary(dgfitaa)
checkresiduals(dgfitaa)
