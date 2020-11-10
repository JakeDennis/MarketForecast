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

#create moving averages



#find appropriate lag


