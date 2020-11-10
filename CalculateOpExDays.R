# Options expirations dates between 5/2011:12/2020
d1 <- dateSeq('2011-05-01', len=115, by='months')
d2 <- dateAlign(d1, by='months', dir=-1)
d3 <- dateAlign(d2, by='weeks', week.align=5)
d4 <- dateWarp(d3, 14, by='days')
d5 <- dateAlign(d4, by='bizdays@NYSEC', dir=-1)

df$OpExDates <- as.Date(d5)

for(i in 1:length(df$date)){
  df$daysUntilOpEx[i] <- as.numeric(max((df$date[i]-df$OpExDates)[df$date[i]-OpExDates < 0]))
}
#remove inf values and negative values
df$daysUntilOpEx <- df$daysUntilOpEx[!is.infinite(df$daysUntilOpEx)]
df$daysUntilOpEx <- abs(df$daysUntilOpEx)

head(df, 30)
tail(df,30)

