rm(list = ls())
library(fpp2)
library(GGally)

dark_index = read.csv("DIX.csv", header = TRUE)
dark_index$date = as.Date(dark_index$date)
#row.names(dark_index) = dark_index$date

head(dark_index)
sapply(dark_index, class)
summary(dark_index)

ggplot(dark_index, aes(date, price)) +
  geom_line(na.rm=TRUE) +
  ggtitle("SPX Performance") +
  xlab("Date") + ylab("Price")

dix = ggplot(dark_index, aes(date, dix)) +
  geom_line(na.rm=TRUE) + 
  geom_hline(yintercept = median(dark_index$dix)) +
  geom_hline(yintercept = .39) +
  geom_hline(yintercept = .45) +
  ggtitle("Dark Index History") +
  xlab("Date") + ylab("Dark Index Activity")

dix

gex = ggplot(dark_index, aes(date, gex)) +
  geom_line(na.rm=TRUE) +
  geom_hline(yintercept = median(dark_index$gex)) +
  geom_hline(yintercept = 0) +
  ggtitle("Gamma Exposure to SPX") +
  xlab("Date") + ylab("Gamma Activity in Options Market")

gex

ggplot(dark_index, aes(date, BoxCox(price,BoxCox.lambda(price)))) +
  geom_line(na.rm = TRUE) + 
  ggtitle("SPX Performance (Log Scale)")+
  xlab("Date") + ylab("Price")