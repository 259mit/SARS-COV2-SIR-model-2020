mcts<-diff(mconf)
mcts<-ts(mcts)
View(mcts)
plot(mcts)
library(astsa)
library(tseries)
library(forecast)
a1<-auto.arima(mcts, allowdrift = T)
summary(a1)
a1
autoplot(a1)
f<-forecast(a1, h=16, level = 80)
autoplot(f)
f1<-as.data.frame(f)
f1$`Point Forecast`
mcts
totmcts<-c(mcts,f1$`Point Forecast`)
length(totmcts)
plot(totmcts, type = "l")

a2<-auto.arima(mconf, allowdrift = T)
summary(a2)
a2
f2<-forecast(a2, h=16, level = 80)
autoplot(f2)
