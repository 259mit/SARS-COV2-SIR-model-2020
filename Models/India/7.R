data<-covid_19_india
Vstr(data)
View(data)
z<-as.Date("1/01/10", "%d/%m/%Y")
data$Date<-as.Date(data$Date, "%d/%m/%Y")
library(lubridate)
year(data$Date)
unique(data$Date)
unique(data$State.UnionTerritory)
nrow(data)
maharashtrad<-subset(data,data$State.UnionTerritory=="Maharashtra")
View(maharashtrad)
nrow(maharashtrad)
srno<-c(1:56)
mconf<-c(maharashtrad$Confirmed)
mcure<-c(maharashtrad$Cured)
mdead<-c(maharashtrad$Deaths)
plot(mconf, type = "l")
mconf[18]<-c(128)
mconf[3]<-c(5)
day(maharashtrad$Date)
macti<-mconf-mcure-mdead
mdata<-data.frame(srno,mconf,macti,mcure,mdead)
View(mdata)






