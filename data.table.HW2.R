## data.table package
library(data.table)
library(lubridate)
#set working directory to file containing csv
setwd("/Users/valeriaduran/Dropbox/Summer 2020/Math 6386/HW2_Data [eCommerce]")

system.time(dat <- fread("eCommerce.csv",header=TRUE))
head(dat)

#data structuring 
system.time(subset.1 <- dat[,.(ymd_hms(event_time), user_id, brand, price, event_type, user_session)])

#Q1 #calculate avg daily visitors
head(subset.1)
daily.visits <- subset.1[,.N, by=.(day(V1))][,mean(N)] #282742.1

#Q2 find which day of the week has the highest visits
day_highest <- subset.1[, wday1 := (wday(V1, label = TRUE))]
day_highest[,.N,by=.(wday1)]
system.time(day_highest[,.N,by=.(wday1)][which.max(N)]) #Tuesday has highest visits


#3. calculate the average price for each brand
system.time(brand.avg <- day_highest[,mean(price), by=brand][order(brand)])
brand.avg

#4. Calculate the conversion rates for each brand
counts<- day_highest[event_type=="view" | event_type=="purchase"][,.(event_type,brand)]
views <- counts[event_type=='view',.(view.count= .N),by=brand][order(brand)]
purchases <- counts[event_type=='purchase',.(purch.count= .N),by=brand][order(brand)]
system.time(conversion.rates<- views[purchases, on = .(brand)][is.na(view.count),view.count:=0][is.na(purch.count),purch.count :=0][order(brand)][,.(ratio=purch.count/view.count),by=brand])
conversion.rates
