##parallel
library(snow)
library(parallel)
library(lubridate)
library(foreach)
library(doParallel)
library(data.table)

#set working directory to file containing csv
setwd("/Users/valeriaduran/Dropbox/Summer 2020/Math 6386/HW2_Data [eCommerce]")

system.time(dat <- fread("eCommerce.csv",header=TRUE))
head(dat)

#data structuring 
system.time(subset.1 <- dat[,.(as.Date(event_time), user_id, brand, price, event_type, user_session)])
head(subset.1)
names(subset.1)[1] <- "event_date"
subset.1$wkday <- weekdays(subset.1$event_date)

numOfProcessors <- detectCores(logical = TRUE) 
cl <- makeCluster(numOfProcessors-1, type = "SOCK") 
system.time(ds.para <- unlist(clusterApply(cl, table(subset.1$event_date), fun=sum, na.rm=TRUE)))
mean(ds.para)


#Q1 & Q2 average daily visitors & day with highest visits
visitors<- function(x){
  ds.para <- unlist(clusterApply(cl, table(subset.1$event_date), fun=sum, na.rm=TRUE))
  daily_visits <- mean(ds.para)
  print("Average daily visits:")
  print(daily_visits)
  highest_day <- which.max(table(subset.1$wkday))
  print("Day with highest visits:")
  print(highest_day)
}
system.time(visitors(subset.1))
stopCluster(cl)

registerDoParallel(cl)


#Q3 find the average price of brand
price_average <- function(x){
  brand_sub = split(subset.1, subset.1$brand)
  brand_avg = (foreach(i = brand_sub, .combine = rbind) %dopar% (data.frame(brand = unique(i$brand), price_avg = mean(i$price))))
  print(brand_avg)
}
system.time(price_average(subset.1))   

#Q4 Calculate the conversion rates for each brand
conv_rate <- function(x){
  brand_sub = split(subset.1, subset.1$brand)
  rates = (foreach(i = unique(brand_sub), .combine = rbind) %dopar% (data.frame(brand = unique(i$brand), event_view = unique(i$event_type=="view"),
                                                                            freq_view = length(i$event_type=="view"), event_purch = unique(i$event_type=="purchase"),
                                                                            freq_purch = length(i$event_type=="purchase"))))
  print(rates)
}
rates
system.time(conv_rate(subset.1))
brand_sub = split(subset.1, list(subset.1$brand))
rates = (foreach(i = brand_sub, .combine = rbind) %dopar% (data.frame(brand = i$brand, 
                                                              event_type = unique(i$event_type), 
                                                              freq = length(i$event_type))))
head(rates)

system.time({
  conv_rate = {
    cr = split(subset.1, list(subset.1$brand))
    f = foreach(i = cr, .combine = rbind) %dopar% (data.frame(brand = unique(i$brand), 
                                                              event_type = unique(i$event_type), 
                                                              freq = length(i$event_type)))
  }
})
subset.2<- subset(conv_rate, (event_type=="view" | event_type=="purchase"))
