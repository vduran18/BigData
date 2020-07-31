##Use traditional R
library(tidyverse)
library(dplyr)
library(lubridate)

#set working directory to file containing csv
setwd("/Users/valeriaduran/Dropbox/Summer 2020/Math 6386/HW2_Data [eCommerce]")

#read in csv file 
system.time(ecom <- read.csv("eCommerce.csv",header=TRUE))

names(ecom) #check names of each column

#calculate avg daily visitors and 
#find which day of the week has the highest number of daily visits.

head(ecom)

#use the lubridate package to convert the event_time into a POSIXct class to ease use of date-time
#create a new dataframe using the POSIXct date and the user_id column
system.time(new_dat <- cbind.data.frame("date" = ymd_hms(ecom$event_time),
                            "user" = ecom$user_id, "brand" = ecom$brand, "price" = ecom$price,
                            "type" = ecom$event_type))
head(new_dat)
class(new_dat$date)

#1. find the average daily visitors using pipelines and tidyverse 
system.time(new_dat %>% 
  count(days = floor_date(date, "day")) %>% summarize(average = mean(n)) -> daily_visits)

daily_visits #282742.1

##2. find which day of the week has the highest visits and use ggplot barplot
system.time(new_dat %>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar() -> p)

p #Tuesday
head(new_dat$brand)                                                    
new_dat[]

#3. calculate the average price for each brand by using tidyverse pipeline, group_by() and summarize()
system.time(brand.price.avg <- new_dat %>% group_by(brand) %>% summarize(avg_price = mean(price)))     
brand.price.avg
write.csv(brand.price.avg, "average_brand_price.csv")

#4.	Calculate the conversion rates for each brand
system.time(sub <- new_dat %>% select(brand, type) %>% filter(type=='view' | type== 'purchase'))

system.time(views <- sub %>% group_by(brand, type) %>% tally(name = 'view.ct') %>% filter(type=='view')) 
purchases <- sub %>% group_by(brand, type) %>% tally(name = 'purch.ct') %>% filter(type=='purchase')
purch <- purchases[,c(1,3)]
vie <- views[,c(1,3)]
zz <- merge(vie, purch, all = TRUE)
zz[is.na(zz)] <- 0
zz


system.time(dat.ratio <- zz %>% mutate(conversion_ratio = (purch.ct/view.ct)*100))
head(dat.ratio)
write.csv(dat.ratio, "conversion_rates.csv")
