library(bit)
library(ff)
library(ffbase)
library(plyr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(doBy)
library(dplyr)

p = getwd()
system("mkdir ffdf") #system #create folder 

p = paste(p, "/ffdf", sep="") #paste folder in the path chosen

options(fftempdir = p) #will set this folder path
gc()
system.time(commerce.ff <- read.csv.ffdf(file="eCommerce.csv", 
                                         VERBOSE=TRUE, header=TRUE, 
                                         next.rows=500000, colClasses=NA))

#data structuring 
names(commerce.ff)
system.time(comm.ff <- as.ffdf(commerce.ff[,c(3,4,8,9,11)]))

#1. find the average daily visitors
#create a subset and transform dates into POSX type
ptm <- proc.time()
comm.subset <- as.data.frame.ffdf(comm.ff)
comm.subset$event_time <- ymd_hms(as.data.frame.ffdf(commerce.ff$event_time))
comm.subset %>% 
              count(day = floor_date(event_time, "day")) %>% summarize(average = mean(n)) -> daily_visits
daily_visits
proc.time() - ptm

#Q2 which day has highest visits
ptm <- proc.time()
day_highest <- ymd_hms(as.data.frame.ffdf(commerce.ff$event_time))
which.max(table(wday(day_highest, label = TRUE)))
proc.time() - ptm


#Q3 calculate average price for brand
system.time(brand_avg <- ffdfdply(comm.ff, 
                               split = comm.ff$brand,
                               FUN=function(x) {
                                 summaryBy(price~brand, 
                                           data=x, FUN=mean, na.rm=TRUE)}))
brand_avg


#4. Calculate the conversion rates for each brand
ptm <- proc.time()
subs1.view.ff <- subset.ffdf(comm.ff, event_type == 'view', 
                        select = brand)

subs1.purch.ff <- subset.ffdf(comm.ff, event_type == 'purchase', 
                             select = brand)

conv.ratios <- table.ff(subs1.purch.ff$brand) / table.ff(subs1.view.ff$brand)
conv.ratios
proc.time() - ptm
write.csv(conv.ratios, "conversion_ratios_ff.csv")

