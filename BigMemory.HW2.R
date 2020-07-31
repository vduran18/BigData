##Bigmemory 
library(bigmemory)
library(biglm)
library(DBI)
library(foreach)
library(biganalytics)
library(bigtabulate)
library(data.table)
#read in the data and prepare it for bigmemory 

comm.struct <- read.csv("eCommerce.csv", header = TRUE, sep = ",")
str(comm.struct)

subset.1 <- as.data.frame(comm.struct[,c(3,4,8,9,11)])
names(subset.1)
class(subset.1)

subset.1$event_time = as.Date(subset.1[,'event_time'])
head(subset.1)
# Calculate day of the week
subset.1$wday = weekdays(subset.1$event_time)
subset.1$event_time = as.factor(subset.1[,'event_time'])
str(subset.1)
# identify classes of columns
classes <- unlist(lapply(colnames(subset.1), function(x) {
  class(subset.1[,x])
}))
print(classes)
# Convert columns that is factor type to interger
subset.1$price <- subset.1$price*100
head(subset.1$price)
ind <- which(classes=="character")
ind
for(i in ind) {subset.1[,i] <- as.factor(subset.1[, i])}
str(subset.1)
# Convert columns that is factor type to interger
print(str(subset.1))

ind <- which(classes!="integer")
ind
for(i in ind) {subset.1[,i] <- as.integer(subset.1[, i])}

str(subset.1)

write.table(subset.1, "need_data.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)

### start bigmemory work
system.time(ecom.mat <- read.big.matrix("need_data.csv", header = TRUE, 
                            sep = ",", type = "double" ))

# 2. Average daily visitors and busiest day of the week
system.time(avg_visits <- mean(table(ecom.mat[,"event_time"])))
avg_visits #282742.1

n_unique <- function(x) {
  return(length(unique(x)))
}

# busiest day of week 
busy_day <- function(x){
  wkday = bigsplit(ecom.mat, ccols = "wday", splitcol = "user_session")
  visitors = sapply(wkday, n_unique)
  visitors = sort(visitors,decreasing = TRUE)
  busiest_day=as.integer(names(visitors[1]))
  print(ecom.mat[busiest_day, "wday"])
}
system.time(busy_day(ecom.mat)) #Tuesday 


#Q3 find the average price of brand
brand_price <- function(x){
  names <- unique(sort(comm.struct$brand), decreasing=TRUE)
  head(names)
  df.brand <- bigsplit(ecom.mat, ccols = "brand", splitcol = "price")
  ave_price = sapply(df.brand, mean)
  ave_price <- as.data.frame(sort(ave_price,decreasing = TRUE))
  ave_price <- ave_price/100
  row.names(ave_price) <- names
  print(ave_price)
}
system.time(brand_price(ecom.mat))

#Q4. Calculate the conversion rates for each brand
conv_rates <- function(dt){
  return(sum(dt==2)/sum(dt==3))
}
rates_conversion <- function(x){
  brand.split <- bigsplit(ecom.mat, ccols = "brand", splitcol = "event_type")
  rate = sapply(brand.split, conv_rates)
  rate = sort(rate,decreasing = TRUE)
  rate = as.data.frame(rate)
  names <- unique(sort(comm.struct$brand), decreasing=TRUE)
  row.names(rate) <- names
  print(rate)
} 
system.time(rates_conversion(ecom.mat))

