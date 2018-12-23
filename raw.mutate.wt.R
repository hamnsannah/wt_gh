# takes lightly revised data from chuck and adds columns needed for many vizzes
# before using, delete the first two rows of data so column names are in row 1.  Also remove ; from ";Department"

raw.mutate.wt <- function(filename.in.whaletale){
  require(lubridate)
  require(dplyr)
  raw.data <- read.csv(filename.in.whaletale, stringsAsFactors = FALSE)
  print(dim(raw.data))
  print(head(raw.data))
  #raw.data <- filename.in.whaletale
  #colnames(raw.data) <- raw.data[2,]
  data.both <- raw.data
  data.both$Total.Sales <- as.numeric(data.both$Total.Sales)
  print(head(raw.data, 5))
  data.both <- filter(data.both, !is.na(Total.Sales)) #removes the sums that had $ signs in them
  print("line 9")
  #36print(paste("sum of Total.Sales is", sum(data.both$Total.Sales)))
  data.both$Date.Sold <- mdy_hm(data.both$Date.Sold)
  data.both$Year <- year(data.both$Date.Sold)
  data.both$Month <- month(data.both$Date.Sold)
  data.both$Department <- trimws(data.both$Department)
  data.both$Category <- trimws(data.both$Category)
  data.both$Supplier <- trimws(data.both$Supplier)
  data.both$Item <- trimws(data.both$Item)
  #print(head(data.both))
  data.both$Dept.By.Year <- paste(data.both$Department, data.both$Year)
  data.both$Categ.By.Year <- paste(data.both$Category, data.both$Year)
  print("line 20")
  write.csv(data.both, paste0("mutated.", filename.in.whaletale), row.names = FALSE)
  print(head(data.both))
}