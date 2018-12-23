# this function prepares data for ab.trail

ab.shiny.subsetting <- function(grouping, hierarchy.value, start.date, end.date, output.metric = "Total Sales", output.ab.input){
  require(dplyr)
  require(lubridate)
  data.sub <- output.ab.input
  data.sub$Date <- as_date(data.sub$Date.Sold)
  column.sub <- data.sub[colnames(data.sub) == grouping]
  data.sub <- cbind(column.sub, select(data.sub, Date, Total.Sales, Cost, Profit))
  print("before filter 1")
  data.sub <- filter(data.sub, Date >= start.date, Date <= end.date)
  print("between filters")
  colnames(data.sub)[1] <- "Grouping"
  print(head(data.sub))
  data.sub <- filter(data.sub, Grouping == hierarchy.value)
  
  print("after filters")
  data.agg <- aggregate(Total.Sales ~ Date + Grouping, data.sub, sum)
  print(head(data.agg))
  date.sequence <- data.frame("Date" = seq.Date(from = as_date(start.date), to = as_date(end.date), by = "day"))
  
  data.agg.w.zero.days <- merge(data.agg, date.sequence, all.y = TRUE, all.x = TRUE)
  data.agg.w.zero.days$Grouping <- hierarchy.value
  data.agg.w.zero.days[is.na(data.agg.w.zero.days$Total.Sales),3] <- 0 
  #print(head(data.agg.w.zero.days))
  data.agg.w.zero.days
  just.daily.sales.vec <- as.numeric(data.agg.w.zero.days$Total.Sales)
  print(head(just.daily.sales.vec))
  just.daily.sales.vec
}