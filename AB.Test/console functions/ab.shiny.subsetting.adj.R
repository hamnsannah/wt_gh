# this function prepares data for ab.trail

#sample usage: a.vec <- ab.shiny.subsetting.adj(grouping = "Category", hierarchy.value = "BIRTHDAY", 
  #start.date = "2017-07-01", end.date = "2017-07-31", 
  #output.metric = "Total Sales", output.ab.input = mutated.four.years, seasonality = TRUE)

ab.shiny.subsetting.adj <- function(grouping, hierarchy.value, start.date, end.date, 
                                output.metric = "Total Sales", output.ab.input, seasonality = FALSE){
  require(dplyr)
  require(lubridate)
  data.sub <- output.ab.input
  data.sub$Date <- as_date(data.sub$Date.Sold)
  column.sub <- data.sub[colnames(data.sub) == grouping]
  data.sub <- cbind(column.sub, select(data.sub, Date, Total.Sales, Cost, Profit))
  data.sub.all.dates <- data.sub #for use by prophet later
  print("before filter 1")
  data.sub <- filter(data.sub, Date >= start.date, Date <= end.date)
  print("between filters")
  colnames(data.sub)[1] <- "Grouping"
  #print(head(data.sub))
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
  
  if(seasonality == TRUE){
    
    # this section adjusts the selected data based on weekly (day of week) and monthly (time of the year) seasonailty
    # process is
      # 1 filter data to the indicated hierarchy.value
      # 2 fill in missing data with zeros so every day has a value
      # 3 calcluate seasonality
      # 4 isolate adjustment amount which is sum of weekly and seasonal columns
      
    require(prophet)
    colnames(data.sub.all.dates)[1] <- "Grouping"
    data.all.agg <- aggregate(Total.Sales ~ Date + Grouping, data.sub.all.dates, sum)
    print(head(data.all.agg, 10))
    data.all.agg <- filter(data.all.agg, Grouping == hierarchy.value)
    date.sequence.proph <- data.frame("Date" = seq.Date(from = as_date("2014-01-01"), to = as_date("2017-12-31"), by = "day"))
    data.proph.zero <- merge(data.all.agg, date.sequence.proph, all.y = TRUE, all.x = TRUE)
    data.proph.zero$Grouping <- hierarchy.value
    data.proph.zero[is.na(data.proph.zero$Total.Sales),3] <- 0 
    colnames(data.proph.zero) <- c("ds", "Grouping", "y")
    prophet.data <- data.proph.zero
    prophet.data <- select(prophet.data, ds, y)
    model <- prophet(prophet.data, yearly.seasonality = TRUE)
    future <- make_future_dataframe(model, periods = 365)
    forecast <- predict(model, future)
    forecast.cols <- select(forecast, ds, seasonal, weekly)
    forecast.cols <- forecast.cols %>%
      mutate("adjust" = (seasonal*-1) + (weekly*-1))
    forecast.cols$ds <- as_date(forecast.cols$ds)

    adjusted.df <- merge(data.agg.w.zero.days, forecast.cols, all.x = TRUE, all.y = FALSE, by.x = "Date", by.y = "ds")
    adjusted.df <- adjusted.df %>%
      mutate("adjusted.y" = Total.Sales + adjust)
    print(head(adjusted.df))
    just.daily.sales.vec <- adjusted.df$adjusted.y
  
  }
  print(head(just.daily.sales.vec))
  just.daily.sales.vec
}