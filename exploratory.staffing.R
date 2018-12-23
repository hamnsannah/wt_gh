# staffing  levels

# need to visualize data in multiple ways.

# use sugrrants package to visualize throughout the year
library(dplyr)
library(ggplot2)
library(sugrrants)
library(lubridate)

data4years <- read.csv("mutated.data1417.csv", stringsAsFactors = FALSE)
data17 <- filter(data4years, Year == 2017)
data17$Date.Sold <- as_datetime(data17$Date.Sold)
data17$Date.Sold.Round <- round_date(data17$Date.Sold, unit = "hour")
data17$Date.Sold <- as_date(data17$Date.Sold)

data17.agg <- aggregate(Total.Sales ~ Date.Sold.Round + Date.Sold, data17, sum)
data17.agg <- data17.agg %>%
  mutate("Hour" = as.integer(hour(Date.Sold.Round))) %>%
  frame_calendar(x = Hour, y = Total.Sales, date = Date.Sold, calendar = "monthly")
data17.plot <- data17.agg %>%
  ggplot(aes(x = .Hour, y = .Total.Sales, group = Date.Sold)) + geom_line()
prettify(data17.plot)
# STILL INCLUDES SPLASH WHICH NEEDS TO BE REMOVED

# Look at weather for correlations but this might take some time
# looks like weatherData will help: https://www.rdocumentation.org/packages/weatherData/versions/0.5.0

#https://www.computerworld.com/article/3109890/data-analytics/these-r-packages-import-sports-weather-stock-data-and-more.html
# look at rnoaa especially for correlation with the tides

#Notes on 17
#1 Almost no big days until 2nd weekend of April and hen it's Fri, Sat, Sun
#2 Weekdays start getting consistently big after Memorial Day
#3 Weekdays slow down after Labor Day
#4 October weekdays even slower than Sept.
#5 Beginning around Halloween Saturday is really the main big weekend day until the week before Xmas and except for Thanksgiving weekend
                              
                                 




#data17.jan.agg$Date.Sold <- as_date(data17.jan.agg$Date.Sold)
#data17.jan.aggregate <- aggregate(Total.Sales ~ Date.Sold.Round + Date.Sold, data17.jan.agg, sum)
#data17.calendar <- data17.jan.aggregate %>%
  #group_by(Date.Sold.Round) %>%
#  mutate("Hour" = as.integer(hour(Date.Sold.Round))) %>%
#  frame_calendar(x = Hour, y = Total.Sales, date = Date.Sold, calendar = "monthly") %>%
#  ggplot(aes(x = .Hour, y = .Total.Sales, group = Date.Sold)) + geom_line()