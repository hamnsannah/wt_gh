holiday.card.ridges.R
library(ggplot2)
library(lubridate)
library(ggridges)
library(dplyr)
sales.data <- read.csv("data/sales.data.cache2019-01-19.csv", stringsAsFactors = FALSE)
card.data <- filter(sales.data, Department == "COUNTER CARD")
card.cats18 <- filter(card.data, Year == 2018)
seasonal.cats <- c(T, F, F, F, F, F, F, F, F, F, F, T, F, F, F, T, F, F, F, F, T, F, F, F, T, T, T, T, F, T, F, T, T, F, F, F, T, T, F, F, T, F, T, F, F, F)
# built off of the 2018 unique category names, of which there were 46
card.cats18 <- arrange(card.cats18, Category)
uniq.cats <- unique(card.cats18$Category)
seasonal.catnames <- uniq.cats[seasonal.cats]

#monthly data (not chosen in favor of daily but may prove useful or preferable)
#card.data18 <- filter(card.data, Year == 2018)
#card.ridges18 <- aggregate(Total.Sales ~ Month + Category, card.data18, sum)
#card.ridges18 <- aggregate(Total.Sales ~ Month + Category, card.data18, sum)
#card.ridges18 <- filter(card.ridges18, Category %in% seasonal.catnames)
#gg <- ggplot(data = card.ridges18, aes(x = Month, y = as.factor(Category), height = Total.Sales, group = as.factor(Category))) + geom_density_ridges(stat = "identity")
#gg
###


#trying daily rather than monthly
card.data18daily <- card.cats18
card.data18daily$Date <- date(card.data18daily$Date.Sold)
card.ridges18daily <- aggregate(Total.Sales ~ Category + Date, card.data18daily, sum)
#seasonal.catnames <- seasonal.catnames$Category
card.ridges18daily <- filter(card.ridges18daily, Category %in% seasonal.catnames)
ggdaily <- ggplot(data = card.ridges18daily, aes(x = Date, y = as.factor(Category),  height = Total.Sales, group = as.factor(Category))) + geom_density_ridges(fill = "light blue", stat = "identity")
ggdaily <- ggdaily + theme_dark() #+ scale_x_date(breaks = 13, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "J"))
#ggdaily <- ggdaily + scale_fill_distiller(direction = -1)
ggdaily
