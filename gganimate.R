#####
##### THIS IS A BROKEN PACKAGE DON'T ATTEMPT TO FIX UNTIL IT'S ON CRAN AND WORKING
#####

#gganimate practice
mutated.data <- filter(mutated.four.years, Year == 2017)

peak.months <- c(6, 7, 8, 9)
mutated.peak <- mutated.data %>%
  filter(Month %in% peak.months)
mutated.peak.dept.agg <- aggregate(Total.Sales ~ Department, mutated.peak, sum)
colnames(mutated.peak.dept.agg) <- c("Department", "Peak.Sales")
mutated.peak.dept.agg$Peak.Sales <- mutated.peak.dept.agg$Peak.Sales/4
print(colnames(mutated.peak.dept.agg))
monthly.with.peaks <- data.frame()
for(i in 1:12){
  mutated.month.i <- mutated.data %>%
    filter(Month == i) 
  mutated.month.i.agg <- aggregate(Total.Sales ~ Department, mutated.month.i, sum)
  
  mutated.month.i.agg.merged <- merge(mutated.month.i.agg, mutated.peak.dept.agg, all.x = TRUE, all.y = TRUE)
  mutated.month.i.agg.merged$Month <- i
  mutated.month.i.agg.merged <- filter(mutated.month.i.agg.merged, Department != "")
  #print(head(mutated.month.i.agg.merged))
  monthly.with.peaks <- rbind(monthly.with.peaks, mutated.month.i.agg.merged)
}
monthly.with.peaks[is.na(monthly.with.peaks)] <- 0
monthly.with.peaks <- monthly.with.peaks %>%
  mutate("Perc.From.Peak" = ((Total.Sales - Peak.Sales)/Peak.Sales)*100)
monthly.with.peaks[is.nan(monthly.with.peaks$Perc.From.Peak),5] <- -1
monthly.no.christmas <- filter(monthly.with.peaks, Department != "CHRISTMAS")
monthly.no.christmas.janmay <- filter(monthly.no.christmas, Month %in% 1:5)

library(gganimate)
#monthly.no.christmas.janmay <- filter(monthly.no.christmas, Month %in% 1:5)
depts5 <- ggplot(data = monthly.no.christmas.janmay, aes(x = Total.Sales, y = Perc.From.Peak, col = Department, size = Peak.Sales, frame = Month)) + geom_point()
gganimate(depts5)