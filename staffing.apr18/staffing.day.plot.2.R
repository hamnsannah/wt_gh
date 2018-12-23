
# input is the dataset but uses parameters to subset
# orgnize data frame so that there are separate vectors of hourly data in the subset
  # use a function that covers the whole hour so that we can look at "the 9 o'clock hour"
# do a loop that runs each of the vectors through 3 t.tests (drawing 2 outputs from the first)
  # the 95% confidence, 75% confidence, and 50% confidence, and the estimate
# use 2 years worth of data?

# use geom_ribbon like in ab.test to plot it

staffing.day.plot.2 <- function(df,nickname.of.time.range){
  
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  
  df.agg <- aggregate(Total.Sales ~ Date.Sold.Round + Hour + Weekday, df.all, sum)
  conf.df <- data.frame()
  
  for(i in 9:23){
    print(i)
    i.hour.vec <- df.agg %>%
      filter(Hour == i) %>%
      select(Total.Sales)
    i.estimate.95 <- t.test(i.hour.vec, conf.level = 0.95)$estimate
    i.conf.int.95 <- t.test(i.hour.vec, conf.level = 0.95)$conf.int
    i.conf.int.75 <- t.test(i.hour.vec, conf.level = 0.75)$conf.int
    i.conf.int.50 <- t.test(i.hour.vec, conf.level = 0.50)$conf.int
    three.conf.int <- c(i, i.estimate.95, i.conf.int.95, i.conf.int.75, i.conf.int.50)
    
    conf.df <- rbind(conf.df, three.conf.int)
    
  }
    colnames(conf.df) <- c("hour", "estimate", "low95", "high95", "low75", "high75", "low50", "high50")
    
    day.plot <- ggplot(data = conf.df, aes(x=hour)) +
      geom_ribbon(aes(ymin=low95, ymax=high95), fill="#0571b0", alpha = .75)+
      geom_ribbon(aes(ymin=low75, ymax=high75), fill="#92c5de", alpha = .75)+
      geom_ribbon(aes(ymin=low50, ymax=high50), fill="#d1e5f0", alpha = .75)+
      geom_ribbon(aes(ymin=estimate-5, ymax=estimate+5), fill="#ef8a62")
    print(day.plot)
}

# end of function ####
