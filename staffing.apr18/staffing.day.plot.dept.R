# DO NOT USE, SEEMS TO BE GENERATING REPORT WITH T-TEST WHICH IS NO LONGER THE APPROACH: USING QUANTILES 


# input is the dataset but uses parameters to subset
# orgnize data frame so that there are separate vectors of hourly data in the subset
  # use a function that covers the whole hour so that we can look at "the 9 o'clock hour"
# do a loop that runs each of the vectors through 3 t.tests (drawing 2 outputs from the first)
  # the 95% confidence, 75% confidence, and 50% confidence, and the estimate
# use 2 years worth of data?

# use geom_ribbon like in ab.test to plot it

staffing.day.plot.dept <- function(df,nickname.of.time.range, palette.color = "green.palette"){
  
  green.palette <- c("#238b45", "#74c476", "#bae4b3")
  red.palette <- c("#cb181d", "#fb6a4a", "#fcae91")
  
  if(palette.color == "green.palette"){
    dark.color = green.palette[1]
    medium.color = green.palette[2]
    light.color = green.palette[3]
  }else if(palette.color == "red.palette"){
    dark.color = red.palette[1]
    medium.color = red.palette[2]
    light.color = red.palette[3]
  }
  
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  
  df.agg <- aggregate(Total.Sales ~ Date.Sold.Round + Hour + Weekday, df.all, sum)
  conf.df <- data.frame()
  
  for(i in 9:23){
    #print(i)
    i.hour.vec <- df.agg %>%
      filter(Hour == i) %>%
      select(Total.Sales)
    #i.hour.vec <- as.double(i.hour.vec)
    #print(head(i.hour.vec))
    #i.estimate.95 <- t.test(i.hour.vec, conf.level = 0.95)$estimate
    #i.conf.int.95 <- t.test(i.hour.vec, conf.level = 0.95)$conf.int
    #i.conf.int.75 <- t.test(i.hour.vec, conf.level = 0.75)$conf.int
    #i.conf.int.50 <- t.test(i.hour.vec, conf.level = 0.50)$conf.int
    #three.conf.int <- c(i, i.estimate.95, i.conf.int.95, i.conf.int.75, i.conf.int.50)
    quantiles <- quantile(as.numeric(unlist(i.hour.vec)), probs = c(.5, .025, .975, .125, .875, .25, .75))
    #print(class(quantiles))
    
    quantiles.hour <- c(i, quantiles)
    #print(class(quantiles.hour))
    conf.df <- rbind(conf.df, quantiles.hour)
    
    #i.estimate.95 <- quantie(i.hour.vec, .5)
    #i.conf.int.95.high <- 
    #quantiles.hour <- c(i, quantile(i.hour.vec, c(.5, .025, .975, .125, .875, .25, .75)))
  }
  colnames(conf.df) <- c("hour", "estimate", "low95", "high95", "low75", "high75", "low50", "high50")
  
  #lbls = paste0(as.character(c(seq(9, 12, 1), seq(1, 11, 1))), c(rep("am", 3), rep("pm", 11)))
  #brks <- seq(9,23, 1)
  
  #lbls.y = paste0("$", seq(0, 20000, 2000))
  #brks.y = seq(0, 20000, 2000)
  day.plot <- 0
  day.plot <- ggplot(data = conf.df, aes(x=hour)) +
    geom_ribbon(aes(ymin=low95, ymax=high95), fill=dark.color, alpha = .75)+
    geom_ribbon(aes(ymin=low75, ymax=high75), fill=medium.color, alpha = .75)+
    geom_ribbon(aes(ymin=low50, ymax=high50), fill=light.color, alpha = .75)+
    geom_ribbon(aes(ymin=estimate-50, ymax=estimate+50), fill="black") 
  #day.plot <- day.plot + scale_x_continuous(labels = lbls, breaks = brks) #+ 
    #scale_y_continuous(labels = lbls.y, breaks = brks.y) #+ ylim(0,2000)
  print(day.plot)
}
