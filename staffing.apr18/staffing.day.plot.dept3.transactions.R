# this script is to show transactions only for Splash instead of sales data

staffing.day.plot.dept3.transactions <- function(df, nickname.of.time.range, palette.color = "green.palette"){
  
  green.palette <- c("#238b45", "#74c476", "#bae4b3")
  red.palette <- c("#cb181d", "#fb6a4a", "#fcae91")
  purple.palette <- c("#6a51a3", "#9e9ac8", "#cbc9e2")
  orange.palette <- c("#d94701", "#fd8d3c", "#fdbe85")
  lavender.palette <- c("#ae017e", "#f768a1", "#fbb4b9")
  
  if(palette.color == "green.palette"){
    dark.color = green.palette[1]
    medium.color = green.palette[2]
    light.color = green.palette[3]
  }else if(palette.color == "red.palette"){
    dark.color = red.palette[1]
    medium.color = red.palette[2]
    light.color = red.palette[3]
  }else if(palette.color == "purple.palette"){
    dark.color = purple.palette[1]
    medium.color = purple.palette[2]
    light.color = purple.palette[3]
  }else if(palette.color == "orange.palette"){
    dark.color = orange.palette[1]
    medium.color = orange.palette[2]
    light.color = orange.palette[3]
  }else if(palette.color == "lavender.palette"){
    dark.color = lavender.palette[1]
    medium.color = lavender.palette[2]
    light.color = lavender.palette[3]
  }
  
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  
  df <- filter(df, Register == "9")
  df <- df[!duplicated(df$Transaction),] # one row per transaction
  
  df.agg <- aggregate(Total.Sales ~ Date.Sold.Round + Hour + Weekday, df, length) # unit switched to length
  conf.df <- data.frame()
  
  for(i in 9:23){
    #print(i)
    i.hour.vec <- df.agg %>%
      filter(Hour == i) %>%
      select(Total.Sales)

    quantiles <- quantile(as.numeric(unlist(i.hour.vec)), probs = c(.5, .025, .975, .125, .875, .25, .75))
    
    quantiles.hour <- c(i, quantiles)
    conf.df <- rbind(conf.df, quantiles.hour)

  }
  colnames(conf.df) <- c("hour", "estimate", "low95", "high95", "low75", "high75", "low50", "high50")
  
  # add column corresponding to # of staff needed based on $400 -> 2, $600 -> 3, etc
  conf.df$staff.estimate <- (round(conf.df$high50/200))
  
  lbls = as.character(c(seq(9, 12, 1), seq(1, 11, 1)))#, c(rep("am", 3), rep("pm", 11)))
  brks <- seq(9,23, 1)
  
  lbls.y = paste0("$", seq(0, 2000, 200))
  brks.y = seq(0, 2000, 200)
  brks.y2 = seq(0,8,1)
  
  day.plot <- ggplot(data = conf.df, aes(x=hour)) +
    geom_ribbon(aes(ymin=low95, ymax=high95), fill=dark.color, alpha = .75)+
    geom_ribbon(aes(ymin=low75, ymax=high75), fill=medium.color, alpha = .75)+
    geom_ribbon(aes(ymin=low50, ymax=high50), fill=light.color, alpha = .75)+
    geom_ribbon(aes(ymin=estimate-5, ymax=estimate+5), fill="black") + ylab("Transactions Per Hour")
    #geom_point(aes(y=staff.estimate*200), color="#FFFF33", size = 10) #+
  #scale_y_continuous(labels = scales::dollar)  
  #geom_text(aes(y = staff.estimate*200, label = staff.estimate), color = "black", size = 5, fontface = "bold")
  day.plot <- day.plot + scale_x_continuous(labels = lbls, breaks = brks) + 
    ggtitle(nickname.of.time.range)
    #scale_y_continuous(sec.axis = sec_axis(~./200, name = "Estimate of Hourly Staff Need", breaks = brks.y2), labels = lbls.y, breaks = brks.y, name = "Range of Hourly Sales") #+ ylim(0,2000)
  #print(head(conf.df, 15))
  #print(day.plot)
  day.plot

}