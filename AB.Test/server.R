# AB Test Shiny

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("D://Users/SPritchard/Music/Documents/R/allocate/whaletale/")
#read in data
data4years <- read.csv("mutated.data.14.17decpart.csv", stringsAsFactors = FALSE)
#data4years <- read.csv("/srv/shiny-server/supplier-dash/four.years.data.csv", stringsAsFactors = FALSE)

depts <- unique(trimws(data4years$Department))
categories <- unique(trimws(data4years$Category))
items <- unique(trimws(data4years$Item))
suppliers <- unique(trimws(data4years$Supplier))
#No options yet in ui.R for these 2
register <- unique(trimws(data4years$Register))
cashier <- unique(trimws(data4years$Cashier))
print("made it this far")

#read in functions
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

t.test.arrow.blue <- function(data.a.baseline, data.b.comparison){
  
  conf.df <- data.frame()
  
  for(i in 1:length(data.a.baseline)){
    b.comp <- data.b.comparison[1:(i+1)]
    a.base <- data.a.baseline[1:(i+1)]
    conf.int95.i <- t.test(b.comp, a.base, conf.level = 0.95)$conf.int
    conf.int85.i <- t.test(b.comp, a.base, conf.level = 0.85)$conf.int
    conf.int75.i <- t.test(b.comp, a.base, conf.level = 0.75)$conf.int
    three.conf.int.i <- c(conf.int95.i, conf.int85.i, conf.int75.i)
    conf.df <- rbind(conf.df, three.conf.int.i)
  }
  conf.df$deg.free <- seq(nrow(conf.df))
  colnames(conf.df) <- c("low95", "high95", "low85", "high85", "low75", "high75", "deg.free")
  
  # create dark blue arrow down
  
  arrow.dblue.down <- ggplot(data=data.frame("x"= 1, "y"=1)) +
    theme(panel.background = element_rect(fill = "#FFFFFF", color="black"), 
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) + 
    xlim(0,2) +
    ylim(0,2) +
    ggtitle("B is lower than A with 95% confidence") +
    geom_segment(x=1, xend=1, y=0, yend=2, color="#6baed6", size=2) +
    geom_segment(x=1, xend=1.5, y=0, yend=.5, color="#6baed6", size=2) +
    geom_segment(x=0.5, xend=1, y=0.5, yend=0, color="#6baed6", size=2)
  
  # create dark blue arrow up
  
  arrow.dblue.up <- ggplot(data=data.frame("x"= 1, "y"=1)) +
    theme(panel.background = element_rect(fill = "#FFFFFF", color="black"), 
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) + 
    xlim(0,2) +
    ylim(0,2) +
    ggtitle("B is higher than A with 95% confidence") +
    geom_segment(x=1, xend=1, y=0, yend=2, color="#6baed6", size=2) +
    geom_segment(x=1, xend=1.5, y=2, yend=1.5, color="#6baed6", size=2) +
    geom_segment(x=0.5, xend=1, y=1.5, yend=2, color="#6baed6", size=2)
  
  # create medium blue arrow down
  
  arrow.mblue.down <- ggplot(data=data.frame("x"= 1, "y"=1)) +
    theme(panel.background = element_rect(fill = "#FFFFFF", color="black"), 
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) + 
    xlim(0,2) +
    ylim(0,2) +
    ggtitle("B is lower than A with 85% confidence") +
    geom_segment(x=1, xend=1, y=0, yend=2, color="#bdd7e7", size=2) +
    geom_segment(x=1, xend=1.5, y=0, yend=.5, color="#bdd7e7", size=2) +
    geom_segment(x=0.5, xend=1, y=0.5, yend=0, color="#bdd7e7", size=2)
  
  # create medium blue arrow up
  
  arrow.mblue.up <- ggplot(data=data.frame("x"= 1, "y"=1)) +
    theme(panel.background = element_rect(fill = "#FFFFFF", color="black"), 
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) + 
    xlim(0,2) +
    ylim(0,2) +
    ggtitle("B is higher than A with 85% confidence") +
    geom_segment(x=1, xend=1, y=0, yend=2, color="#bdd7e7", size=2) +
    geom_segment(x=1, xend=1.5, y=2, yend=1.5, color="#bdd7e7", size=2) +
    geom_segment(x=0.5, xend=1, y=1.5, yend=2, color="#bdd7e7", size=2)
  
  # create light blue arrow down
  
  arrow.lblue.down <- ggplot(data=data.frame("x"= 1, "y"=1)) +
    theme(panel.background = element_rect(fill = "#FFFFFF", color="black"), 
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) + 
    xlim(0,2) +
    ylim(0,2) +
    ggtitle("B is lower than A with 75% confidence") +
    geom_segment(x=1, xend=1, y=0, yend=2, color="#eff3ff", size=2) +
    geom_segment(x=1, xend=1.5, y=0, yend=.5, color="#eff3ff", size=2) +
    geom_segment(x=0.5, xend=1, y=0.5, yend=0, color="#eff3ff", size=2)
  
  # create light blue arrow up
  
  arrow.lblue.up <- ggplot(data=data.frame("x"= 1, "y"=1)) +
    theme(panel.background = element_rect(fill = "#FFFFFF", color="black"), 
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) + 
    xlim(0,2) +
    ylim(0,2) +
    ggtitle("B is higher than A with 75% confidence") +
    geom_segment(x=1, xend=1, y=0, yend=2, color="#eff3ff", size=2) +
    geom_segment(x=1, xend=1.5, y=2, yend=1.5, color="#eff3ff", size=2) +
    geom_segment(x=0.5, xend=1, y=1.5, yend=2, color="#eff3ff", size=2)
  
  # print a question mark
  q.mark <- ggplot(data=data.frame("x"= 1, "y"=1)) +
    xlim(0,2) +
    ylim(0,2) +
    ggtitle("B is neither higher nor lower than A per available data") +
    geom_text(aes(x=x, y=y,label="?"), size=50) +
    theme(panel.background = element_rect(fill = "#FFFFFF", color="black"), 
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          text = element_blank()) +
    ggtitle("B is neither higher nor lower than A per available data")
  
  # determine which arrow should be printed
  
  if(tail(conf.df$low95,1) <0 & 
     tail(conf.df$high95,1)<0){
    print(arrow.dblue.down)
    arrow.plot <- arrow.dblue.down
  } else if(tail(conf.df$low95,1) >0 & 
            tail(conf.df$high95,1)>0){
    print(arrow.dblue.up)
    arrow.plot <- arrow.dblue.up
  } else if(tail(conf.df$low85,1) <0 & 
            tail(conf.df$high85,1)<0){
    print(arrow.mblue.down)
    arrow.plot <- arrow.mblue.down
  } else if(tail(conf.df$low85,1) >0 & 
            tail(conf.df$high85,1)>0){
    print(arrow.mblue.up)
    arrow.plot <- arrow.mblue.up
  } else if(tail(conf.df$low75,1) <0 & 
            tail(conf.df$high75,1)<0){
    print(arrow.lblue.down)
    arrow.plot <- arrow.lblue.down
  } else if(tail(conf.df$low75,1) >0 & 
            tail(conf.df$high75,1)>0){
    print(arrow.lblue.up)
    arrow.plot <- arrow.lblue.up
  } else {
    print(q.mark)
    arrow.plot <- q.mark
  }
  #to avoid any unintended output
  print("see plot for arrow direction and color, or question mark if difference can't be conclusively calculated")
  print("dark blue means 95% confidence, medium blue means 85% confidence, light blue means 75% confidence")
  
  arrow.plot
}

ab.trail <- function(data.a.baseline, data.b.comparison){
  
  conf.df <- data.frame()
  
  for(i in 1:length(data.a.baseline)){
    b.comp <- data.b.comparison[1:(i+1)]
    a.base <- data.a.baseline[1:(i+1)]
    
    a.estimate95.i <- t.test(a.base, conf.level = 0.95)$estimate
    a.conf.int95.i <- t.test(a.base, conf.level = 0.95)$conf.int
    a.conf.int85.i <- t.test(a.base, conf.level = 0.85)$conf.int
    a.conf.int75.i <- t.test(a.base, conf.level = 0.75)$conf.int
    
    b.estimate95.i <- t.test(b.comp, conf.level = 0.95)$estimate
    b.conf.int95.i <- t.test(b.comp, conf.level = 0.95)$conf.int
    b.conf.int85.i <- t.test(b.comp, conf.level = 0.85)$conf.int
    b.conf.int75.i <- t.test(b.comp, conf.level = 0.75)$conf.int
    three.conf.int.i <- c(a.estimate95.i, a.conf.int95.i, a.conf.int85.i, a.conf.int75.i, 
                          b.estimate95.i, b.conf.int95.i, b.conf.int85.i, b.conf.int75.i)
    conf.df <- rbind(conf.df, three.conf.int.i)
  }
  conf.df$deg.free <- seq(nrow(conf.df))
  colnames(conf.df) <- c("a.estimate", "a.low95", "a.high95", "a.low85", "a.high85", "a.low75", "a.high75", 
                         "b.estimate", "b.low95", "b.high95", "b.low85", "b.high85", "b.low75", "b.high75", "deg.free")
  print(head(conf.df))
  three.plot <- ggplot(conf.df, aes(x=deg.free)) +
    #geom_point(aes(y=low95))+
    #geom_point(aes(y=high95))+
    #A plot in blues
    geom_ribbon(aes(ymin=a.low95, ymax=a.high95), fill="#6baed6", alpha = .6)+
    #geom_ribbon(aes(ymin=a.low85, ymax=a.high85), fill="#bdd7e7", alpha = .6)+
    geom_ribbon(aes(ymin=a.low75, ymax=a.high75), fill="#eff3ff", alpha = .6)+
    geom_ribbon(aes(ymin=(a.estimate-5), ymax=(a.estimate+5)), fill="#2171b5")+
    
    #B plot in reds
    geom_ribbon(aes(ymin=b.low95, ymax=b.high95), fill="#fb6a4a", alpha = .6)+
    #geom_ribbon(aes(ymin=b.low85, ymax=b.high85), fill="#fcae91", alpha = .6)+
    geom_ribbon(aes(ymin=b.low75, ymax=b.high75), fill="#fee5d9", alpha = .6)+
    geom_ribbon(aes(ymin=(b.estimate-5), ymax=(b.estimate+5)), fill="#cb181d")+
    #geom_segment(x=0, xend=length(data.a.baseline), y=0, yend=0, color="yellow", size=2) +
    labs(x= "Days of Sales Included", y="Sales Per Day for Selections",
         title="A/B Test Progress with 95% & 75% Confidence Intervals",
         subtitle="The baseline A is represented by the blue trail and the comparison B by the red trail \n There is a statistically significant difference between A & B when separation appears between the two trails")
  #print(three.plot)
  three.plot
}

#shiny app
shinyServer(
  function(input, output){
    # create choices for Hierarchy for A
    output$grouping.values.a <- renderUI({
      colname <- input$ida1
      if(colname == "Department"){grouping.ops <- depts} else
        if(colname == "Category"){grouping.ops <- categories} else
          if(colname == "Item"){grouping.ops <- items} else
            if(colname == "Supplier"){grouping.ops <- suppliers} else
            if(colname == "Cashier"){grouping.ops <- cashier} else
              if(colname == "Register"){grouping.ops <- register}
      
      selectInput('ida2', 'Specific One', choices = grouping.ops, selected = "BIRTHDAY")
    })
    
    # select choices for hierarchy for B
    
    output$grouping.values.b <- renderUI({
      print("broadcasting from inside renderUI")
      colname <- input$idb1
      if(colname == "Department"){grouping.ops <- depts} else
        if(colname == "Category"){grouping.ops <- categories} else
          if(colname == "Item"){grouping.ops <- items} else
            if(colname == "Supplier"){grouping.ops <- suppliers} else
              if(colname == "Cashier"){grouping.ops <- cashier} else
                if(colname == "Register"){grouping.ops <- register}
      
      #menu.facility.list <- unique(narrow.df.for.menu[narrow.df.for.menu$Market %in% market2, 2])
      #menu.facility.list <- menu.facility.list[order(menu.facility.list)]
      #print(menu.facility.list)
      selectInput('idb2', 'Specific One', choices = grouping.ops, selected = "BIRTHDAY")
    })
    
    print("made it to 290")
    #### use the scripts ####

    ### create arrow and build in all necessary reactivity
    output$arrow <- renderPlot({
      
    grouping.a <- input$ida1
    hierarchy.value.a <- input$ida2
    if(is.null(hierarchy.value.a)){
      hierarchy.value.a <- "BIRTHDAY"
    }

    start.date.a <- input$ida3
    end.date.a <- input$ida4
    season.value <- input$id.season

    a.vec <- ab.shiny.subsetting(grouping = grouping.a, hierarchy.value = hierarchy.value.a, 
                                 start.date = start.date.a, end.date = end.date.a, 
                                 output.metric = "Total Sales", output.ab.input = data4years, seasonality = season.value)
    print(paste("A vec begins", head(a.vec)))
    
    ## subset for B
    grouping.b <- input$idb1
    hierarchy.value.b <- input$idb2
    print(input$idb2)
    if(is.null(hierarchy.value.b)){
      hierarchy.value.b <- "BIRTHDAY"
    }
    #hierarchy.value.b <- "BIRTHDAY"
    start.date.b <- input$idb3
    end.date.b <- input$idb4
    #output.ab.input.b <- data4years
    
    if(is.null(hierarchy.value.b)){
      Sys.sleep(1)
      hierarchy.value.b <- input$idb2
    }
    
    b.vec <- ab.shiny.subsetting(grouping = grouping.b, hierarchy.value = hierarchy.value.b, 
                                 start.date = start.date.b, end.date = end.date.b, 
                                 output.metric = "Total Sales", output.ab.input = data4years, seasonality = season.value)
    
    
    ## create ab.arrow
    t.test.arrow.blue(data.a.baseline = a.vec, data.b.comparison = b.vec)
    })
    
    
    ###### create ab.trail and all necessary reactivity ######
    
    output$trail <- renderPlot({
      grouping.a <- input$ida1
      hierarchy.value.a <- input$ida2
      if(is.null(hierarchy.value.a)){
        hierarchy.value.a <- "BIRTHDAY"
      }
      
      start.date.a <- input$ida3
      end.date.a <- input$ida4
      season.value <- input$id.season
      
      a.vec <- ab.shiny.subsetting(grouping = grouping.a, hierarchy.value = hierarchy.value.a, 
                                   start.date = start.date.a, end.date = end.date.a, 
                                   output.metric = "Total Sales", output.ab.input = data4years, seasonality = season.value)
      print(paste("A vec begins", head(a.vec)))
      
      ## subset for B
      grouping.b <- input$idb1
      hierarchy.value.b <- input$idb2
      print(input$idb2)
      if(is.null(hierarchy.value.b)){
        hierarchy.value.b <- "BIRTHDAY"
      }
      #hierarchy.value.b <- "BIRTHDAY"
      start.date.b <- input$idb3
      end.date.b <- input$idb4
      #output.ab.input.b <- data4years
      
      if(is.null(hierarchy.value.b)){
        Sys.sleep(1)
        hierarchy.value.b <- input$idb2
      }
      
      b.vec <- ab.shiny.subsetting(grouping = grouping.b, hierarchy.value = hierarchy.value.b, 
                                   start.date = start.date.b, end.date = end.date.b, 
                                   output.metric = "Total Sales", output.ab.input = data4years, seasonality = season.value)
      
    ab.trail(data.a.baseline = a.vec, data.b.comparison = b.vec)
    })
  }
)