line.graph.wt <- function(name, hierarchy = "Supplier", data.object.mutated){
  require(dplyr)
  require(ggplot2)
  require(lubridate)
  #require(viridis)
  
  data.both <- data.object.mutated
  supplier.time.agg <- aggregate(Total.Sales ~ Year + Month + Supplier, data.both, sum)
  supplier.time.agg$Year <- as.factor(supplier.time.agg$Year)
  supplier.time.agg$Month <- as.factor(supplier.time.agg$Month)
  
  supplier.t <- supplier.time.agg[supplier.time.agg$Supplier == name,]
  supp.plot.t <- ggplot(supplier.t, aes(x=Month,y=Total.Sales, group = Year)) +
    geom_line(aes(color = Year), size = 2) + ggtitle(paste("Trend Year Over Year for", supplier.t$Supplier[1]))+
    #scale_color_viridis(discrete = TRUE)
    #scale_color_manual(values=c("blue", "red"))+
    scale_color_brewer(palette= "RdBu") +
    #scale_color_manual(values=c("#000066", "#FF0000"))
    theme_dark()
  print(supp.plot.t)
}