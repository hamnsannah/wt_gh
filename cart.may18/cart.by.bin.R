cart.by.bin <- function(dataset, bin.max, hier= "Department", items.to.explore = 10){
  
  require(knitr)
  require(dplyr)
  data.agg <- aggregate(Total.Sales ~ Transaction, dataset, sum)
  print(dim(data.agg))
  transactions.in.bin <- filter(data.agg, Total.Sales > (bin.max-5), Total.Sales <= bin.max)
  print(head(transactions.in.bin, 20))
  
  bin.data <- filter(dataset, Transaction %in% transactions.in.bin$Transaction) # all transactions in $5 bin
  print(dim(bin.data))
  
  bin.data$full.item <- paste0(bin.data$Description, " by ", bin.data$Supplier, " (", bin.data$Item, ")")
  print(head(bin.data))
  bin.winners <- aggregate(Qty.Sold ~ full.item + Category + Department, bin.data, sum)
  bin.winners <- arrange(bin.winners, desc(Qty.Sold))
  
  print(head(bin.winners, 20))
  
  for(i in 1:items.to.explore){
    #print(paste("i equals", i))
    bin.winner.i <- bin.winners[i,1]
    #print(paste("everything linked with", bin.winner.i))
    winner.i.trans <- bin.data %>%
      filter(full.item == bin.winner.i) %>%
      select(Transaction)
    #print(head(winner.i.trans))
    #print(class(winner.i.trans))
    #print(dim(winner.i.trans))
    paired.w.i <- bin.data %>%
      filter(Transaction %in% winner.i.trans[,1]) %>%
      filter(!full.item %in% bin.winner.i) # removes original item
    
    if(nrow(paired.w.i) > 0){
      paired.test <- select(paired.w.i, Transaction, full.item, Total.Sales, Date.Sold, Qty.Sold, Department) %>%
        arrange(Transaction)
      #print(paired.test)
      #print("THIS ONE:")
      #print(str(paired.test))
      
      if(hier == "Department"){
        top.pairing.i <- aggregate(Qty.Sold ~ Department, paired.w.i, sum)  
      } else if(hier == "Category"){
        top.pairing.i <- aggregate(Qty.Sold ~ Category, paired.w.i, sum)
      } else {
        top.pairing.i <- aggregate(Qty.Sold ~ full.item, paired.w.i, sum)
      }
      
      top.pairing.i <- arrange(top.pairing.i, desc(Qty.Sold))

      sentence <- paste0(bin.winners[i,1], ", in ", bin.winners[i,2], " within ", bin.winners[i,3], " was sold ", 
                   bin.winners[i,4], " times and was most often paired with these:")
      df <- "Pairing for Top Item" = sentence
      kable(df)
      
      #print("was sold")
      #print(bin.winners[i,4])
      #print("times and was most paired with these")
      #print(head(top.pairing.i, 20))
      kable(head(top.pairing.i, 20))
    }else{
      "nothing paired with it"
    }
  }
  
}