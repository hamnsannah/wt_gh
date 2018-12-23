ab.shiny.input <- function(selected.column, value.in.column, output.metric = "Total.Sales", start.date, end.date, data.object.prepped){
  require(lubridate)
  #column possibilities: Department, Category, Supplier, Item, Register, Cashier
  #y possibilities: Total.Sales, weighted profit margin (use Total.Sales - Cost)/Total.Sales, gross profit, daily avg cart size
  data.object <- data.object.prepped
  #data.both <- rbind(data17, data16)
  data.object$Total.Sales <- as.numeric(data.object$Total.Sales)
  data.object <- filter(data.object, !is.na(Total.Sales)) #removes the sums that had $ signs in them
  print(paste("sum of Total.Sales is", sum(data.object$Total.Sales)))
  data.object$Date.Sold <- mdy_hm(data.object$Date.Sold)
  data.object$Date <- as_date(data.object$Date.Sold)
  data.object$Year <- year(data.object$Date.Sold)
  data.object$Month <- month(data.object$Date.Sold)
  data.object$Department <- trimws(data.object$Department)
  data.object$Category <- trimws(data.object$Category)
  data.object$Supplier <- trimws(data.object$Supplier)
  data.object$Item <- trimws(data.object$Item)
  data.object$Year.Month <- as.numeric(paste0(substr(data.object$Year,3,4), data.object$Month))
  data.object$Year.Month <- as.factor(data.object$Year.Month)
  print(unique(data.object$Department))
  column.data <- data.object[colnames(data.object) == selected.column]
  narrow.data <- cbind(column.data, select(data.object, Date, Total.Sales, Cost, Profit))
  #narrow.date.defined <- filter(narrow.data, Date >= start.date, Date <= end.date)
  #print(head(narrow.date.defined, 25))
  narrow.data
  
  #next steps
  # thinking that best way to allow for 3 output is to actually compute all three to make it easy to toggle
    # also unsure if I'll be able to use aggregate() with a variable defining the columns and this gets around that by...
      #aggregating all 3 and then subsetting the one I want (which can easily be done with a variable)
}