fake.data.wt.apps <- function(){
  fake.df <- data.frame("Department" = 0, "Category" = 0, "Supplier" = 0, "Item" = 0, "Description" = 0, "On.Hand" = 0, "Price" = 0, "Last.Sold" = 0,
                        "Qty.Sold"= 0, "Sold.Price" = 0, "Total.Sales" = 0, "Transaction" = 0, "Date.Sold" = 0, "Cost" = 0, "Profit" = 0, "Profit.Margin" = 0,
                        "Register" = 0, "Cashier" = 0, "Year" = 0, "Month" = 0, "Dept.By.Year" = 0, "Categ.By.Year" = 0, "Date.Sold.Round" = 0)

  
#  = c("GIFTS", "TOYS", "HOME", "BODY", "OTHER")
#  = c("Category 1", "Category 2", "Category 3")
  fake.df
}


# start here

fake.data4years.wide <- data4years.wide
fake.data4years.wide <- select(fake.data4years.wide, Department:Sold.Price,Transaction:Date.Sold.Round)
fake.data4years.wide$Total.Sales <- rpois(nrow(fake.data4years.wide), 100)
unique(fake.data4years.wide$Department)

fake.data4years.wide[fake.data4years.wide$Department %in% c("COUNTER CARD", "PAPER GOODS", "POSTCARDS","TOYS", "BATH/ACCESSORIES"), 1] <- "WIDGETS"
fake.data4years.wide[fake.data4years.wide$Department %in% c("GENERAL MERCHANDISE", "JEWELRY", "ORNAMENTS", "BOOKS", "HOLIDAY"), 1] <- "FIDGETS"
fake.data4years.wide[fake.data4years.wide$Department %in% c("SHELLS", "MISCELLANEOUS", "GIFT CARD", "MISCELLANEOUS PAPER", "POSTAGE", "", "DISPLAY", "CHRISTMAS"), 1] <- "ZIDGETS"

fake.widgets <- filter(fake.data4years.wide, Department == "WIDGETS")
fake.widgets$Category2 <- c(rep(c("RED WIDGETS", "BLUE WIDGETS", "GREEN WIDGETS"), (nrow(fake.widgets)/3)), "RED WIDGETS")

fake.fidgets <- filter(fake.data4years.wide, Department == "FIDGETS")
fake.fidgets$Category2 <- c(rep(c("RED FIDGETS", "BLUE FIDGETS", "GREEN FIDGETS"), (nrow(fake.fidgets)/3)), "RED FIDGETS", "BLUE FIDGETS")
fake.zidgets <- filter(fake.data4years.wide, Department == "ZIDGETS")
fake.zidgets$Category2 <- c(rep(c("RED ZIDGETS", "BLUE ZIDGETS", "GREEN ZIDGETS"), (nrow(fake.zidgets)/3)), "RED ZIDGETS")


fake.widgets$Supplier <- c(rep(c("The Red Widget Group", "Blue Widget Corp", "Green Widget Machine", "We Are Red Widgets", "Blue Like Widgets", "Lean Green Widgets", "Red Widgets Unlimted", "Blue Widgets Smiling", "The Widget is Always Greener"), (nrow(fake.widgets)/9)), "The Red Widget Group", "Blue Widget Corp", "Green Widget Machine", "We Are Red Widgets")
fake.fidgets$Supplier <- c(rep(c("The Red Fidget Group", "Blue Fidget Corp", "Green Fidget Machine", "We Are Red Fidgets", "Blue Like Fidgets", "Lean Green Fidgets", "Red Fidgets Unlimted", "Blue Fidgets Smiling", "The Fidget is Always Greener"), (nrow(fake.fidgets)/9)), "The Red Fidget Group", "Blue Fidget Corp")
fake.zidgets$Supplier <- c(rep(c("The Red Zidget Group", "Blue Zidget Corp", "Green Zidget Machine", "We Are Red Zidgets", "Blue Like Zidgets", "Lean Green Zidgets", "Red Zidgets Unlimted", "Blue Zidgets Smiling", "The Zidget is Always Greener"), (nrow(fake.zidgets)/9)), "The Red Zidget Group", "Blue Zidget Corp", "Green Zidget Machine", "We Are Red Zidgets", "Blue Like Zidgets", "Lean Green Zidgets", "Red Zidgets Unlimted")

fake.wt.fixed <- rbind(fake.widgets, fake.fidgets, fake.zidgets)

fake.wt.fixed$Dept.By.Year <- paste(fake.wt.fixed$Department, fake.wt.fixed$Year)
fake.wt.fixed$Categ.By.Year <- paste(fake.wt.fixed$Category2, fake.wt.fixed$Year)
fake.wt.fixed$Category <- fake.wt.fixed$Category2
fake.wt.fixed <- select(fake.wt.fixed, Department:Total.Sales)


library(dplyr)
#fake.data4years.wide <- select(fake.data4years.wide, Department:Sold.Price,Transaction:Date.Sold.Round)
#fake.data4years.wide$Total.Sales <- rpois(nrow(fake.data4years.wide), 100)


Suppliers 

#c("The Red Widget Group", "Blue Widget Corp", "Green Widget Machine", "We Are Red Widgets", "Blue Like Widgets", "Lean Green Widgets", "Red Widgets Unlimted", "Blue Widgets Smiling", "The Widget is Always Greener")

#c("The Red Fidget Group", "Blue Fidget Corp", "Green Fidget Machine", "We Are Red Fidgets", "Blue Like Fidgets", "Lean Green Fidgets", "Red Fidgets Unlimted", "Blue Fidgets Smiling", "The Fidget is Always Greener")

#c("The Red Zidget Group", "Blue Zidget Corp", "Green Zidget Machine", "We Are Red Zidgets", "Blue Like Zidgets", "Lean Green Zidgets", "Red Zidgets Unlimted", "Blue Zidgets Smiling", "The Zidget is Always Greener")
