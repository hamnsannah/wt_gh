sql.wrapper.wt.salesreport <- function(){
  
  library(odbc)
  library(DBI)
  library(dplyr)
  
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "LAPTOP-JHRVTF04\\SQLEXPRESS",
                        Database = "WHALESTALE",
                        Trusted_Connection = "True")
  print("connection made...")
  
  item.dept.cat2 <- DBI::dbGetQuery(con,'
                                SELECT TransactionEntry.ItemID, TransactionEntry.Price, 
                                    TransactionEntry.TransactionTime, TransactionEntry.Quantity, 
                                    TransactionEntry.TransactionNumber, Item.Description, 
                                    Department.Name AS "Department", Category.Name AS "Category", 
                                    SupplierName AS "Supplier", Cashier.Name AS "CashierName", 
                                    Register.Number AS "Register", Register.Description AS "RegLocation"
                                    
                                    FROM TransactionEntry
                                    
                                    JOIN Item ON TransactionEntry.ItemID = Item.ID
                                    JOIN Department ON Item.DepartmentID = Department.ID
                                    JOIN Category ON Item.CategoryID = Category.ID
                                    JOIN Supplier ON Item.SupplierID = Supplier.ID
                                    JOIN [Transaction] ON TransactionEntry.TransactionNumber = [Transaction].TransactionNumber
                                    JOIN Batch ON [Transaction].BatchNumber = Batch.BatchNumber
                                    JOIN Register ON Batch.RegisterID = Register.ID
                                    JOIN Cashier ON [Transaction].CashierID = Cashier.ID')
  print("data imported...")
  sql.df.mutate <- function(sql.df){
    
    sql.df <- sql.df %>%
      rename(Sold.Price = Price, Cashier = CashierName, Date.Sold = TransactionTime,
             Qty.Sold = Quantity, Transaction = TransactionNumber) %>%
      mutate(Total.Sales = Qty.Sold*Sold.Price) %>%
      select(Department, Category, Supplier, ItemID, Description, Qty.Sold, Sold.Price, 
             Total.Sales, Transaction, Date.Sold, Register, Cashier)
    
    sql.df$Date.Sold <- ymd_hms(sql.df$Date.Sold)
    sql.df$Year <- year(sql.df$Date.Sold)
    sql.df$Month <- month(sql.df$Date.Sold)
    sql.df$Department <- trimws(sql.df$Department)
    sql.df$Category <- trimws(sql.df$Category)
    sql.df$Supplier <- trimws(sql.df$Supplier)
    sql.df$Item <- trimws(sql.df$Item)
    #print(head(sql.df))
    sql.df$Dept.By.Year <- paste(sql.df$Department, sql.df$Year)
    sql.df$Categ.By.Year <- paste(sql.df$Category, sql.df$Year)
    sql.df
  }
  sql.df <- sql.df.mutate(item.dept.cat2)
  print("data mutated...")
  sql.df
  
}