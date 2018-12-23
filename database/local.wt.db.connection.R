library(odbc)
library(DBI)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = "LAPTOP-JHRVTF04\\SQLEXPRESS",
                      Database = "WHALESTALE",
                      Trusted_Connection = "True")

# Remember to first open SQL Server Configuration Manager and turn on the database before accessing, if not working right away

table.test <- DBI::dbGetQuery(con,'
                              SELECT "ItemID","Price", "TransactionTime"
                              FROM "TransactionEntry"',
                              n=6
                              )

item.query <- DBI::dbGetQuery(con,'
                              SELECT "ItemID",TransactionEntry.Price, "TransactionTime", Item.Description
                             FROM "TransactionEntry"
                              JOIN "Item"
                              ON TransactionEntry.ItemID = Item.ID',
                             n=6
)

item.dept.cat <- DBI::dbGetQuery(con,'
                                 SELECT TransactionEntry.ItemID, TransactionEntry.Price, 
                                 TransactionEntry.TransactionTime, Item.Description, 
                                  Department.Name AS "Department", Category.Name AS "Category", SupplierName AS "Supplier"
                                 FROM TransactionEntry
                                 JOIN Item ON TransactionEntry.ItemID = Item.ID
                                 JOIN Department ON Item.DepartmentID = Department.ID
                                 JOIN Category ON Item.CategoryID = Category.ID
                                 JOIN Supplier ON Item.SupplierID = Supplier.ID', n=6)