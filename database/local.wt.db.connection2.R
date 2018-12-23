# this one is closest to Detailed Sales Report (comment 9/5/2018)

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
                                 JOIN Cashier ON [Transaction].CashierID = Cashier.ID', n=6)


JOIN [Transaction] ON TransactionEntry.TransactionNumber = [Transaction].TransactionNumber

, Cashier.Name AS "Cashier"

JOIN Transaction ON TransactionEntry.TransactionNumber = Transaction.TransactionNumber

JOIN Cashier ON Transaction.CashierID = Cashier.ID


item.dept.cat3 <- DBI::dbGetQuery(con,'
                                SELECT [Transaction].CashierID 
                                  
                                  FROM [Transaction]
                                  ', n=6)


