
#this one is working but can't get TransactionTime working

item.dept.cat3 <- DBI::dbGetQuery(con,'
                                  SELECT TransactionEntry.ItemID, TransactionEntry.Price, 
                                  TransactionEntry.TransactionTime, TransactionEntry.Quantity, 
                                  TransactionEntry.TransactionNumber 
                                  FROM TransactionEntry
                                  WHERE TransactionEntry.Price > 50', n=60)

