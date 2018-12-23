profit.calc.append <- function(combined.df, imported.retail.price.df){
  print("need df with both ingredients and phases which includes Cost.Div.Batch column")
  
  cost.agg <- aggregate(Cost.Div.Batch ~ Product + Batch.Size, combined.df, sum)
  print(cost.agg)
  
  price.df <- imported.retail.price.df
  print(str(price.df))

  cost.price <- merge(cost.agg, price.df, all.x=TRUE, all.y=FALSE)
  cost.price <- cost.price %>%
    mutate("Profit" = `Retail Price`-Cost.Div.Batch)
  print(cost.price)
  new.rows <- as.data.frame(cbind("Product" = as.character(cost.price$Product), "Batch.Size" = cost.price$Batch.Size, "Cost.Type" = "Profit", "Ing.or.Phase" = NA, "Quantity" = 1, "Unit" = "units", "Cost" = (cost.price$Profit*cost.price$Batch.Size), "Cost.Div.Batch" = cost.price$Profit))
  print(colnames(new.rows))
  combined.df <- rbind(combined.df, new.rows)

  }