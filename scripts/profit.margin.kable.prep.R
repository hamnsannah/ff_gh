
profit.margin.kable.prep <- function(df.with.profit, pretty.nums = TRUE){
  require(knitr)
  df.with.profit$Cost.Div.Batch <- as.numeric(df.with.profit$Cost.Div.Batch)
  prof.agg <- aggregate(Cost.Div.Batch ~ Product + Cost.Type, df.with.profit, sum)
  
  #create subsets and rename columns for widening
  ing.df <- filter(prof.agg, Cost.Type == "Ingredient")
  colnames(ing.df) <- c("Product", "Ing.Label", "Ingredients")
  labor.df <- filter(prof.agg, Cost.Type == "Labor")
  colnames(labor.df) <- c("Product", "Labor.Label", "Labor")
  profit.df <- filter(prof.agg, Cost.Type == "Profit")
  colnames(profit.df) <- c("Product", "Profit.Label", "Profit")
  
  #join new dfs wider
  new.df <- left_join(ing.df, labor.df)
  new.df2 <- left_join(new.df, profit.df)
  new.df2 <- mutate(new.df2, "Price" = new.df2$Ingredients + new.df2$Labor + new.df2$Profit, "Margin" = Profit/Price)
  #print(head(new.df2))
  
  #get columns I want in order
  pretty.df <- select(new.df2, Product, Margin, Price, Profit, Ingredients, Labor)
  
  if(pretty.nums == TRUE){
  #make columns pretty
  pretty.df$Ingredients <- paste0("$", format(round(pretty.df$Ingredients, 2), nsmall = 2))
  pretty.df$Price <- paste0("$", format(round(pretty.df$Price, 2), nsmall = 2))
  pretty.df$Profit <- paste0("$", format(round(pretty.df$Profit, 2), nsmall = 2))
  pretty.df$Labor <- paste0("$", format(round(pretty.df$Labor, 2), nsmall = 2))
  pretty.df$Margin <- paste0(round(pretty.df$Margin*100, 1), "%")
  }
  pretty.df <- arrange(pretty.df, desc(Margin))
  pretty.df

}