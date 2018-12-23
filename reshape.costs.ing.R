# this function takes the imported df from google sheet and reshapes it with just ingredients (not labor)

reshape.costs.ing <- function(imported.ingredient.info.df, raw.unit.costs.df, rows.of.products){
  conv.df <- imported.ingredient.info.df
  
  unit.converter <- function(df, quantity.col.num, units.col.num){
    conversions <- data.frame("eng.meas" = c("unit(s)", "ounce(s) fl","cup(s) fl", "quart(s)", "gallon(s)", "tablespoon(s) fl", "cup(s) dry", "tablespoon(s) dry", "ounce(s) dry", "lb(s)"),
                              "to.metric" = c(1, 29.57, 240, 15, 946.35, 3840, 190, 12, 30, 453.59),
                              "metric.unit" = c("units", "ml","ml", "ml", "ml", "ml", "g", "g", "g", "g"))
    library(dplyr)
    
    print(conversions)
    
    num.convert <- function(quantity.col, eng.units.col){
      this.conversion <- conversions %>%
        filter(eng.meas == eng.units.col)
      #print(this.conversion)
      quantity.col <- quantity.col*this.conversion$to.metric
    }
    
    num.col <- as.numeric(mapply(num.convert, df[,quantity.col.num], df[,units.col.num]))
    print(num.col)
    
    unit.convert <- function(eng.units.col){
      this.conversion <- conversions[conversions$eng.meas == eng.units.col,]
      eng.units.col <- as.character(this.conversion$metric.unit)
      # use as.character() on the result after sapply
      eng.units.col
    }
    
    units.col <- as.character(sapply(df[,units.col.num], unit.convert))
    print(units.col)
    
    df[,quantity.col.num] <- num.col
    df[,units.col.num] <- units.col
    
    print(df)
    df
  }
  
  colnames(conv.df)[3:32] <- rep(c("Ing.or.Phase", "Quantity", "Unit"), 10)
  all.long.df <- data.frame()
  for(i in rows.of.products){
    print(i)
    if(!is.na(conv.df[i,1])){
  ingreds <- rbind(conv.df[i,3:5], conv.df[i,6:8], conv.df[i,9:11], conv.df[i, 12:14], 
                   conv.df[i, 15:17], conv.df[i, 18:20], conv.df[i, 21:23], conv.df[i, 24:26]
                   , conv.df[i, 27:29], conv.df[i, 30:32])
  ingreds <- ingreds[!is.na(ingreds$Unit),]
  long.df <- cbind("Product" = conv.df[i,1], "Batch.Size" = conv.df[i,2], "Cost.Type" = "Ingredient", ingreds)
  all.long.df <- rbind(all.long.df, long.df)
    } #ends the NA check
  }
  
  all.long.df <- unit.converter(all.long.df,5,6)
  
  all.long.merged <- merge(all.long.df, raw.unit.costs.df[,1:2], by.x = "Ing.or.Phase", by.y = "Ingredient Name", 
                           all.x = TRUE, all.y = FALSE)
  all.long.merged$Cost <- all.long.merged$`Price Paid`*all.long.merged$Quantity
  all.long.merged$Batch.Size <- as.numeric(all.long.merged$Batch.Size)
  all.long.merged$Cost.Div.Batch <- all.long.merged$Cost/all.long.merged$Batch.Size
  all.long.merged <- all.long.merged %>%
    select(Product, Batch.Size, Cost.Type, Ing.or.Phase, Quantity, Unit, Cost, Cost.Div.Batch) %>%
    arrange(Product)
  print(all.long.merged)
  all.long.merged

}