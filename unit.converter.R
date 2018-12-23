
# function transmutes adjacent columns with english units cooking measurements to metric
# uses column numbers rather than names as references

#sample usage: unit.converter(ing.costs, 3, 4)

unit.converter <- function(df, quantity.col.num, units.col.num){
  conversions <- data.frame("eng.meas" = c("unit(s)", "ounce(s) fl","cup(s) fl", "quart(s)", "gallon(s)", "tablespoon(s) fl", "cup(s) dry", "tablespoon(s) dry", "ounce(s) dry", "lb(s)"),
                            "to.metric" = c(1, 29.57, 240, 15, 946.35, 3840, 190, 12, 30, 453.59),
                            "metric.unit" = c("units", "ml","ml", "ml", "ml", "ml", "g", "g", "g", "g"))
  library(dplyr)
  
  #print(conversions)
  
  num.convert <- function(quantity.col, eng.units.col){
    this.conversion <- conversions %>%
      filter(eng.meas == eng.units.col)
    #print(this.conversion)
    quantity.col <- quantity.col*this.conversion$to.metric
  }
  
  num.col <- as.numeric(mapply(num.convert, df[,quantity.col.num], df[,units.col.num]))
  #print(num.col)
  
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

  #print(df)
  df
}


