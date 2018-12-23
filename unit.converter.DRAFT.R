unit.converter <- function(df, quantity.col.n, units.col.name){
  conversions <- data.frame("eng.meas" = c("unit(s)", "cup(s) fl", "quart(s)", "gallon(s)", "tablespoon(s) fl", "cup(s) dry", "tablespoon(s) dry", "ounce(s) dry", "lb(s)"),
                            "to.metric" = c(1, 240, 15, 946.35, 3840, 190, 12, 30, 453.59),
                            "metric.unit" = c("units", "ml", "ml", "ml", "ml", "g", "g", "g", "g"))
  print(conversions)

num.convert <- function(quantity.col, eng.units.col){
  #this.conversion <- conversions %>% filter(eng.meas == eng.units.col)
  #print(this.conversion)
  this.conversion <- conversions %>%
    filter(eng.meas == eng.units.col)
  print(this.conversion)
  quantity.col <- quantity.col*this.conversion$to.metric
}

num.col <- as.numeric(mapply(num.convert, quantity.col.name, units.col.name))
print(num.col)

unit.convert <- function(eng.units.col){
  this.conversion <- conversions %>% filter(eng.meas == eng.units.col)
  eng.units.col <- as.character(this.conversion$metric.unit)
  # use as.character() on the result after sapply
}

units.col <- as.character(sapply(units.col.name, unit.convert))
print(units.col)


#df[colnames(df) == quantity.col.name,] <- num.col
#df[colnames(df) == units.col.name,] <- units.col

ing.costs2 <- ing.costs%>%
  select(Quantity) %>%
  transmute(Quantity == num.col, Units == units.col)

print(df)

}


