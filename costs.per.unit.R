# this function takes a list of prices of raw ingredients which has already been converted to metric
# and it takes it to per usnit

costs.per.unit <- function(unit.converted.cost.list.df, price.col.num, quantity.col.num){
  cost.df <- unit.converted.cost.list.df
  unit.divide <- function(a,b){a <- (a/b)}
  price.per.unit.col <- mapply(unit.divide, cost.df[,price.col.num], cost.df[,quantity.col.num])
  #quantity.one.col <- 1
  
  cost.df[,price.col.num] <- price.per.unit.col
  cost.df[,quantity.col.num] <- 1
  cost.df
}