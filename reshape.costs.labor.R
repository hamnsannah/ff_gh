reshape.costs.labor <- function(imported.ingredient.info.df, rows.of.products){
  library(dplyr)

  conv.df <- imported.ingredient.info.df
  temp.col.names <- rep(c("Length", "People"), 5)
  colnames(conv.df)[34:43] <- temp.col.names
  
  #colnames(conv.df)[3:32] <- rep(c("Ing.or.Phase", "Quantity", "Units"), 10)
  all.long.df <- data.frame()
  for(i in rows.of.products){
    print(i)
    if(!is.na(conv.df[i,1])){
      labors <- rbind(conv.df[i,34:35], conv.df[i,36:37], conv.df[i,38:39], conv.df[i, 40:41], 
                       conv.df[i, 42:43])
      labor.hours.df <- cbind(labors, labor.hours = labors[,1]*labors[,2])

      labor.hours.df <- labor.hours.df[!is.na(labor.hours.df$labor.hours),]
      print(labor.hours.df)
      long.df <- cbind("Product" = conv.df[i,1], "Batch.Size" = conv.df[i,2], 
                       "Cost.Type" = "Labor", "Ing.or.Phase" = paste("Phase", seq_len(nrow(labor.hours.df))),
                       "Quantity" = labor.hours.df$labor.hours, "Unit" = "labor hours")
      long.df <- as.data.frame(long.df)
      #print("line 22")

      print(conv.df[i,33])
      pay.rate <- as.numeric(conv.df[i,33])
      long.df$Quantity <- as.numeric(long.df$Quantity)
      long.df$Cost <- long.df$Quantity*pay.rate
      print("Batch Size is")
      print(head(long.df$Batch.Size, 20))
      print(class(long.df$Batch.Size))
      long.df$Batch.Size <- as.numeric(as.vector(long.df$Batch.Size))
      print(head(long.df$Batch.Size, 20))
      long.df$Cost.Div.Batch <- long.df$Cost/long.df$Batch.Size
      #long.df$Cost <- "dollars"
      print(long.df)      
      all.long.df <- rbind(all.long.df, long.df)
      print("here is all.long.df")
      print(all.long.df)
    } #ends the NA check
  }
  
  print(all.long.df)
  all.long.df
}