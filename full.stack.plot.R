choc.green.swap <- c("#E8D5A1", "#6F2919", "#98F95D")

full.stack.plot <- function(df, product.to.plot, palette.vector){
  
  library(ggplot2)
  library(scales)
  library(dplyr)
  
  #print(df)
  #print(str(df))
  df$Cost.Div.Batch <- as.numeric(as.vector(df$Cost.Div.Batch))
  
  df.agg <- aggregate(Cost.Div.Batch ~ Product + Cost.Type, df, sum)
  
  df.agg$Cost.Type <- factor(df.agg$Cost.Type, levels = c("Labor", "Ingredient", "Profit"))
  bar.plot1 <- 0
  
  print(str(df.agg))
  print(dim(df.agg))
  print(df.agg)
  bar.plot1 <- ggplot(data=filter(df.agg, Product == product.to.plot), aes(x=Product, y=Cost.Div.Batch)) + 
    #bar.plot1 <- ggplot(data=df.agg, aes(x=Product, y=Cost.Div.Batch)) + 
      geom_col(aes(fill = factor(Cost.Type, levels = c("Labor", "Ingredient", "Profit"))), 
               position = position_stack(reverse = TRUE), width = .5) +
      #geom_label(aes(label = factor(Cost.Type, levels = c("Labor", "Ingredient", "Profit"))), 
      #               position = position_stack(reverse = FALSE, vjust = 0.5))+
      scale_y_continuous(labels = scales::dollar) +
      scale_fill_manual(values = palette.vector) +
      labs(x="Product", y="Costs and Profit in Retail Price", subtitle = "A green bar below zero indicates a loss", title = paste0("Costs and Profit for ", product.to.plot)) +
      theme_dark() +
    theme(legend.title = element_blank()) +
    NULL
  
  bar.plot1
}