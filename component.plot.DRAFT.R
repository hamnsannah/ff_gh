component.plot <- function(df, title, palette.vector){
  
  df <- arrange(df, Ing.or.Phase)
  df$Ing.or.Phase <- factor(df$Ing.or.Phase)
  library(ggplot2)
  library(scales)
  
  print(df)
  print(str(df))
  bar.plot1 <- ggplot(data=filter(df, Product == "Cashew Cluster"), aes(x=Product, y=Cost.Div.Batch)) + 
    geom_col(aes(fill = factor(Ing.or.Phase)), position = position_stack(reverse = TRUE)) +
    geom_label(aes(label = factor(Ing.or.Phase)), position = position_stack(vjust = 0.5))+
    scale_y_continuous(labels = scales::dollar) +
    theme(legend.position = "none") +
    NULL
  
  bar.plot1
}