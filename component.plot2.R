#6F2919, #EBD9A7, #98F95D
#choc.green <- c("#6F2919", "#EBD9A7", "#98F95D")
choc.green <- c("#6F2919", "#E8D5A1", "#98F95D")
#EAE168
component.plot2 <- function(df, product.to.plot, palette.vector){
  
  #df <- arrange(df, Ing.or.Phase)
  df$Ing.or.Phase <- factor(df$Ing.or.Phase, levels = c("Phase 8", "Phase 7", "Phase 6", "Phase 5", 
                                                        "Phase 4", "Phase 3", "Phase 2", "Phase 1", 
                                                        "Cocoa", "Sugar", "Milk", "Cashews", "Frosting", 
                                                        "Eggs", "Flour", "Caramel", "Vanilla bean", 
                                                        "Liquid Sugar", "Lollipop sticks", "Wrapper"))
  library(ggplot2)
  library(scales)
  library(dplyr)
  
  #print(df)
  #print(str(df))
  bar.plot1 <- ggplot(data=filter(df, Product == product.to.plot), aes(x=Ing.or.Phase, y=Cost.Div.Batch, group = Cost.Type)) + 
    geom_col(aes(fill = Cost.Type), position = "dodge") +
    #geom_label(aes(label = factor(Ing.or.Phase)), position = position_stack(vjust = 0.5))+
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_manual(values = palette.vector) +
    #theme(legend.position = "none") +
    labs(x= "Cost Type", y="Cost in One Unit", title = paste0("Cost Breakdown for ", product.to.plot)) +
    theme_dark() +
    theme(legend.title = element_blank()) +
    coord_flip() +
    NULL
  
  bar.plot1
}