
shane.alluvial <- function(df.from.reshape.costs.ing, min.cost.to.include = .015){
  library(dplyr)
  library(alluvial)

  
  # Would probably be better to get more stretched out version of this, where tbe brown starts at dark not milk
choc12 <- c("#6F2918", "#7A3520", "#854329", "#905232", "#9B613D", "#A67148",
                 "#B18155", "#BC9262", "#C6A370", "#D1B37F", "#DCC48F", "#E7D5A1")

ing.filt <- df.from.reshape.costs.ing[df.from.reshape.costs.ing$Cost.Div.Batch >= min.cost.to.include,]
ingredient.vec <- unique(ing.filt$Ing.or.Phase)
#print(ingredient.vec)
alluvial.plot <- alluvial(ing.filt[,c(4,1)], freq = ing.filt$Cost.Div.Batch, cex = 0.7, col = ifelse(ing.filt$Ing.or.Phase == ingredient.vec[1], choc12[5],
                                                                                                         ifelse(ing.filt$Ing.or.Phase == ingredient.vec[2], choc12[10], 
                                                                                                                ifelse(ing.filt$Ing.or.Phase == ingredient.vec[3], choc12[1],
                                                                                                                       ifelse(ing.filt$Ing.or.Phase == ingredient.vec[4], choc12[8], 
                                                                                                                              ifelse(ing.filt$Ing.or.Phase == ingredient.vec[5], choc12[7], 
                                                                                                                                     ifelse(ing.filt$Ing.or.Phase == ingredient.vec[6], choc12[12], choc12[6])))))))
alluvial.plot
}