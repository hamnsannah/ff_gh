#sample usage: ing.quants <- s.gsheet.imp("FF Fake Data")
#sample usage: ing.costs <- s.gsheet.imp("Ingredient Costs")

s.gsheet.imp <- function(googlesheet.name){
  library(googlesheets)
  gs_auth()
  source.gsheet <- gs_title(googlesheet.name)
  imported.data <- gs_read(source.gsheet)#, range = "A1:AQ100")
  imported.data <- as.data.frame(imported.data)
  print(imported.data)
  imported.data

  
  # next steps is change units in data to metric
}
#######


conversions <- data.frame("eng.meas" = c("cups fl", "tablespoons fl", "cups dry", "tablespoons dry", "units", "ounces dry", "gallons"),
"to.metric" = c("240", "15", "190", "12", "1", "30", "3840"),
"metric.unit" = c("ml", "ml", "g", "g", "na", "g", "ml"))
print(conversions)