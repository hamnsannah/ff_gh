# this function either generates a plot with sales and profit per day along with margin as a label; 
# or it generates a kable table with the same data




# what is the question to answer?  How profitable were yesterday's, last month's, x period's sales?
# that question by itself is just a percentage.  What are next questions?  Why so high/low, why increase/decrease?

# what info, table, or viz helps with that?
#1 percentage margin for one period and some prior period, vs. same period year earlier, vs. most recent month
#1a maybe stil show breakdown of ingredients and labor, but may be tough to retain that granularity passing it through, 
    #but I do think it's in the kable.prep
    # would a stacked area graph be helpful with profit and ingredient and labor by day, no I think granularity wouldn't be useful because not actionable.
    # doesn't matter which cost, just avoid low margin regardless of why low margin.  Line graph with sales as total and aggregate costs might be really good.

fake.sales.data <- data.frame("Product" = rep(c("Van. Buttercream Reg. Dark", "Van. Buttercream Reg. Milk", 
                                             "Van. Butter Car. Dipped Dark", "Van. Butter Car. Dipped Milk",    
                                             "Van. Butter Car. Wrapped", "Van. Butter Car. Salt. Dip. Milk", 
                                             "Van. Butter Car. Salt. Dip. Dark", "Van. Butter Car. Salt. Wrapped"), 7), 
                              "Sold" = round(abs(rnorm(n = 56, mean = 50, sd = 25))),
                              "Date" = c(rep("2018-01-01", 8), rep("2018-01-02", 8), rep("2018-01-03", 8), rep("2018-01-04", 8),
                              rep("2018-01-05", 8), rep("2018-01-06", 8), rep("2018-01-07", 8)))
fake.sales.data$Date <- as.Date(as.vector(fake.sales.data$Date))

#example: product.test <- product.mix.margin(kable.data, fake.sales.data, plot.or.kable = "plot")
product.mix.margin <- function(kable.data.not.pretty, sales.data, plot.or.kable = "plot"){
  library(dplyr)
  library(ggplot2)
  library(knitr)
  #use loop to break data into each day
  days <- unique(sales.data$Date)
  
  multi.day.data <- data.frame()
  for(i in 1:length(days)){
    day.i <- days[i]
    sales.joined <- sales.data %>%
      filter(Date == day.i) %>%
      left_join(kable.data) %>%
      mutate("Day.Sales" = Sold*Price, "Day.Profit" = Sold*Profit)
    sales.joined$Day.Sales <- as.numeric(sales.joined$Day.Sales)
    sales.joined$Day.Profit <- as.numeric(sales.joined$Day.Profit)
    print(sales.joined$Day.Sales)[1]
    day.data <- data.frame("Day" = day.i, "Day.Sales" = sum(sales.joined$Day.Sales), "Day.Profit" = sum(sales.joined$Day.Profit))
    #                                                          )) c(day.i, as.numeric(sum(sales.joined$Day.Sales)), as.numeric(sum(sales.joined$Day.Profit)))
    print(day.data)
    multi.day.data <- rbind(multi.day.data, day.data)
  }
  multi.day.data  
  multi.day.data <- mutate(multi.day.data, "margin.perc" = paste0(round((Day.Profit/Day.Sales)*100),"%"))
  print(multi.day.data)
  if(plot.or.kable == "plot"){
  day.plot <- 0
  day.plot <- ggplot(data = multi.day.data, aes(x = Day)) +
    geom_line(aes(y=Day.Sales), col = "#E8D5A1", size = 3)+
    geom_line(aes(y=Day.Profit), col = "#98F95D", size = 3)+
    geom_label(aes(y= Day.Profit, label = margin.perc), nudge_y = .5) +
    ylim(0,NA)+
    ggtitle(label = "Sales and Profit Line Graphs Labeled with Margin %", 
            subtitle = "Green = Profit; Tan = Sales")+
    ylab("Dollars($)")+
    theme_dark() #"#E8D5A1", "#6F2919", "#98F95D"
  multi.day.data <- day.plot
  } else {
    multi.day.data$Day.Sales <- paste0("$", round(multi.day.data$Day.Sales, 2))
    multi.day.data$Day.Profit <- paste0("$", round(multi.day.data$Day.Profit, 2))
    multi.day.data <- transmute(multi.day.data, "Day" = Day, "Daily Sales" = Day.Sales, 
                                "Daily Profit" = Day.Profit, "Margin %" = margin.perc)
multi.day.data <- kable(multi.day.data)    
  }
  multi.day.data
}
  
  #ultimately use Google Sheets to import, probably