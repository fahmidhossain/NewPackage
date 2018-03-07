#' @title Apply a filter to a data.frame
#'
#' @param df data.frame
#'
#' @export

library(tidyverse)
MyData <- read_csv("https://raw.githubusercontent.com/JackStat/PracticalDataScience/master/data/winemag-data-130k-v2.csv")
View(MyData)
ggDensity= function(df= mtcars, filter.var= 'cyl', filter.val= 6){

  Filter= paste0(filter.var, "==", filter.val)
  print(Filter)
  df2=
    df %>%
    filter(Filter)

  ggplot(df2, aes(x= raquate))
}
densityPlot <- function(x,y){
  attach(x)
  ggplot(x)+
    geom_density(aes(y))+theme_customer()
}
densityPlot(MyData, points)

theme_customer <- function(base_size = 10){

  txt <- element_text(size = base_size+2, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size+2, colour = "black", face = "bold")

  theme_bw(base_size = base_size)+
    theme(
      ###### clean up!
      legend.key = element_blank(),
      strip.background = element_blank(),
      ########### text basics
      text = txt,
      plot.title = txt,

      axis.title = txt,
      axis.text = txt,

      legend.title = bold_txt,
      legend.text = txt ) +

    ############## AXIS lines
    theme(

      axis.line.y = element_line(colour = "pink", size = 1, linetype = "dashed"),
      axis.line.x = element_line(colour = "pink", size = 1.2,linetype = "dashed"),
      #### remove Tick marks
      axis.ticks=element_blank(),

      ### legend  top and no title!
      legend.position = "top",
      legend.title = element_blank(),
      legend.key = element_rect(fill = "lightskyblue1", color = "lightskyblue1"),
      legend.background = element_rect( fill = "lightskyblue1",color = "pink", size = 0.5,linetype = "longdash"),

      ## background
      plot.background = element_rect(fill = "lightskyblue1",colour = "pink",size = 0.5, linetype = "longdash")
    )
}

theme_customer()
densityPlot(MyData, points)
