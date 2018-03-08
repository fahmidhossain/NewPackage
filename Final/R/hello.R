#' @title Apply a filter to a data.frame
#'
#' @param df data.frame
#'
#' @export
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(lme4)
library(tidyverse)
install.packages("boot",dep=TRUE)
library(boot)
MyData <- read_csv("https://raw.githubusercontent.com/JackStat/PracticalDataScience/master/data/winemag-data-130k-v2.csv")
View(MyData)

##3. Density Plot


densityPlot <- function(x,y){
  attach(x)
  ggplot(x)+
    geom_density(aes(y))+theme_customer()
}
densityPlot(MyData, points)

##4. Customer GGplot theme

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

##5. top n number of a grouped variable by counts

Grouping<- function(x,y){
    group_by(MyData,variety,  na.rm = TRUE) %>%
    summarise(
      count = n()
    )%>%
    arrange(desc(count))%>%
    head(6)
}
Grouping(MyData, variety)

#6. Other 8 functions
Top6_data= filter(datafile, variety == "Pinot Noir" | variety == "Chardonnay" | variety == "Cabernet Sauvignon" | variety == "Red Blend" | variety == "Bordeaux-style Red Blend" | variety == "Riesling")
#1 bootstrap
newdata= filter(MyData,!is.na(price))
fc <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$price, d2$points))
}
bootcorr <- boot(newdata, fc, R=500)
bootcorr
plot(bootcorr)

#2 scatter plot

ScatterPlot <- function(x,y,z){
  attach(x)
  ggplot(x, aes(y, z))+geom_point()
}
ScatterPlot(Top6_data, points, price)

#3 Boxplot

BoxPlot <- function(x,y,z,a){
  attach(x)
  ggplot(x, aes(y, z, group = a))+geom_point()+
    geom_boxplot()
}
BoxPlot(Top6_data, points, price, points)

#4 Violin plot

ViolinPlot <- function(x,y,z,a){
  attach(x)
  ggplot(x, aes(y, z, group = a))+geom_point()+
    geom_violin()
}
ViolinPlot(Top6_data, points, price, points)

#5 Barchart

BarChart <- function(x,y){
  attach(x)
  ggplot(x)+ geom_bar(aes(y))
}
BarChart(Top6_data, country)

#6 Areaplot

AreaPlot <- function(x,y,z,a){
  attach(x)
  ggplot(x, aes(y, z, color = a))+
    geom_area()
}
AreaPlot(Top6_data, points, price, variety)

#7 Columnplot

ColumnPlot <- function(x,y,z,a){
  attach(x)
  ggplot(x, aes(y, z, color = a))+
    geom_col()
}
ColumnPlot(Top6_data, points, price, variety)

#8 DotPlot
DotPlot <- function(x,y,z,a){
  attach(x)
  ggplot(x, aes(y, z, color = a))+
    geom_dotplot()
}
DotPlot(Top6_data, points, price, variety)

