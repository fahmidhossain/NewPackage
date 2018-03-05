#' @title Apply a filter to a data.frame
#'
#' @param df data.frame
#'
#' @export

library(tidyverse)
MyData <- read.csv(file="D:/UfU Study/CVEEN 6560_Transportation Planning/Data/HouseholdLevel.csv", header=TRUE, sep=",")
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
    geom_density(aes(y))
}
densityPlot(MyData, hhsize)
