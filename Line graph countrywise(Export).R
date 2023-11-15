library(tidyverse)
library(plotly)
exp=read.csv("C:/Users/darsh/Downloads/Export.csv")
data=ggplot()+
      geom_line(exp,mapping= aes(x=Year, y=Revenue1, col="China"))+
      geom_point(exp, mapping=aes(x=Year, y=Revenue1, col="China"))+
      geom_line(exp,mapping= aes(x=Year, y=Revenue2, color="Russia"))+
      geom_point(exp, mapping=aes(x=Year, y=Revenue2, colour="Russia"))+
      geom_line(exp,mapping= aes(x=Year, y=Revenue3, colour="USA"))+
      geom_point(exp, mapping=aes(x=Year, y=Revenue3, colour="USA"))+
      labs(x="Years", y="Export in millions", title="Change in revenue of export from 2009 to 2015")
  ggplotly(data)
  
