#
# file name: app.R
#Author: Maaz Kamal

#install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)




server <- function(input, output){
 # data.dir<-"C:/Users/maazk/OneDrive - Syracuse University/S/Uni/Srping 2021/IST 719 Viz/data/"
  sales<-read.csv("sales.csv", header=TRUE, stringsAsFactors= FALSE)
  
  output$yearlySales<- renderPlot({
    
    sales %>% group_by(rep.region, year) %>%
      summarise(recipt = sum(recipt)) %>%
      ggplot() +
      aes(x = year, y = recipt, color = rep.region) +
      geom_line(size = 2) + ylim(c(0,100000)) +
      ggtitle("Sales by Region") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    
  })
  
  
  output$wine.by.region<- renderPlot({
    
    sales.2<-sales
    region.name<-"ALL"
    my.year<-"ALL"
    
    if (input$region != "ALL"){
      
      
      sales.2<- sales.2 %>% filter(rep.region== input$region)
      region.name<- input$region
    }
    
    
    
    if (input$year != "ALL"){
      
      
      sales.2<- sales.2 %>% filter(year== input$year)
      my.year<- input$year
    }
    
    
    sales.2 %>% group_by(wine, type) %>%
      summarise(recipt=sum(recipt)) %>%
      ggplot()+
      aes(x=wine, y=recipt, fill=type)+
      geom_bar(stat="identity", position="dodge")+
      scale_fill_manual(values=c("#B62D23","#FBD16D"))+
      theme_minimal()+
      ggtitle(paste("Wine sales for region", region.name, ", year", my.year))
    
  })
  
  
  output$maaz<-renderPlot({
    
    hist(rnorm(100))
  }
  
  )
  
  

  
  
}

ui <- fluidPage(
  titlePanel("ACME Wine Company Dashboard"),
  
  sidebarLayout(
    sidebarPanel(tags$style(".well (background-color:#A0A0A0)"),
                plotOutput("yearlySales")
                
                 
  ),
  
  
  
  mainPanel(
    
    
    
    selectInput("region", "Select Region",
                choices=c("ALL","Central","East","North", "South", "West")),
    selectInput("year", "Select Year",
                choices = c("ALL", "2010", "2011", "2012", "2013", "2014")),
    plotOutput("wine.by.region")
    
  )
)
  
)


shinyApp(ui=ui, server=server)
