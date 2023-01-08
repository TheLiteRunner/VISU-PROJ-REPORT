library(ggplot2)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(googlesheets4)
library(lubridate)
library(openintro)
library(palmerpenguins)
library(maps)
library(ggmap)
library(ggthemes)
library(ggpubr)
library(tidyverse)
library(RcppEigen)
library(shiny)
library(plotly)
library(readr)


rishi2 <- read_csv("./data/rishi2.csv")
s_p <- read_csv("./data/s_p.csv")
s_n <- read_csv("./data/s_n.csv")
s_m <- read_csv("./data/s_m.csv")
s_k <- read_csv("./data/s_k.csv")
s_j <- read_csv("./data/s_j.csv")
s_g <- read_csv("./data/s_g.csv")
s_d <- read_csv("./data/s_d.csv")
s_c <- read_csv("./data/s_c.csv")
s_b <- read_csv("./data/s_b.csv")
rishi2_srm <- read_csv("./data/rishi2_srm.csv")
rishi2_rrm <- read_csv("./data/rishi2_rrm.csv")
rishi2_prm <- read_csv("./data/rishi2_prm.csv")
#CITY <- read_csv("./data/CITY.csv")
#CITY_srm <- read_csv("./data/CITY_srm.csv")
#CITY_prm <- read_csv("./data/CITY_prm.csv")

ui <- shinyUI(dashboardPage(
  
  dashboardHeader(title="House Prices (IND)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Count Of BHK No.s", tabName = "M0"),
      menuItem("Maps describing Price and BHK No.",tabName = "M1"),
      menuItem("Trends wrt BHK No.",tabName = "M2")
    )
  ), 
  dashboardBody(  
    tabItems(
      
      tabItem("M0",h2("Count of BHK No. in different cities of 
india",align = "center"),
              
              fluidRow(column(4,actionButton("Bar_G","Bar Graph")),
                       column(4,actionButton("Pie_G","Pie Chart")),    
                       sidebarLayout(sidebarPanel(selectInput("Univ", 
                                                              "Choose a City/Location:", 
                                                              
                                                              choices=c("Guwahati","New_Delhi","Bangalore","Mumbai","Pune","Chennai","Nagpur","Kolkata","Whole_India","Jaipur"),selected = c("Mumbai")),
                       ),
                       mainPanel(plotOutput("plot2")
                       )
                       )
                       
              )),
      
      tabItem("M1", 
              
              
              
              
              
              
              fluidPage(
                sidebarLayout(sidebarPanel( 
                  selectInput("var", 
                              label = "Choose a variable to display",
                              choices=c("Guwahati","New_Delhi","Bangalore","Mumbai","Pune","Chennai","Nagpur","Kolkata","Whole_India","Jaipur"),selected = c("Mumbai")),
                ),
                
                
                mainPanel(plotlyOutput("plot1"))
                )
              )
              
      ),
      tabItem("M2",h2("How Price, Area and Area Per unit Price changes 
with BHK No.",align = "center"),
              
              fluidRow(column(4,actionButton("Guwahati","Guwahati")),
                       column(4,actionButton("New_Delhi","New_Delhi")), 
                       column(4,actionButton("Bangalore","Bangalore")),
                       column(4,actionButton("Mumbai","Mumbai")),
                       column(4,actionButton("Pune","Pune")),
                       column(4,actionButton("Chennai","Chennai")),
                       column(4,actionButton("Nagpur","Nagpur")),
                       column(4,actionButton("Kolkata","Kolkata")),
                       column(4,actionButton("Whole_India","Whole_India")),
                       column(4,actionButton("Jaipur","Jaipur")),
                       sidebarLayout(sidebarPanel(selectInput("Univa", 
                                                              "Choose a City/Location:", 
                                                              
                                                              choices=c("Price","Area","Area_Per_Lakh_Rs"),selected = c("Area")),
                       ),
                       mainPanel(plotOutput("plot3")
                       )
                       )
                       
              ))
      
    ))))

server <- shinyServer(
  
  function(input, output) {
    
    
    output$plot1 <- renderPlotly({
      
      if(input$var=="Whole_India"){
        
        
        
        india_map = get_stamenmap(bbox = c(left = 58.711, bottom=5.572, 
                                           right = 98, top=36.457),
                                  maptype="toner-lite",
                                  zoom=5)
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))
        
        q+scale_size_area(max_size=2)  
        
        
        ggplotly(q+scale_size_area(max_size=2)+labs(size="Price ", color="BHK no "))}
      
      
      
      
      else if(input$var=="Guwahati"){
        india_map = get_stamenmap(bbox = c(left = 91.38, bottom=25.94, 
                                           right = 92.128, top=26.4),
                                  maptype="toner-lite",
                                  zoom=11)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))
        
        q+labs(size="Price ", color="BHK no ")
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      else if(input$var=="Mumbai"){
        india_map = get_stamenmap(bbox = c(left = 72.5050, bottom=18.8335, 
                                           right = 73.2534, top=19.3299),
                                  maptype="toner-lite",
                                  zoom=11)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))+scale_size_area(max_size=3)
        
        q+labs(size="Price ", color="BHK no ")
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      else if(input$var=="Kolkata"){
        india_map = get_stamenmap(bbox = c(left = 88.1025, bottom=22.4202, 
                                           right = 88.5968, top=22.6628),
                                  maptype="toner-lite",
                                  zoom=11)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))+scale_size_area(max_size = 3)
        
        q+labs(size="Price ", color="BHK no ")
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      else if(input$var=="Chennai"){
        india_map = get_stamenmap(bbox = c(left = 79.8500, bottom=12.6712, 
                                           right = 80.8388, top=13.1831),
                                  maptype="toner-lite",
                                  zoom=11)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))+scale_size_area(max_size = 3)
        
        q+labs(size="Price ", color="BHK no ")
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      else if(input$var=="Jaipur"){
        india_map = get_stamenmap(bbox = c(left = 91.38, bottom=25.94, 
                                           right = 92.128, top=26.4),
                                  maptype="toner-lite",
                                  zoom=11)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))
        
        q+labs(size="Price ", color="BHK no ")
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      else if(input$var=="Nagpur"){
        india_map = get_stamenmap(bbox = c(left = 78.9000, bottom=21.0195, 
                                           right = 79.3944, top=21.2644),
                                  maptype="toner-lite",
                                  zoom=11)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))
        
        q+labs(size="Price ", color="BHK no ")
        
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      else if(input$var=="New_Delhi"){
        india_map = get_stamenmap(bbox = c(left = 76.9235, bottom=28.2856, 
                                           right = 77.6720, top=28.7472),
                                  maptype="toner-lite",
                                  zoom=11)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)),position = "stack")
        
        q+labs(size="Price ", color="BHK no ")
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      else if(input$var=="Pune"){
        india_map = get_stamenmap(bbox = c(left = 73.6379, bottom=18.4008, 
                                           right = 74.1323, top=18.6498),
                                  maptype="toner-lite",
                                  zoom=12)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)))+scale_size_area(max_size = 3)
        
        q+labs(size="Price ", color="BHK no ")
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      else if(input$var=="Bangalore"){
        india_map = get_stamenmap(bbox = c(left = 77.4286, bottom=12.8640, 
                                           right = 77.8028, top=13.1199),
                                  maptype="toner-lite",
                                  zoom=12)
        
        q=ggmap(india_map)+
          geom_point(rishi2,mapping=aes(x=LATITUDE, y=LONGITUDE, 
                                        size=PRICE_IN_LACS, color=factor(BHK_NO.)),position="stack")
        
        q
        
        
        
        ggplotly(q+labs(size="Price ", color="BHK no "))}
      
      
      
      
      
      
      
      
      
    })
    
    observeEvent(
      input$Bar_G,{output$plot2 <- renderPlot({
        ##
        if(input$Univ=="Whole_India"){
          
          ggplot()+geom_bar(data=rishi2,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        
        else if(input$Univ=="Guwahati"){
          
          ggplot()+geom_bar(data=s_g,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        
        else if(input$Univ=="New_Delhi"){
          
          ggplot()+geom_bar(data=s_d,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        
        else if(input$Univ=="Bangalore"){
          
          ggplot()+geom_bar(data=s_b,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        else if(input$Univ=="Jaipur"){
          
          ggplot()+geom_bar(data=s_j,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        else if(input$Univ=="Mumbai"){
          
          ggplot()+geom_bar(data=s_m,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        else if(input$Univ=="Pune"){
          
          ggplot()+geom_bar(data=s_p,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        else if(input$Univ=="Chennai"){
          
          ggplot()+geom_bar(data=s_c,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        else if(input$Univ=="Nagpur"){
          
          ggplot()+geom_bar(data=s_n,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        else if(input$Univ=="Kolkata"){
          
          ggplot()+geom_bar(data=s_k,aes(x=factor(BHK_NO.),fill=factor(BHK_NO.)))+labs(x="Number 
of BHKs")+theme(plot.caption = element_text(hjust=0.5,size=16, color="dark 
blue"))+guides(fill 
               = guide_legend(title = "BHK No.s"))}
        
        
        
        
        
      })})
    
    observeEvent(
      input$Pie_G,{output$plot2 <- renderPlot({
        ##
        
        if(input$Univ=="Whole_India"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(rishi2$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
          
        }
        
        
        
        else if(input$Univ=="Guwahati"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_g$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        else if(input$Univ=="New_Delhi"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_d$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        else if(input$Univ=="Mumbai"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_m$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
          
        }
        
        
        
        
        
        else if(input$Univ=="Pune"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_p$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        else if(input$Univ=="Bangalore"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_b$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        else if(input$Univ=="Nagpur"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_n$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        else if(input$Univ=="Chennai"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_c$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        else if(input$Univ=="Kolkata"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_k$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        else if(input$Univ=="Jaipur"){
          data <- data.frame(
            BHK_No.=c("1","2","3","4","5","6"),
            value=c(table(s_j$BHK_NO.)[1:6])
            
          )
          
          ggplot(data, aes(x="", y=value, fill=BHK_No.)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
        }
        
        
        
        
        
        
        
        
        
      })})
    
    observeEvent(
      input$Whole_India,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(rishi2_prm,aes(x=factor(rishi2_prm$BHK_NO.),y=PRICE_IN_LACS, 
                                fill=factor(rishi2_prm$BHK_NO.)))+geom_boxplot()+guides(fill = 
                                                                                          guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        else if(input$Univa=="Area"){
          
          ggplot(rishi2_srm,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                           = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(rishi2_rrm,aes(x=factor(rishi2_rrm$BHK_NO.),y=RATIO,fill=factor(rishi2_rrm$BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                                             = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
        
      })})
    
    observeEvent(
      input$Guwahati,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_g,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        else if(input$Univa=="Area"){
          
          ggplot(s_g,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_g,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    
    observeEvent(
      input$New_Delhi,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_d,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        
        else if(input$Univa=="Area"){
          
          ggplot(s_d,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_d,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    observeEvent(
      input$Bangalore,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_b,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        
        else if(input$Univa=="Area"){
          
          ggplot(s_b,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_b,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    observeEvent(
      input$Mumbai,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_m,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        
        else if(input$Univa=="Area"){
          
          ggplot(s_m,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_m,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    observeEvent(
      input$Pune,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_p,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        
        else if(input$Univa=="Area"){
          
          ggplot(s_p,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_p,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    observeEvent(
      input$Chennai,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_c,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        
        else if(input$Univa=="Area"){
          
          ggplot(s_c,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_c,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    observeEvent(
      input$Nagpur,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_n,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        
        else if(input$Univa=="Area"){
          
          ggplot(s_n,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_n,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    observeEvent(
      input$Kolkata,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_k,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        
        else if(input$Univa=="Area"){
          
          ggplot(s_k,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_k,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    observeEvent(
      input$Jaipur,{output$plot3 <- renderPlot({
        ##
        if(input$Univa=="Price"){
          
          ggplot(s_j,aes(x=factor(BHK_NO.),y=PRICE_IN_LACS,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                        = guide_legend(title = "BHK No.s"))+labs(y= "Price", x = "BHK No.s")}
        
        else if(input$Univa=="Area"){
          
          ggplot(s_j,aes(x=factor(BHK_NO.),y=SQUARE_FT,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                    = guide_legend(title = "BHK No.s"))+labs(y= "Area", x = "BHK No.s")}
        
        else if(input$Univa=="Area_Per_Lakh_Rs"){
          
          ggplot(s_j,aes(x=factor(BHK_NO.),y=RATIO,fill=factor(BHK_NO.)))+geom_boxplot()+guides(fill 
                                                                                                = guide_legend(title = "BHK No.s"))+labs(y= "Area_Per_Lakh_Rs", x = "BHK 
No.s")}
        
      })})
    
  }
)
shinyApp(ui, server)

