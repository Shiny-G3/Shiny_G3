
library(shiny)
library(shinythemes)
library(readr)
library(rjson)
library(leaflet)
library(MAP)
library(mapproj)
library(maps)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(qdapRegex)
library(DT)



crime <- read_csv(file = "crime2022.csv")

crime <- crime %>% filter(Lat!=0, Long!=0) %>%
  mutate(OFFENSE_DESCRIPTION=str_to_title(OFFENSE_DESCRIPTION))



map0 <- st_read('City_of_Boston_Boundary.geojson')
# plot(map0)

basemap <- ggplot()+
  geom_sf(data=map0,colour='#0C090A',fill=NA)+
  theme_bw()
basemap

crimetype<-crime %>% 
  group_by(OFFENSE_DESCRIPTION)	%>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  slice_head(n=10)

crimedistrict<-crime %>% 
  group_by(OFFENSE_DESCRIPTION)	%>%
  group_by(DAY_OF_WEEK) %>%
  group_by(DISTRICT) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  slice_head(n=10)


ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">CRIME 2022</a>'), id="nav",
                 windowTitle = "Crime2022",
                 tabPanel("Map", 
                          selectInput('offensetype',' topoffense',
                                      choices=crimetype$OFFENSE_DESCRIPTION),
                          fluidRow(plotOutput("offensemap", width=700,height=700)),
                 ),
                 tabPanel("Table",
                          fluidPage(
                            DT::dataTableOutput("crime"))),
                 
                 tabPanel("Location",
                          sidebarLayout(
                            sidebarPanel(
                              titlePanel("Location of Crimes"),
                              fluidRow(column(4, checkboxGroupInput(
                                inputId = 'District',
                                label = 'Select one District',
                                choices = c("A1", "A7", "A15","B2", "B3", "C6", "C11",
                                            "D4","D14","E5", "E13", "E18","External", ""),
                                selected = "A1")),
                                column(4,checkboxGroupInput(
                                  inputId ='Day',
                                  label = 'Select one Day',
                                  choices = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                              "Friday", "Saturday", "Sunday"),
                                  selected = "Monday")),
                                column(10, selectInput(
                                  inputId = "typeofoffense",
                                  label = "Select type of offense",
                                  choices = unique(crime$OFFENSE_DESCRIPTION),
                                  selected = "Investigate Person")),
                                tableOutput("District_Table"))),
                            mainPanel(fluidRow(plotOutput("District_Plot", width=700,height=700)))))
)


server <- function(input, output){
  output$offensemap <- renderPlot({
    crime1<-crime %>% filter(OFFENSE_DESCRIPTION==input$offensetype) %>%
      group_by(Long,Lat) %>%
      summarise(n=n())
    totalN<-crimetype[match(input$offensetype,crimetype$OFFENSE_DESCRIPTION),'count']
    
    basemap+
      geom_point(mapping=aes(x=Long,y=Lat,color=n,size=n),data=crime1,alpha=1,pch=20)+
      scale_colour_gradientn(colors=c(rev(brewer.pal(5,'YlGnBu')),brewer.pal(15,'PuRd')))+
      labs(x='',y='',title=paste0('Distribution of ',totalN,' "',input$offensetype,'s" in Boston in 2022'))+
      scale_size_continuous(range=c(0.6,6))
  })
  
  output$crime <-DT::renderDataTable(datatable(
    crime[,c(-3,-9:-17)],filter = 'top',
    colnames = c("INCIDENT_NUMBER", "OFFENSE_CODE", "OFFENSE_DESCRIPTION", "DISTRICT", "REPORTING_AREA", "SHOOTING","OCCURRED_ON_DATE")
  ))
  
  output$District_Table<- renderTable({
    crime3<-crime %>% 
      filter(DISTRICT==input$District) %>%
      filter(DAY_OF_WEEK==input$Day) %>%
      filter(OFFENSE_DESCRIPTION==input$typeofoffense) %>%
      group_by(OFFENSE_DESCRIPTION)%>%
      summarise(count=n()) %>%
      arrange(desc(count))
  })
  
  output$District_Plot <- renderPlot({
    crime4 <- crime %>% 
      filter(DISTRICT==input$District) %>%
      filter(DAY_OF_WEEK==input$Day) %>%
      filter(OFFENSE_DESCRIPTION==input$typeofoffense) %>%
      group_by(Long,Lat) %>%
      summarise(n=n())
    totalN<-crimedistrict[match(input$District,
                        input$Day,
                        input$typeofoffense,
                        crimedistrict$DISTRICT),'count']
    
    basemap+
      geom_point(mapping=aes(x=Long,y=Lat,color=n,size=n),data=crime4,alpha=1,pch=20)+
      scale_colour_gradientn(colors=c(rev(brewer.pal(5,'YlGnBu')),brewer.pal(15,'PuRd')))+
      labs(x='',y='',title=paste0('Distribution of Crimes in Boston in 2022'))+
      scale_size_continuous(range=c(0.6,6))
  })
}

shinyApp(ui = ui, server = server)



