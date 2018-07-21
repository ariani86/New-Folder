#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



    
navbarPage("WineDiscovery", id="nav",
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(2,
                             selectInput("country", "Country", c("All countries"="", sort(unique(as.character(cleantable$Country)))), multiple=TRUE)
                      ),
                      column(2,
                             conditionalPanel("input.country",
                                              selectInput("province", "Province", c("All provinces"=""), multiple=TRUE)
                             )
                      ),
                      column(2,
                             conditionalPanel("input.province",
                                              selectInput("region_1", "Region1", c("All Region1"=""), multiple=TRUE)
                             )
                      ),
                      column(2,
                             conditionalPanel("input.province",
                                              selectInput("region_2", "Region2", c("All Region2"=""), multiple=TRUE)
                             )
                      ),
                      column(2,
                             conditionalPanel("input.province",
                                              selectInput("winery", "Winery", c("All wineries"=""), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(4,
                             #numericInput("minPoints", "Min points", min=0, max=100, value=0)
                             sliderInput("rangePoints", label = "Points Range", min = 0, 
                                         max = 100, value = c(60, 80))
                      ),
                      column(4,
                             #numericInput("minPrice", "Min price", min=0, max=100, value=0)
                             sliderInput("rangePrice", label = "Price Range", min = 0, 
                                         max = max(cleantable$Price, na.rm = T), value = c(10, 50))
                      )
                  
                    ),
                    hr(),
                    DT::dataTableOutput("winetable")
           ),
           tabPanel("Wine Visuals",
                    sidebarPanel(
                      selectInput("histCountry", "Country", c("All countries"="", sort(unique(as.character(cleantable$Country)))), multiple=TRUE),
                      conditionalPanel("input.histCountry",
                                       selectInput("histProvince", "Province", c("All provinces"=""), multiple=TRUE)
                      ),
                      conditionalPanel("input.histProvince",
                                       selectInput("histWinery", "Winery", c("All wineries"=""), multiple=TRUE)
                      )
                    ),
                    mainPanel(
                      plotOutput('priceGraph'),
                      plotOutput('hist'),
                      plotOutput('wordCloud')
                    )
           )
)
