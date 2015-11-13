library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

shinyUI(fluidPage(
        

        titlePanel("Seattle Crime Microscope"),
        
        fluidRow(
                column(4, h3("How is crime changing in Seattle?"),
                       p("This tool uses Seattle Police Incident Reports from 2010 to 2015. 
                         In that time period there have been over 600,000 reported crimes across the city."),
                       p("",
                         a("Source Data", 
                           href = "https://data.seattle.gov/Public-Safety/Seattle-Police-Department-911-Incident-Response/3k2p-39jp")),
  
                       selectInput("crime",
                                   label = "Which crime?",
                                   choices = crimeChoices,
                                   selected = "Burglaries"),
                       selectInput("area",
                                   label = "Select a Precinct: See map below",
                                   choices = list("B","C","D","E","F","G","J","K","L","M","N","O","Q","R","S","U","W"
                                   )),
                       dateInput('from',
                                 label = 'Start Date: yyyy-mm-dd',
                                 value = "2010-01-01"
                       ),
                       dateInput('to',
                                 label = 'End Date: yyyy-mm-dd',
                                 value = "2015-01-01"
                       ),
                       h3("Seattle Precincts"),
                       img(src = "precinctmap.png", height = 500, width = 350)
                ),
                column(8,
                       fluidRow(
                               column(12,
                                      plotOutput("crimePlot"),
                                      fluidRow(
                                              column(6,
                                                      plotOutput("byHour")),
                                              column(6,
                                                      plotOutput("acrossCity"))
                                                )
                                      )
                       )
                )
        )
))



