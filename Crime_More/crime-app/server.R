library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

df <- readRDS("crime_df.rds")
df <- df[df$District != "99",]
pop <- read.csv("precPop.csv")
pop <- pop[,c(1,7)]
pop$Precinct <- as.character(pop$Precinct) 

shinyServer(function(input, output, session) {
        
        crime <- reactive({input$crime})
        area <- reactive({input$area})

        myData <- reactive({
                area <- input$area
                crime <- input$crime
                from <- as.POSIXct(input$from)
                to <- as.POSIXct(input$to)
                
                df %>% filter(District == area, Offense == crime, Occurred >= from, Occurred <= to) %>%
                group_by(Year, Month) %>% 
                summarize(count = n()) %>% 
                ungroup() %>%
                mutate(x = as.POSIXct(paste(Year, Month,"01", sep = "-"), format = "%Y-%m-%d"))
        })

        output$crimePlot <- renderPlot({
            

                data <- myData()
                crime <- crime()
                area <- area()
                

                p <- ggplot(data, aes(x, count))
                p <- p + geom_point(size = 2.0)
                p <- p + stat_smooth(se = TRUE, col = "red", fill = "red", alpha = 0.25)
                p <- p + ggtitle(paste(names(crimeChoices[crimeChoices==input$crime]), "per month", "in Precinct", area, sep = " "))
                p <- p + xlab("Time")
                p <- p + ylab("Number of Crimes") 
                p <- p + theme(axis.text=element_text(size=16),
                               axis.title=element_text(size=16,face="bold"),
                               title=element_text(size = 20, face="bold"),
                               panel.border=element_rect(color = "black", fill = NA),
                               panel.background=element_rect(fill = "white"))
                print(p)    
        })

        myTime <- reactive({
                area <- input$area
                crime <- input$crime
                from <- as.POSIXct(input$from)
                to <- as.POSIXct(input$to)
                
                df %>% filter(District == area, Offense == crime, Occurred >= from, Occurred <= to) %>%
                        group_by(time = hour(Occurred)) %>% 
                        summarize(count = n())
        })       
        
        
        
        output$byHour <- renderPlot({
                
                crime <- crime()
                time <- myTime()
                area <- area()
                
                p <- ggplot(time, aes(time, count))
                p <- p + geom_bar(stat = "identity", fill = "gray")
                p <- p + ggtitle(paste(names(crimeChoices[crimeChoices==input$crime]), "per hour", "in Precinct", area, sep = " "))
                p <- p + xlab("Hour of the Day")
                p <- p + ylab("Total Crimes by Hour")
                p <- p + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14,face="bold"),
                               title=element_text(size = 16, face="bold"),
                               panel.border=element_rect(color = "black", fill = NA),
                               panel.background=element_rect(fill = "white"))
                print(p)
                
                
        })
        
        city <- reactive({
                area <- input$area
                crime <- input$crime
                from <- as.POSIXct(input$from)
                to <- as.POSIXct(input$to)
                
                df %>% filter(Offense == crime, Occurred >= from, Occurred <= to) %>%
                        group_by(District) %>% 
                        summarize(count = n()) %>% 
                        left_join(pop, by = c("District" = "Precinct")) %>% 
                        mutate(normalized = count/Pop) %>%
                        arrange(desc(normalized)) 
                
        })
        
        output$acrossCity <- renderPlot({
                
                city <- city()
                crime <- crime()
                area <- area()
                
                city$District <- factor(city$District, levels = city$District) 
                
                p <- ggplot(city, aes(District, normalized))
                p <- p + geom_bar(stat = "identity", aes(fill = District == area))
                p <- p + scale_fill_manual("", values = c('gray', 'red'), labels = c("City", "Selected Precinct"))
                p <- p + ggtitle(paste(names(crimeChoices[crimeChoices==input$crime]), "across the city", sep = " "))
                p <- p + xlab("Precinct")
                p <- p + ylab("Crimes normalized by population")
                p <- p + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14,face="bold"),
                               title=element_text(size = 16, face="bold"),
                               panel.border=element_rect(color = "black", fill = NA),
                               panel.background=element_rect(fill = "white"),
                               legend.position = c(0.8, 0.8))
                print(p)
                
                
        })
        
        
        
  
})