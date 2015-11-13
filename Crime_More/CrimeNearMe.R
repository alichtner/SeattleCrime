# Crime near me
library(shiny)

CrimeNearMe <- function(area = "U", crime = "BIKE THEFT", from = "2010-01-01", to = "2015-01-01") {
        
        df <- readRDS("crime_df.rds")
        
        df %>% filter(District == area, Offense == crime, Occurred >= from, Occurred <= to) %>%
                group_by(Year, Month) %>% 
                summarize(count = n()) %>% 
                ungroup() %>%
                mutate(x = as.POSIXct(paste(Year, Month,"01", sep = "-"), format = "%Y-%m-%d")) %>%
                ggvis(x = ~x, y = ~count) %>% layer_lines() %>% layer_smooths(se = TRUE)
        
}