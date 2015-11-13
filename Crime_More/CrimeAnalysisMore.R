# Data Incubator Challenge Question # 3

library(dplyr)
library(lunar)
library(ggplot2)
library(tidyr)
library(ggmap)
library(ggvis)
library(leaflet)
library(shiny)
library(LakeMetabolizer)

## ======= Get data ========= ##

crime <- read.csv("Seattle_Police_Department_Police_Report_Incident.csv", stringsAsFactors = FALSE)

## ====== Clean data ======== ##

crime$Occurred.Date.or.Date.Range.Start <- 
        as.POSIXct(crime$Occurred.Date.or.Date.Range.Start, 
        format = "%m/%d/%Y %I:%M:%S %p")  # Convert times to readable format

crime$dates <- as.Date(crime$Occurred.Date.or.Date.Range.Start)


## ========== HOW DO CRIMES CHANGE OVER THE YEAR? ============ ##
# Group by month and Offense Type

crimeMonthType <- crime %>% 
                        group_by(Month, Summarized.Offense.Description) %>% 
                        summarize(count = n()) %>%
                        arrange(desc(count))

# transforms df to a wide format

data_wide <- spread(crimeMonthType, Month, count)
data_wide[is.na(data_wide)] <- 0 
data_wide$total <- rowSums(data_wide[2:13]) # Find crime totals for sorting
data_wide <- data_wide[order(data_wide$total, decreasing = TRUE),]
data_wide <- data_wide[c(1,3:25),]


## ========= PLOTTING ========= ##
# Graph top 25 most prevalent crimes over time
# Tells how crimes change over time

par(mfrow=c(5,5), mar=c(1,3,3,1))
barplot(as.numeric(data_wide[1,2:13])) # Plot dummy plot to be removed
for (i in 1:nrow(data_wide[,1])) {
        currCrime <- as.numeric( data_wide[i,2:13] )
        
        # Find month with highest count
        maxCnt <- max(currCrime)
        
        # plot
        yOffset <- (maxCnt - currCrime) / 2
        plot(0, 0, type="n", xlim=c(0,12), ylim=c(0, max(currCrime)),
             xlab="", ylab="", axes=FALSE, bty="n", cex.main=0.8, main=data_wide[i,1])
        rect(0:11, yOffset, 1:12, currCrime+yOffset, col="#e26b43", border="white")
}


## ========== DOES THE MOON HAVE AN EFFECT? ============ ##
# Map night time crimes as a function of lunar stage
# Does the moon seem to have an effect on crimes across Seattle

crime$moon <- lunar.phase(crime$Occurred.Date.or.Date.Range.Start, name = TRUE)
crimeMoon <- crime %>% group_by(moon) 

# subset for crimes which occurred at night
crimeNight <- crimeMoon %>% mutate(dark = is.night(Occurred.Date.or.Date.Range.Start, -122)) %>% 
                        filter(dark == TRUE)


## ========= PLOTTING ========= ##
ggplot(crimeNight, aes(x=Longitude, y=Latitude, fill="black")) + facet_grid(~moon) +
        geom_point(size = 0.5) +
        coord_equal() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                              axis.title.x=element_blank(),
                              axis.title.y=element_blank(),legend.position="none",
                              panel.background=element_blank(),panel.border=element_blank(),
                              panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),plot.background=element_blank())

# Rearrange data and get totals by Offense type and moon
crimeNightTotals <- crimeNight %>% group_by(moon, Summarized.Offense.Description) %>% summarize(count = n())
Night_wide <- spread(crimeNightTotals, moon, count)
Night_wide[is.na(Night_wide)] <- 0 
Night_wide$total <- rowSums(Night_wide[2:5])
Night_wide <- Night_wide[order(Night_wide$total, decreasing = TRUE),]

totals <- colSums(Night_wide[,c(2:5)])

# How does the center of crime change year by year

center <- crime %>% 
        group_by(Year) %>% 
        filter(Latitude > 46 & Longitude > -123) %>% 
        summarize(count = n(), 
                  long_mean = mean(Longitude), 
                  lat_mean = mean(Latitude), 
                  long_sd = sd(Longitude), 
                  lat_sd = sd(Latitude)) %>%
        filter(count > 1000)

lat_from <- center$lat_mean[1:length(center$lat_mean)-1]
long_from <- center$long_mean[1:length(center$long_mean)-1]
lat_to <- center$lat_mean[2:length(center$lat_mean)]
long_to <- center$long_mean[2:length(center$long_mean)]

lines <- data.frame(cbind(lat_from,long_from,lat_to,long_to))


# Still doesn't plot the lines connecting them
mydf2 <- data.frame(group = c("A", "B"), 
                    lat <- c(lines$lat_from, lines$lat_to), 
                    long <- c(lines$long_from, lines$long_to))

center %>% 
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lng = -122.33, lat = 47.62, zoom = 14) %>% 
        addCircles(lng = ~long_mean, 
                   lat = ~lat_mean, 
                   weight = 1, 
                   radius = ~sqrt(count)/10, 
                   popup = ~as.character(Year)) %>%
        addPolylines(data = mydf2, lng = ~lat, lat = ~long, weight = 3, color = "red", group = ~group)

# Map it

c <- ggplot()

c <- c + geom_point(data=center, aes(x=long_mean, y=lat_mean, size=count)) +  
        geom_segment(data = lines, aes(x = long_from, 
                                           y = lat_from,
                                           xend = long_to,
                                           yend = lat_to,
                                           size = 1), color = "#8EDD65",
                   lineend = "butt", arrow = arrow(length = unit(0.25, "inches"))) + 
        scale_size_continuous(range = c(1,20)) + 
        theme_bw()
c


# How is crime changing over time

totals <- crime %>% 
        filter(dates > "2008-01-01") %>% 
        group_by(dates) %>% 
        summarize(count = n()) %>% 
        ggvis(x = ~dates, y = ~count) %>% 
        layer_points(size := 4) %>% 
        layer_smooths(se = TRUE)
totals

# By lat and long
latlong <- crime %>% 
        filter(dates > "2008-01-01" & District.Sector != "H" & District.Sector != "99" & District.Sector != "") %>%
        group_by(lat = round(Latitude, 3), long = round(Longitude,3)) %>% summarize(count = n())

m <- ggplot(latlong, aes(lat, long)) + geom_raster(aes(fill = count))
m

crimeRegion <- crime %>% 
        filter(dates > "2008-01-01" & District.Sector != "H" & District.Sector != "99" & District.Sector != "") %>% 
        group_by(District.Sector, dates) %>%
        summarize(count = n())
        
g <- ggplot()
g <- g + geom_point(data = crimeRegion, aes(x = dates, y = count), size = 0.2, color = "blue", alpha = 0.1) + 
        facet_wrap(~District.Sector) + theme_bw()
g

UDist <- crime %>% 
        filter(dates > "2008-01-01" & District.Sector == "U") %>% 
        group_by(dates) %>% 
        summarize(count = n()) %>% 
        ggvis(x = ~dates, y = ~count) %>% 
        layer_points(size := 4) %>% 
        layer_smooths(se = TRUE)
UDist


crime_df <- crime[,c(7,5,9,11,12,13,15,16,18,19,20)]
names(crime_df) <- c("Offense", "Offense.Detail", "Occurred", "Block.Location", "District", "Beat",
                      "Longitude", "Latitude", "Month", "Year", "Dates")
saveRDS(crime_df, file = "crime_df.rds")
