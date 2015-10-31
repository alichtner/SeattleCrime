library(dplyr)
library(lunar)
library(ggplot2)
library(tidyr)
library(LakeMetabolizer)

## ======= Get data ========= ##

crime <- read.csv("Seattle_Police_Department_Police_Report_Incident.csv", stringsAsFactors = FALSE)

## ====== Clean data ======== ##

crime <- crime[,c(3,5,6,7,9,12,13,15,16,18,19)]

crime$Occurred.Date.or.Date.Range.Start <- 
        as.POSIXct(crime$Occurred.Date.or.Date.Range.Start, 
        format = "%m/%d/%Y %I:%M:%S %p")  # Convert times to readable format


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

