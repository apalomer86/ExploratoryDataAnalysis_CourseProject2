# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
# ----------------------------------------------------------------------

# Load Libraries
library(dplyr)
library(ggplot2)

# Download Data for Peer Assessment
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "DataForPeerAssessment.zip")
unzip("DataForPeerAssessment.zip")
file.remove("DataForPeerAssessment.zip")

# Read in Data
NEI <- readRDS("summarySCC_PM25.rds")
NEI$year <- as.Date(as.character(NEI$year), "%Y")
baltimore <- NEI[NEI$fips == "24510", ]
baltimore <- data.frame(baltimore$Emissions, baltimore$year, 
                        baltimore$type)
baltimore <- rename(baltimore, Emissions = baltimore.Emissions, 
                    year = baltimore.year, type = baltimore.type)
        

# Calculate sum of Emissions by year + type
baltimore <- aggregate(Emissions ~ year + type, data = baltimore, 
                       FUN = sum)
rm(NEI)

# Generate PNG file
png("plot3.png")

# Generate Plot
g <- ggplot(baltimore, aes(x = year, y = Emissions, color = type)) +
        geom_point(alpha = 0.5) +
        facet_wrap(~ type, nrow = 1) +
        geom_smooth(method = "lm", se = FALSE) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("PM2.5 Emissions (in Tons) in Baltimore, 
                MD by Year + Type ")+
        scale_x_date(breaks = as.Date(c("1999-09-08", "2002-09-08", 
                                        "2005-09-08", "2008-09-08")), 
                     labels = c("'99", "'02", "'05", "'08")) + 
        labs(x = "Year", y = "PM2.5 Emissions (in Tons)")
g

# Close PNG device
dev.off()

# ----------------------------------------------------------------------
# Of the four types of emission sources, non-road, nonpoint, and on-road sources show a decrease in PM2.5 emissions levels from 1999 - 2008. This is supported the decreasing linear regression lines in the plots above. On the other hand, point type emission sources showed increasing levels of PM2.5 emissions from 1999 - 2008. This is supported by the inreasing regression line in the plot above.