## 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# ----------------------------------------------------------------------

# Load Libraries
library(dplyr)

# Download Data
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "DataForPeerAssessment.zip")
unzip("DataForPeerAssessment.zip")
file.remove("DataForPeerAssessment.zip")

# Read in Data
NEI <- readRDS("summarySCC_PM25.rds")
NEI$year <- as.Date(as.character(NEI$year), "%Y")
class(NEI$year)

# Calculate Emissions by Year in Kilotons
Emissions_by_Year <- data.frame(NEI$Emissions, NEI$year) %>%
  aggregate(NEI.Emissions ~ NEI.year, sum) %>%
  mutate(NEI.Emissions = NEI.Emissions / 1000)

# Remove NEI data frame
rm(NEI)

# Generate PNG file
png("plot1.png")

# Generate Plot
par(mar = c(5, 5, 3, 2))
plot(Emissions_by_Year$NEI.year, Emissions_by_Year$NEI.Emissions, 
     type = "b", pch = 19, col = factor(Emissions_by_Year$NEI.year), 
     main = "Total PM2.5 Emissions (in Kilotons) by Year",
     xaxt = "n", xlab = "Year", 
     ylim = c(0, 8000), 
     ylab = "PM2.5 Emissions (in Kilotons)")
axis(1, at = c(unique(Emissions_by_Year$NEI.year)), 
     labels = c("1999", "2002", "2005", "2008"))
legend(x = "topright", pch = 19, 
       col = factor(Emissions_by_Year$NEI.year),
       legend = c(paste("1999 - ", 
                        round(Emissions_by_Year[1, 2]), "kt"),
                  paste("2002 - ", 
                        round(Emissions_by_Year[2, 2]), "kt"),
                  paste("2005 - ", 
                        round(Emissions_by_Year[3, 2]), "kt"),
                  paste("2008 - ", 
                        round(Emissions_by_Year[4, 2]), "kt")))

# Close PNG device
dev.off()

# ----------------------------------------------------------------------
# The downward trend in the plot above suggests total emissions of PM2.5 have been decreasing from 1999 to 2008.