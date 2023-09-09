# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
# ----------------------------------------------------------------------

# Load Libraries
library(dplyr)

# Download Data for Peer Assessment
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "DataForPeerAssessment.zip")
unzip("DataForPeerAssessment.zip")
file.remove("DataForPeerAssessment.zip")

# Read in Data
NEI <- readRDS("summarySCC_PM25.rds")
NEI$year <- as.Date(as.character(NEI$year), "%Y")
baltimore <- NEI[NEI$fips == "24510", ]
rm(NEI)

# Calculate Emissions by Year in Tons
baltimore <- data.frame(baltimore$Emissions, baltimore$year) %>%
  aggregate(baltimore.Emissions ~ baltimore.year, sum)
head(baltimore)

# Generate PNG file
png("plot2.png")

# Generate Plot
par(mar = c(5, 5, 3, 2))
plot(baltimore$baltimore.year, baltimore$baltimore.Emissions, 
     type = "b", pch = 19, col = factor(baltimore$baltimore.year), 
     main = "Total PM2.5 Emissions (in Tons) in Baltimore, MD by Year",
     xaxt = "n", xlab = "Year", 
     ylim = c(0, 3500), 
     ylab = "PM2.5 Emissions (in Tons)")
axis(1, at = c(unique(baltimore$baltimore.year)), 
     labels = c("1999", "2002", "2005", "2008"))
legend(x = "bottomleft", pch = 19, 
       col = factor(baltimore$baltimore.year),
       legend = c(paste("1999 - ", round(baltimore[1, 2]), "t"),
                  paste("2002 - ", round(baltimore[2, 2]), "t"),
                  paste("2005 - ", round(baltimore[3, 2]), "t"),
                  paste("2008 - ", round(baltimore[4, 2]), "t")))

# Close PNG device
dev.off()


# ----------------------------------------------------------------------
# The downward trend in the plot above suggests total emissions of PM2.5 have generally been decreasing in the city of Baltimore, MD from 1999 to 2008, with a slight spike in 2005 that went downwards again afterward.