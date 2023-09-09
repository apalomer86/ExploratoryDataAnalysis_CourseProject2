# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
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
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI data to include only rows where SCC$EI.Sector column has vehicle-related emissions
vehicle_df <- grepl("*Vehicle", SCC$EI.Sector)
vehicle_SCC <- SCC[vehicle_df, ]
vehicle <- baltimore[(baltimore$SCC %in% vehicle_SCC$SCC), ]
rm(vehicle_df, vehicle_SCC, baltimore, NEI, SCC)

# Calculate sum of Combustion Coal Emissions in Kilotons by year
vehicle <- aggregate(Emissions ~ year, data = vehicle, FUN = sum)

# Generate PNG file
png("plot5.png")

# Generate Plot
g <- ggplot(vehicle, aes(x = year, y = Emissions)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Emissions (in Tons) from Vehicle Sources in Baltimore, MD") +
  scale_x_date(breaks = as.Date(c("1999-09-08", "2002-09-08", 
                                  "2005-09-08", "2008-09-08")), 
               labels = c("'99", "'02", "'05", "'08")) +
  labs(x = "Year", y = "PM2.5 Emissions (in Tons)")
g

# Close PNG device
dev.off()

# ----------------------------------------------------------------------
# The downward trend in the plot above supports that PM2.5 emissions from motor vehicle sources have decreased in the years 1999-2008.