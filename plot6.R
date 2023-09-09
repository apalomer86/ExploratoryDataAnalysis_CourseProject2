# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037". Which city has seen greater changes over time in motor vehicle emissions?
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
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI data to include only values for Baltimore (fips == "24510") and Los Angeles (fips == "06037")
baltimore <- NEI[NEI$fips == "24510", ]
los_angeles <- NEI[NEI$fips == "06037", ]
NEI_sub <- rbind(baltimore, los_angeles)
rm(NEI, baltimore, los_angeles)

# Subset NEI_sub data to include only rows where SCC$EI.Sector column has vehicle values
vehicle_df <- grepl("*Vehicle", SCC$EI.Sector)
vehicle_SCC <- SCC[vehicle_df, ]
NEI_sub_baltimore <- NEI_sub[NEI_sub$SCC %in% vehicle_SCC$SCC, ]
NEI_sub_los_angeles <- NEI_sub[NEI_sub$SCC %in% vehicle_SCC$SCC, ]
NEI_sub <- rbind(NEI_sub_baltimore, NEI_sub_los_angeles)
rm(vehicle_df, vehicle_SCC, SCC, NEI_sub_baltimore, NEI_sub_los_angeles)

# Calculate sum of Combustion Coal Emissions in Kilotons by year
NEI_sub <- aggregate(Emissions ~ year + fips, data = NEI_sub, FUN = sum)

# Replace fips values with descriptive strings for Baltimore and Los Angeles
NEI_sub$fips
levels(NEI_sub$fips) <- c("Los Angeles", "Baltimore")

# Generate PNG file
png("plot6.png")

# Generate Plot
g <- ggplot(NEI_sub, aes(x = year, y = Emissions, color = fips)) +
  geom_point() +
  facet_wrap(~ fips, nrow = 1) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("PM2.5 Emissions (in Tons) from Motor Vehicle Sources")+
  scale_x_date(breaks = as.Date(c("1999-09-08", "2002-09-08", 
                                  "2005-09-08", "2008-09-08")), 
               labels = c("'99", "'02", "'05", "'08")) + 
  labs(x = "Year", y = "PM2.5 Emissions (in Tons)")
g

# Close PNG device
dev.off()

# ----------------------------------------------------------------------
# The plots above suggest that Los Angeles County, California experienced greater changes over time in motor vehicle emissions than Baltimore, Maryland.