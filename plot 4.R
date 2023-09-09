# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
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

# Subset NEI data to include only rows where SCC$EI.Sector column has combustion coal values
coal_df <- grepl("Comb.*Coal", SCC$EI.Sector)
coal_SCC <- SCC[coal_df, ]
coal <- NEI[(NEI$SCC %in% coal_SCC$SCC), ]
rm(coal_df, coal_SCC, NEI, SCC)

# Calculate sum of Combustion Coal Emissions in Kilotons by year
coal <- aggregate(Emissions ~ year, data = coal, FUN = sum)

# Generate PNG file
png("plot4.png")

# Generate Plot
g <- ggplot(coal, aes(x = year, y = Emissions)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Emissions (in Tons) from Coal Combustion-Related Sources") +
  scale_x_date(breaks = as.Date(c("1999-09-08", "2002-09-08", 
                                  "2005-09-08", "2008-09-08")), 
               labels = c("'99", "'02", "'05", "'08")) +
  labs(x = "Year", y = "PM2.5 Emissions (in Tons)")
g

# Close PNG device
dev.off()

# ----------------------------------------------------------------------
# The downward trend in the plot above supports the PM2.5 emissions from coal combustion-related sources have gone down from 1999 to 2008.