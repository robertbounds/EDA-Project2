library(data.table)
library(ggplot2)
library(scales)
library(quantmod)

plot6.3 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        ### subset NEI data for Baltimore City
        ### and Los Angeles by their given fips values
        baltimoreCity <- NEI[which(NEI$fips == "24510"), ]
        LAcounty      <- NEI[which(NEI$fips == "06037"), ]
        
        ### subset motor vehicle data from Baltimore and Los Angeles data
        baltimoreMVs <- baltimoreCity[which(baltimoreCity$type == "ON-ROAD"), ]
        LAMVs <- LAcounty[which(LAcounty$type == "ON-ROAD"), ]
        
        ### Set year as an ordered factor to assist x-axis grouping later
        baltimoreMVs$year <- factor(baltimoreMVs$year,
                                    levels = c("1999", "2002", "2005", "2008"),
                                    ordered = TRUE)
        ### rename fips values to be more meaningful
        baltimoreMVs$fips <- "Baltimore City"

        ### Set year as an ordered factor to assist x-axis grouping later
        LAMVs$year <- factor(LAMVs$year,
                             levels = c("1999", "2002", "2005", "2008"),
                             ordered = TRUE)
        ### rename fips values to be more meaningful
        LAMVs$fips <- "Los Angeles County"
        
        mergedData <- rbind(baltimoreMVs, LAMVs)
        mergedData <- data.table(mergedData)
        aggregate <- mergedData[, list(Location = fips, Emissions = sum(Emissions)), by = list(year, fips)]
        
        LAemissions <- aggregate[which(aggregate$Location == "Los Angeles County"), ]$Emissions
        BCemissions <- aggregate[which(aggregate$Location == "Baltimore City"), ]$Emissions
        
        LA_1 <- LAemissions[1]
        BC_1 <- BCemissions[1]
        LAemissions <- 1 - LAemissions/sum(LAemissions)
        BCemissions <- 1 - BCemissions/sum(BCemissions)
        aggregate[which(Location == "Baltimore City")]$Emissions <- BCemissions
        aggregate[which(Location == "Los Angeles County")]$Emissions <- LAemissions

        
        g <- ggplot(data = aggregate,
                    mapping = aes(x = year, y = Emissions))
        g <- g + layer(geom = "bar", 
                       mapping = aes(fill = Location), 
                       position = "dodge", 
                       stat = "identity")
        #g <- g + scale_y_continuous(labels = percent)
        g <- g + ylab("% Change of PM2.5 Emissions")
        g <- g + xlab("Year")
        g <- g + ggtitle("Relative Changes of PM2.5 Emissions from Motor Vehicles:\nBaltimore City vs Los Angeles County")
                
        ggsave("plot6-3.png", height = 8, width = 8, dpi = 80)

}