library(data.table)
library(ggplot2)

plot6 <- function() {
        
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
        ### rename fips data to be more meaningful
        baltimoreMVs$fips <- "Baltimore City"

        ### Set year as an ordered factor to assist x-axis grouping later
        LAMVs$year <- factor(LAMVs$year,
                             levels = c("1999", "2002", "2005", "2008"),
                             ordered = TRUE)
        ### rename fips data to be more meaningful
        LAMVs$fips <- "Los Angeles County"
        
        mergedData <- rbind(baltimoreMVs, LAMVs)
        mergedData <- data.table(mergedData)
        aggregate <- mergedData[, list(Location = fips, Emissions = sum(Emissions)), by = list(year, fips)]
        
        g <- ggplot(data = aggregate,
                    mapping = aes(x = year, y = Emissions))
        g <- g + layer(geom = "bar", 
                       mapping = aes(fill = Location), 
                       position = "dodge", 
                       stat = "identity")
        g <- g + ylab("PM2.5 Emissions, in tons")
        g <- g + xlab("Year")
        g <- g + ggtitle("Total PM2.5 Emissions from Motor Vehicles:\nBaltimore City vs Los Angeles County")
                
        ggsave("plot6.png", height = 6, width = 6, dpi = 80)

}