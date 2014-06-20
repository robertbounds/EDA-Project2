library(data.table)
library(ggplot2)

plot6 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        baltimoreCity <- NEI[which(NEI$fips == "24510"), ]
        LAcounty      <- NEI[which(NEI$fips == "06037"), ]
        
        baltimoreMVs <- baltimoreCity[which(baltimoreCity$type == "ON-ROAD"), ]
        LAMVs <- LAcounty[which(LAcounty$type == "ON-ROAD"), ]

        baltimoreMVs$year <- factor(baltimoreMVs$year,
                                    levels = c("1999", "2002", "2005", "2008"),
                                    ordered = TRUE)
        baltimoreMVs$fips <- "Baltimore City"

        LAMVs$year <- factor(LAMVs$year,
                             levels = c("1999", "2002", "2005", "2008"),
                             ordered = TRUE)
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