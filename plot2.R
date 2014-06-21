plot2 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        
        ## subset Baltimore City data from NEI using the fips code provided
        baltimoreCity <- NEI[which(NEI$fips == 24510), ]
        years         <- unique(baltimoreCity$year)
        
        ## subset the emissions data for each year from the Baltimore baltimore data
        emissions99   <- baltimoreCity[which(baltimoreCity$year == 1999), ]$Emissions
        emissions02   <- baltimoreCity[which(baltimoreCity$year == 2002), ]$Emissions
        emissions05   <- baltimoreCity[which(baltimoreCity$year == 2005), ]$Emissions
        emissions08   <- baltimoreCity[which(baltimoreCity$year == 2008), ]$Emissions
        
        ## sum baltimore emissions data for each year
        baltimoreTotals <- c(sum(emissions99),
                             sum(emissions02),
                             sum(emissions05),
                             sum(emissions08)
                             )
        
        names(baltimoreTotals) <- years
        
        png(file = "plot2.png", width = 720, height = 720)
        
        barplot(baltimoreTotals,
                main = "Total PM2.5 Emissions, Baltimore City",
                ylab = "PM2.5 emissions, in tons",
                xlab = "Year",
                col = "seagreen3"
                )
        
        dev.off()
}