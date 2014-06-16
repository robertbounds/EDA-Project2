plot1 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        
        years       <- unique(NEI$year)
        emissions99 <- NEI[which(NEI$year == 1999), ]$Emissions
        emissions02 <- NEI[which(NEI$year == 2002), ]$Emissions
        emissions05 <- NEI[which(NEI$year == 2005), ]$Emissions
        emissions08 <- NEI[which(NEI$year == 2008), ]$Emissions
        
        emissionTotals <- c(sum(emissions99),
                            sum(emissions02),
                            sum(emissions05),
                            sum(emissions08)
                            )
        
        names(emissionTotals) <- years
        
        png(file = "plot1.png", width = 480, height = 480)
        
        barplot(emissionTotals,
                main = "Total PM2.5 Emissions, Nationwide",
                ylab = "PM2.5 emitted, in tons",
                xlab = "Year",
                col = "seagreen3"
                )
        
        dev.off()
}