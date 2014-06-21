plot1 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        
        years <- unique(NEI$year)
        
        ## extract emissions data by year
        emissions99 <- NEI[which(NEI$year == 1999), ]$Emissions
        emissions02 <- NEI[which(NEI$year == 2002), ]$Emissions
        emissions05 <- NEI[which(NEI$year == 2005), ]$Emissions
        emissions08 <- NEI[which(NEI$year == 2008), ]$Emissions
        
        ## apply sum to emissions data
        emissionTotals <- c(sum(emissions99),
                            sum(emissions02),
                            sum(emissions05),
                            sum(emissions08)
                            )
        
        ## add years as names to emissions totals
        names(emissionTotals) <- years
        
        png(file = "plot1.png", width = 720, height = 720)
        
        ## plot emissionsTotals using barplot from base plottting system
        barplot(emissionTotals,
                main = "Total U.S. PM2.5 Emissions",
                ylab = "PM2.5 emissions, in tons",
                xlab = "Year",
                col = "seagreen3"
                )
        
        dev.off()
}