library(data.table)
library(ggplot2)

plot4 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        ### Subset coal combustion information
        ### I chose to first subset for coal in the fuel/source column
        ### Then, I subset the coal data for a combustion process
        coal <- SCC[grep("Coal", SCC$SCC.Level.Three), ]
        coal <- coal[grep("Combustion", coal$SCC.Level.One), ]
        
        coal$EI.Sector <- gsub("Fuel Comb - ", "", coal$EI.Sector)
        coal$EI.Sector <- gsub(" - Coal", "", coal$EI.Sector)
        
        ### this data being removed had such a tiny total emissions amount
        ### that there was no visible color bar on the plot.
        ### I removed this data because leaving it in created blanks in the
        ### plot, disrupted the experience.
        coal <- coal[-which(coal$EI.Sector == "Electric Generation - Other"), ]

        
        ### Convert DFs to DTs, then merge data on SCC
        coal <- data.table(coal, key = "SCC")
        NEI <- data.table(NEI, key = "SCC")
        mergedTable <- merge(coal, NEI, key = "SCC")
        
        ### Set year as an ordered factor to assist x-axis grouping later
        mergedTable$year <- factor(mergedTable$year, 
                                   levels = c("1999", "2002", "2005", "2008"),
                                   ordered = TRUE)
        
        
        g <- ggplot(data = mergedTable,
                    mapping = aes(x = year, y = Emissions))
        g <- g + layer(geom = "bar", 
                       mapping = aes(fill = EI.Sector),
                       position = "dodge",
                       stat = "identity")
        g <- g + ylab("PM2.5 Emissions, in tons")
        g <- g + xlab("Year")
        g <- g + ggtitle("U.S. Coal Combustion-related, PM2.5 Emissions")
        
        ggsave("plot4.png", dpi = 80, height = 5, width = 7)        
}