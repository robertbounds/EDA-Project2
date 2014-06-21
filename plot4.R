library(data.table)
library(ggplot2)

plot4 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        ############################################################
        ### Extract coal combustion-related emissions information
        ############################################################
        
        coal <- SCC[grep("Coal", SCC$SCC.Level.Three), ]
        coal <- coal[grep("Combustion", coal$SCC.Level.One), ]

        
        ############################################################
        ### Convert DFs to DTs, then merge data
        ############################################################
        
        coal <- data.table(coal, key = "SCC")
        NEI <- data.table(NEI, key = "SCC")
        mergedTable <- merge(coal, NEI, key = "SCC")
        
        ############################################################
        ### Set year as an ordered factor
        ############################################################
        
        mergedTable$year <- factor(mergedTable$year, 
                                   levels = c("1999", "2002", "2005", "2008"),
                                   ordered = TRUE)
        
        ############################################################
        ### Plot
        ############################################################
        
        g <- ggplot(data = mergedTable,
                    mapping = aes(x = year, y = Emissions))
        g <- g + layer(geom = "bar", 
                       mapping = aes(fill = EI.Sector),
                       position = "dodge",
                       stat = "identity")
        g <- g + ylab("Emissions, in tons")
        g <- g + xlab("Year")
        g <- g + ggtitle("Total U.S. Coal Combustion-related Emissions")
        
        ggsave("plot4.png")
}