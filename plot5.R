library(data.table)
library(ggplot2)

plot5 <- function() {
        
        ### A quick Google search for 'definition of motor vehicle'
        ### brings up a definition:
        ###
        ###     "a road vehicle powered by an 
        ###      internal combustion engine; an automobile."
        ###
        ### That is the definition I applied to the data.
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        ### Baltimore City data from NEI
        ### National motor vehicle emissions data from SCC
        baltimoreCity <- NEI[which(NEI$fips == 24510), ]
        onroad        <- SCC[which(SCC$Data.Category == "Onroad"), ]
        
        ### Convert DFs to data tables and define common key for merging
        baltimoreCity <- data.table(baltimoreCity, key = "SCC")
        onroad        <- data.table(onroad, key = "SCC")
        
        ### use data table merge to attain table of 
        ### Baltimore City Motor Vehicle data
        baltimoreMVs <- baltimoreCity[onroad]
        
        ### perform light data scrubbing
        baltimoreMVs <- baltimoreMVs[which(!is.na(baltimoreMVs$year)), ]
        
        
        baltimoreMVs$year <- factor(baltimoreMVs$year, 
                                    levels = c("1999", "2002", "2005", "2008"),
                                    ordered = TRUE)
        
        g <- ggplot(data = baltimoreMVs,
                    mapping = aes(x = year, y = Emissions))
        g <- g + layer(geom = "bar", mapping = aes(fill = type), stat = "identity")
        g <- g + ylab("Emissions, in tons")
        g <- g + xlab("Year")
        g <- g + ggtitle("PM2.5 Emissions from Motor Vehicles, Baltimore City")
        g <- g + guides(fill = FALSE)
        
        ggsave("plot5.png", height = 6, width = 6, dpi = 80)
}