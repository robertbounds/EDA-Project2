library(ggplot2)

plot3 <- function() {
        
        NEI <- readRDS("summarySCC_PM25.rds")
        
        # Baltimore City is given by fips = 24510
        baltimore <- NEI[which(NEI$fips == 24510), ]
        
        # Order Baltimore DF's elements by type
        baltimore <- baltimore[order(baltimore$type), ]
        
        # ordering the factors of year eliminated numerical values on x-axis
        baltimore$year <- factor(baltimore$year, levels = c("1999", "2002", "2005", "2008"), ordered = TRUE)

        # construct barplot of data, subsetted by type
        g <- ggplot(data = baltimore, mapping = aes(x = year, y = Emissions))
        g <- g + layer(geom = "bar", mapping = aes(fill = type), stat = "identity")
        
        
        # further modify plot: main, x- and y-axis labels.

        

        

}