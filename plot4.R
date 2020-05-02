#Question: Across the United States, how have emissions from coal
#       combustion-related sources changed from 1999–2008?

#Plan: Create a boxplot for each year for the
#       subset of coal combustion-related sources

library(tidyverse)#This contains ggplot2

if (length(ls()) == 0 ||
    (!any(grepl("NEI", ls())) & !any(grepl("scc", ls())))) {
        #Prevents waiting when re-running code
        NEI <- readRDS("./summarySCC_PM25.rds")
        scc <- readRDS("./Source_Classification_Code.rds")
}

#Open png & set dimensions
phi <- (1 + sqrt(5))/2
png("./plot4.png", height = 480, width = 480*phi)

#Get SCC that correlates to Coal-combustion
coalNames <- unique(scc$EI.Sector[grep("Coal", scc$EI.Sector)])
coalNames <- scc[is.element(scc$EI.Sector, coalNames),]$SCC

#Subset out coal related polution
coal <- filter(NEI, is.element(SCC, coalNames))

#Create a log-based Emission reading and account for 0s
coal <- mutate(coal, log = ifelse(Emissions==0, 0, log10(Emissions)))

#Creating pallete
library(RColorBrewer)
pal <- c(brewer.pal(4, "Dark2"))


#Create plot
plot <- ggplot(coal, aes(year, log, fill = as.factor(year)))
plot +
        geom_boxplot() +
        guides(fill = FALSE) +
        xlab("Year") +
        scale_x_continuous(breaks = unique(coal$year)[1:4]) +
        ylab(expression('log'[10]*'(Weight in U. S. Tons)')) +
        scale_y_continuous(n.breaks = 7) +
        ggtitle(expression(
        'Distribution of PM'[2.5]*' Attributed to Coal Combustion for Each Year'))

#Close png
dev.off()
