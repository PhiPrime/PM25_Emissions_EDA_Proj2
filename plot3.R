#Question: Of the four types of sources indicated by the type
#       (point, nonpoint, onroad, nonroad) variable, which of these four
#       sources have seen decreases in emissions from 1999–2008 for
#       Baltimore City?
#       Which have seen increases in emissions from 1999–2008?
#       Use the ggplot2 plotting system to make a plot answer this question.

#Plan: Create a line plot with source of emission determined by color
library(tidyverse)#This contains ggplot2

if (length(ls()) == 0 || !any(grepl("NEI", ls()))) {
        #Prevents waiting when re-running code
        NEI <- readRDS("./summarySCC_PM25.rds")
}

#Open png & set dimensions
phi <- (1 + sqrt(5))/2
png("./plot3.png", height = 480, width = 480*phi)

#Subset baltimore data
baltimore <- subset(NEI, NEI$fips == "24510")

#group data by year&type then get the sums of Emissions
condenced <- baltimore %>%
        group_by(year, type) %>%
        summarise(total = sum(Emissions)) %>%
        mutate(type = factor(type, levels = c("POINT", "NONPOINT",
                                              "ON-ROAD", "NON-ROAD")))

#Creating pallete
library(RColorBrewer)
pal <- c(brewer.pal(7, "Set2"))

#Rearranging pallete order/dropping mids
pal <- c(pal[7], pal[2], pal[3], pal[1])

#Create plot
plot <- ggplot(condenced, aes(year, total))
plot +
        scale_colour_manual(values = pal) +
        geom_point(aes(color = type), size = 3, pch = 19) +
        geom_line(aes(color = type), lwd = 2) +
        xlab("Year") +
        scale_x_continuous(breaks = unique(condenced$year)[1:4]) +
        ylab(expression('Total Pm'[2.5]*' (U.S. Tons)')) +
        ggtitle(expression(
                'Total PM'[2.5]*' by Type in Baltimore for Each Year'))

#Close png
dev.off()
