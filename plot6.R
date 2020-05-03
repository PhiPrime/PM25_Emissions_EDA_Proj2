#Question: Compare emissions from motor vehicle sources in
#       Baltimore City with emissions from motor vehicle sources in
#       Los Angeles County, California (fips == "06037").
#       Which city has seen greater changes over time in
#       motor vehicle emissions?

#Plan: Have '99 be the baseline for each area and make a line chart
#       of change per recording-period ('99-'02, '02-'05, '05-'08)
#       plot both on top of eachother and color code the regions
library(tidyverse)#This contains ggplot2

if (length(ls()) == 0 ||
    (!any(grepl("NEI", ls())) & !any(grepl("scc", ls())))) {
        #Prevents waiting when re-running code
        NEI <- readRDS("./summarySCC_PM25.rds")
        scc <- readRDS("./Source_Classification_Code.rds")
}

#Open png & set dimensions
phi <- (1 + sqrt(5))/2
png("./plot6.png", height = 480, width = 480*phi)

#Get SCC that correlates to Vehicle sources
vehicleNames <- unique(scc$EI.Sector[grep("Vehicles", scc$EI.Sector)])
vehicleNames <- scc %>% select(SCC, EI.Sector) %>%
        filter(is.element(EI.Sector, vehicleNames)) %>%
        mutate(EI.Sector = sub("Mobile+ +-+ +On-Road+ ", "", EI.Sector))

#Subset Baltimore & LA data that has a Vehicle related SCC
condenced <- NEI %>%
        filter((fips == "24510" | fips == "06037") &
                       is.element(SCC, vehicleNames$SCC))
condenced <- condenced %>%
        mutate(Sector = vehicleNames$EI.Sector[
                as.integer(sapply(condenced$SCC, grep, vehicleNames$SCC))],
               log = ifelse(Emissions == 0, NaN, log10(Emissions)),
               City = as.factor(ifelse(#Only two options
                       fips == "24510", "Baltimore", "Los Angeles")))
condenced <- condenced %>%
        group_by(year, City, Sector)

#Get summary of totals
totals <- condenced %>% summarise(total = sum(Emissions)) %>%
        arrange(year, City, Sector)

#Store first year totals
year1Vals <- totals$total[1:8]

#Subtract each of the totals by it's related first year total
totals <- totals %>% ungroup() %>%
        mutate(Change = total - rep(year1Vals, times = 4)) %>%
        unite("City_Sector", City, Sector, remove = FALSE)

#Creating palletes
Baltpal <- c("#241773", "#000000", "#9E7C0C", "#C60C30")
LApal <-  c("#002244", "#866D4B", "#002A5E", "#0080C6")
pal <- c(Baltpal[1], LApal[2])

#Create plot
plot <- ggplot(totals, aes(year, Change))
plot +
        scale_colour_manual(values = pal) +
        facet_wrap(Sector~., scales = "free") +
        geom_point(aes(color = City, shape = City), size = 3) +
        geom_line(aes(color = City), lwd = 2) +
        geom_hline(yintercept = 0, color = "#FF0000") +
        xlab("Year") +
        scale_x_continuous(breaks = unique(condenced$year)[1:4]) +
        ylab(expression('Change in Weight of Pm'[2.5]*' (U.S. Tons)')) +
        scale_y_continuous(n.breaks = 4) +
        ggtitle(expression(
                'Change in PM'[2.5] * ' From 1999' *
                ' Attributed to Vehicles for Each Year in' *
                ' Baltimore and Los Angeles'))

#Close png
dev.off()


