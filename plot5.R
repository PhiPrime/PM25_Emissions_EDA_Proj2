#Question: How have emissions from motor vehicle sources changed from
#       1999–2008 in Baltimore City?

#Plan: Create a boxplot for each year for the
#       subset of motor vehicle sources

library(tidyverse)#This contains ggplot2

if (length(ls()) == 0 ||
    (!any(grepl("NEI", ls())) & !any(grepl("scc", ls())))) {
        #Prevents waiting when re-running code
        NEI <- readRDS("./summarySCC_PM25.rds")
        scc <- readRDS("./Source_Classification_Code.rds")
}

#Open png & set dimensions
phi <- (1 + sqrt(5))/2
png("./plot5.png", height = 480, width = 480*phi)

#Get SCC that correlates to Vehicle sources
vehicleNames <- unique(scc$EI.Sector[grep("Vehicles", scc$EI.Sector)])
vehicleNames <- scc %>% select(SCC, EI.Sector) %>%
        filter(is.element(EI.Sector, vehicleNames)) %>%
        mutate(EI.Sector = sub("Mobile+ +-+ +On-Road+ ", "", EI.Sector))

#Subset Baltimore data that has a Vehicle related SCC
condenced <- NEI %>%
        filter(fips == "24510" & is.element(SCC, vehicleNames$SCC))
condenced <- condenced %>%
        mutate(Sector = vehicleNames$EI.Sector[
        as.integer(sapply(condenced$SCC, grep, vehicleNames$SCC))])
condenced <- mutate(condenced,
                    log = ifelse(Emissions == 0, NaN, log10(Emissions)))
# There shouldn't be any Emissions = 0 in condenced but I left this ifelse
        # just in case

#Creating pallete
library(RColorBrewer)
pal <- c(brewer.pal(4, "Dark2"))


#Create plot
plot <- ggplot(condenced, aes(year, log, fill = as.factor(year)))
plot +
        geom_boxplot() +
        guides(fill = FALSE) +
        facet_grid(.~Sector) +
        xlab("Year") +
        scale_x_continuous(breaks = unique(condenced$year)[1:4]) +
        ylab(expression('log'[10]*'(Weight in U. S. Tons)')) +
        scale_y_continuous(n.breaks = 7) +
        ggtitle(expression(
        'Distribution of PM'[2.5]*' Attributed to Vehicles for Each Year'))

#Close png
dev.off()

