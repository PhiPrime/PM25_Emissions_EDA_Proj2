#Question: Have total emissions from PM2.5 decreased in the United States from
#       1999 to 2008? Using the base plotting system, make a plot showing
#       the total PM2.5 emission from all sources for each of the years
#       1999, 2002, 2005, and 2008.

#Plan: Create a simple line chart with the total emissions on the y-axis
#       and year on the x-axis

if (length(ls()) == 0 || !grepl("NEI", ls())) {
        #Prevents waiting when re-running code
        NEI <- readRDS("./summarySCC_PM25.rds")
}

totals <- tapply(NEI$Emissions, as.factor(NEI$year), sum)
x <- names(totals)
y <- totals

plot(x, y,
     ylab = expression(
             "Total Weight PM[2.5]*(Troll A offshore drilling platforms)"),
     xlab = "Year",
     pch = 19)
lines(x, y)
