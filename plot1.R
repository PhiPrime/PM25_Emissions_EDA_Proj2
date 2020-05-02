#Question: Have total emissions from PM2.5 decreased in the United States from
#       1999 to 2008? Using the base plotting system, make a plot showing
#       the total PM2.5 emission from all sources for each of the years
#       1999, 2002, 2005, and 2008.

#Plan: Create a simple line chart with the total emissions on the y-axis
#       and year on the x-axis

if (length(ls()) == 0 || !any(grepl("NEI", ls()))) {
        #Prevents waiting when re-running code
        NEI <- readRDS("./summarySCC_PM25.rds")

        #There were two outliers for Emissions, 646952.0 and 112619.8
        #by investigating Source Classification Code (SCC) I found these
        #readings were both from Wildfires as such the high readings
        #make sense and are kept in the data set
}

#Open png & set dimensions
phi <- (1+sqrt(5))/2
png("./plot1.png", height = 480, width = 480*phi)

#Get sum of emissions per year
totals <- tapply(NEI$Emissions, as.factor(NEI$year), sum)

#Generate plot
par(mar = c(5, 5, 3, 2))
x <- names(totals)
y <- totals/1e+06

plot(x, y,
     main = expression(
             'Total Weight of PM'[2.5]*' Per Year'),
     ylab = expression(
             'Total Weight PM'[2.5]*' (Million U.S. Tons)'),
     ylim = c(3,8),
     xlab = "Year", xaxt = "n",
     pch = 19)

#Set axis labels for each year
axis(1, at = x, labels = x, las = 1)

#Connect points with a line
lines(x, y)

#Label points
text(x, y + 0.3, labels = round(y, digits = 4))

#Close png device
dev.off()
