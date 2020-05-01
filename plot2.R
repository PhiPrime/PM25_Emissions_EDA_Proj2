#Question: Have total emissions from PM2.5 decreased in the Baltimore City,
#       Maryland (fips == "24510") from 1999 to 2008?
#       Use the base plotting system to make a plot answering this question.

#Plan: Create a simple line chart with the total emissions on the y-axis
#       and year on the x-axis

if (length(ls()) == 0 || !any(grepl("NEI", ls()))) {
        #Prevents waiting when re-running code
        NEI <- readRDS("./summarySCC_PM25.rds")
}

#Open png & set dimensions
phi <- (1+sqrt(5))/2
png("./plot2.png", height = 480, width = 480*phi)

#Get sum of emissions per year
baltimore <- subset(NEI, NEI$fips == "24510")
totals <- tapply(baltimore$Emissions, as.factor(baltimore$year), sum)

#Generate plot
par(mar = c(5, 5, 3, 2))
x <- names(totals)
y <- totals

#Adjusting y's range because I didn't like the default
yrange <- c(100*floor(min(y)/100), 100*ceiling(max(y)/100) + 100)

plot(x, y,
     main = expression(
             'Total Weight of PM'[2.5]*' Per Year in Baltimore City'),
     ylab = expression(
             'Total Weight PM'[2.5]*' (U.S. Tons)'),
     ylim = yrange, yaxt = "n",
     xlab = "Year", xaxt = "n",
     pch = 19)

#Set axis labels for each year
axis(1, at = x, labels = x, las = 1)

#Manually set labels on yaxis
axis(2, seq(yrange[1], yrange[2], by = 400))

#Connect points with a line
lines(x, y)

#Label points
text(x, y + rep(c(100, 150), times = 2), labels = round(y, digits = 1))


#Close png device
dev.off()
