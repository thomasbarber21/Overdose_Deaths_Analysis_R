# Load the data and necessary libraries ----
data <- read.csv("data.csv")

library("maps")
library("mapproj")
library(RColorBrewer)

# Clean the Data ----
data$year <- as.numeric(substr(data$Year,1,4))
midpoint <- function(x) {
  if (length(strsplit(x, "-")[[1]]) == 2) {
    y <- as.numeric(strsplit(x, "-")[[1]])   # need the index [[1]] to take first element of list
    return(mean(y))                         # Calculates the mean of the given ranges
  }
  if (substr(x, 1, 1) == "<") {
    z <- as.numeric(substr(x, 2, 2))
    return(z/2)                           # Calculates the mean for < values
  }
  if (substr(x, 3, 3) == "+") {
    w <- as.numeric(substr(x, 1, 2))
    return(w)                          # Returns values for + category
  }
}

# Applies function over data 
data$overdose_deathrate <- sapply(data$Estimated.Age.adjusted.Death.Rate..16.Categories..in.ranges., midpoint)

# Plotting the Data ----

# Sets breaks and colors exactly as the NY Times article
breaks <- c(0, 4, 8, 12, 16, 20)
leg.txt <- c("0", "4", "8", "12", "16", "20")
colors <- c("#a7c9ea","#deecf7", "#f8eec1", "#ffc065", "#f9842c", "#d91e09")

# Begin writing the png file to the drive
png("NY_Times_Image_Replication.png", width = 1200, height = 500)

layout(matrix(c(0, 0, 1, 1, 0, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
                17, 18, 19), nrow = 4, ncol = 6, byrow = TRUE), heights = c(0.035, 0.15, 0.15, 0.15))

# Sets margins for legend
par(mar=c(0.25, 1.5, 2.5, 1.5))
xvalue <- seq(0, 24, 1)
yvalue <- seq(0, 24, 1)

# Create a plot for the legend
plot(xvalue, yvalue, type = "n", axes=F, xlab="", ylab="", ylim = c(0, 0.5))

rect(xleft=0, xright=4, ybottom=0, ytop=0.5, border="transparent", col="#a7c9ea")
rect(xleft=4, xright=8, ybottom=0, ytop=0.5, border="transparent", col="#deecf7")
rect(xleft=8, xright=12, ybottom=0, ytop=0.5, border="transparent", col="#f8eec1")
rect(xleft=12, xright=16, ybottom=0, ytop=0.5, border="transparent", col="#ffc065")
rect(xleft=16, xright=20, ybottom=0, ytop=0.5, border="transparent", col="#f9842c")
rect(xleft=20, xright=24, ybottom=0, ytop=0.5, border="transparent", col="#d91e09")
axis(1, las=1, at=seq(4,20,4), tck=-.01, lwd.tick=1, lwd=0, labels=T)
abline(v=seq(4,20,4), lty=1)
mtext("Overdose deaths per 100,000", side=3, line=1, font=2)

# sets parameter margins for the map plots, and plots them in similar fashion shown in class, but over a loop for 1999-2015
par(mar=c(0.01, 0.01, 0.01, 0.01))

for (year in unique(data$year)[1:12]) {
  data_year <- data[data$year == year, ]
  data_year$colorBuckets <- as.numeric(cut(data_year$overdose_deathrate, breaks))
  data_year$colorBuckets <- findInterval(data_year$overdose_deathrate, breaks)
  
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
  colorsmatched <- data_year$colorBuckets[match(cnty.fips, data_year$FIPS)]
  
  map("county", col = colors[colorsmatched], fill = TRUE,  lty = 1, projection = "polyconic", border = colors[colorsmatched])
  
  text(0.1, 0.47, as.character(year), col = "black", cex = 1.5, font = 1)
}

# Creating another loop for last 5 years to prevent repeating plots
for (year in unique(data$year)[13:17]) {
  data_year <- data[data$year == year, ]
  data_year$colorBuckets <- as.numeric(cut(data_year$overdose_deathrate, breaks))
  data_year$colorBuckets <- findInterval(data_year$overdose_deathrate, breaks)
  
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
  colorsmatched <- data_year$colorBuckets[match(cnty.fips, data_year$FIPS)]
  
  map("county", col = colors[colorsmatched], fill = TRUE,  lty = 1, projection = "polyconic", border = colors[colorsmatched])
  
  text(0.1, 0.47, as.character(year), col = "black", cex = 1.5, font = 1)
}

dev.off()
