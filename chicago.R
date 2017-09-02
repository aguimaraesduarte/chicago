# Clear all packages
pkgs <- names(sessionInfo()$otherPkgs)
if(!is.null(pkgs)) pkgs <- paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)

# Clean session
rm(list=ls(all = TRUE))
cat("\014")

list.of.packages <- c("ggplot2", "lubridate", "scales", "plyr", "dplyr", "magrittr", "reshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# Set global theme for plots
theme_set(theme_bw(base_size = 14))
theme_update(plot.title = element_text(hjust = 0.5))

# Read in files
setwd('../Downloads/')
HomicidesPerDistrictPerYearMonth <- read.csv("HomicidesPerDistrictPerYearMonth.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,6)]
HomicidesPerYearMonth <- read.csv("HomicidesPerYearMonth.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,5)]
CrimesPerYearMonth <- read.csv("CrimesPerYearMonth.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,5)]
CrimesPerHour <- read.csv("CrimesPerHour.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,4)]
CrimesPerHourPerLocation <- read.csv("CrimesPerHourPerLocation.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,5)]
CrimesPerDistrict <- read.csv("CrimesPerDistrict.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,4)]
CrimesPerDistrictPerYearMonth <- read.csv("CrimesPerDistrictPerYearMonth.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,6)]
CrimesPerLocDesc <- read.csv("CrimesPerLocDesc.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,4)]
CrimesPerLocDescPerYearMonth <- read.csv("CrimesPerLocDescPerYearMonth.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,6)]
CrimesPerPrimaryType <- read.csv("CrimesPerPrimaryType.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,4)]
CrimesPerPrimaryTypePerYearMonth <- read.csv("CrimesPerPrimaryTypePerYearMonth.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,6)]
CrimesPerPrimaryTypePerHour <- read.csv("CrimesPerPrimaryTypePerHour.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,5)]
CrimesBreakdownPct <- read.csv("CrimesBreakdownPct.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,6)]
PctChangeCrimeYoY <- read.csv("PctChangeCrimeYoY.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,8)]
ArrestsPerYearMonth <- read.csv("ArrestsPerYearMonth.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)[,-c(1,5)]


# Plots
######### Crimes and arrests per year
CrimesPerYearMonth$date <- as.Date(paste(CrimesPerYearMonth$date_crime_year, CrimesPerYearMonth$date_crime_month, '01', sep='-'))
ArrestsPerYearMonth$date <- as.Date(paste(ArrestsPerYearMonth$date_crime_year, ArrestsPerYearMonth$date_crime_month, '01', sep='-'))
m <- merge(CrimesPerYearMonth, ArrestsPerYearMonth, 'date')
m <- m[,-c(2,3,5,6)]
colnames(m) <- c('date', 'crimes', 'arrests')
m <- melt(m, 'date')
ggplot(m, aes(x=date, y=value, color=variable)) +
  geom_line(size=1.2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0,0)) +
  scale_color_discrete(name='', labels=c('crimes'='Crimes', 'arrests'='Arrests')) +
  ylab('Crime count') +
  xlab('Year') +
  ggtitle('Total number of crimes and arrests per month in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### Homicides per year
HomicidesPerYearMonth$date <- as.Date(paste(HomicidesPerYearMonth$date_crime_year, HomicidesPerYearMonth$date_crime_month, '01', sep='-'))
ggplot(HomicidesPerYearMonth, aes(x=date, y=cnt)) +
  geom_line(size=1.2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0,0))  +
  ylab('Homicide count') +
  xlab('Year') +
  ggtitle('Total number of homicides per month in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### Crimes per location description
top20loc <- CrimesPerLocDesc[1:20,]
ggplot(top20loc, aes(x=reorder(location_description, cnt), y=cnt)) +
  geom_bar(stat='identity', size=1.2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  ylab('Crime count') +
  xlab('Location description') +
  ggtitle('Total number of crimes per location description in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


######### Plot of crimes per location description (top 4)
top4 <- c('STREET', 'RESIDENCE', 'APARTMENT', 'SIDEWALK')
CrimesPerLocDescPerYearMonth$date <- as.Date(paste(CrimesPerLocDescPerYearMonth$date_crime_year, CrimesPerLocDescPerYearMonth$date_crime_month, '01', sep='-'))
top4loc <- subset(CrimesPerLocDescPerYearMonth, location_description %in% top4)
ggplot(top4loc, aes(x=date, y=cnt, color=location_description)) +
  geom_line(size=1.2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0,0)) +
  scale_color_discrete(name='Location description') +
  ylab('Crime count') +
  xlab('Year') +
  ggtitle('Total number of crimes in specific locations per month in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### Crimes per primary type
top20type <- CrimesPerPrimaryType[1:20,]
ggplot(CrimesPerPrimaryType, aes(x=reorder(primary_type, cnt), y=cnt)) +
  geom_bar(stat='identity', size=1.2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  ylab('Crime count') +
  xlab('Location description') +
  ggtitle('Total number of crimes per primary type in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


######### Plot of crimes per primary type (top 20)
CrimesPerPrimaryTypePerYearMonth$date <- as.Date(paste(CrimesPerPrimaryTypePerYearMonth$date_crime_year, CrimesPerPrimaryTypePerYearMonth$date_crime_month, '01', sep='-'))
top20typeb <- subset(CrimesPerPrimaryTypePerYearMonth, primary_type %in% top20type$primary_type)
ggplot(CrimesPerPrimaryTypePerYearMonth, aes(x=date, y=cnt, color=primary_type)) +
  geom_line(size=1.2) +
  facet_wrap(~primary_type, scales="free_y") +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0,0)) +
  scale_color_discrete(guide=FALSE) +
  ylab('Crime count') +
  xlab('Year') +
  ggtitle('Total number of specific crimes per month in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### Percent change in crime year over year
PctChangeCrimeYoY <- subset(PctChangeCrimeYoY, yearpos!=2017)
PctChangeCrimeYoY$sign <- ifelse(PctChangeCrimeYoY$pctchangeyoy >= 0, "positive", "negative")
ggplot(PctChangeCrimeYoY, aes(x=yearpos, y=pctchangeyoy, fill=sign)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = c("positive" = "darkblue", "negative" = "red"), guide=FALSE) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = seq(min(PctChangeCrimeYoY$yearpos), max(PctChangeCrimeYoY$yearpos), by = 2)) +
  facet_wrap(~ primary_type, scales="free_y") +
  ylab('Percent change year over year') +
  xlab('Year') +
  ggtitle('Percent change year over year for each type of crime in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######## Same, only for homicide
ggplot(subset(PctChangeCrimeYoY, primary_type=='HOMICIDE'), aes(x=yearpos, y=pctchangeyoy, fill=sign)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = c("positive" = "darkblue", "negative" = "red"), guide=FALSE) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = seq(min(PctChangeCrimeYoY$yearpos), max(PctChangeCrimeYoY$yearpos), by = 1)) +
  ylab('Percent change year over year') +
  xlab('Year') +
  ggtitle('Percent change year over year for each type of crime in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### Crimes per district
ggplot(CrimesPerDistrict, aes(x=reorder(district, -cnt), y=cnt)) +
  geom_bar(stat='identity', size=2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  ylab('Crime count') +
  xlab('District') +
  ggtitle('Total number of crimes per district in Chicago')


######### Plot of crimes per district
CrimesPerDistrictPerYearMonth$date <- as.Date(paste(CrimesPerDistrictPerYearMonth$date_crime_year, CrimesPerDistrictPerYearMonth$date_crime_month, '01', sep='-'))
CrimesPerDistrictPerYearMonth$district <- as.factor(CrimesPerDistrictPerYearMonth$district)
validDistricts <- subset(CrimesPerDistrictPerYearMonth, !(district %in% c(0, 13, 21, 23, 31)))
ggplot(validDistricts, aes(x=date, y=cnt, color=district)) +
  geom_line(size=1.2) +
  facet_wrap(~district, scales="free_y") +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0,0)) +
  scale_color_discrete(guide=FALSE) +
  ylab('Crime count') +
  xlab('Year') +
  ggtitle('Total number of crimes per district per month in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### Crimes per hour
ggplot(CrimesPerHour, aes(x=time_crime_hour, y=cnt)) +
  geom_line(size=2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = seq(min(CrimesPerHour$time_crime_hour), max(CrimesPerHour$time_crime_hour), by = 1)) +
  ylab('Crime count') +
  xlab('Hour') +
  ggtitle('Total number of crimes per hour in Chicago')


######### Crimes per hour per location description (top4)
top4hloc <- subset(CrimesPerHourPerLocation, location_description %in% top4)
ggplot(top4hloc, aes(x=time_crime_hour, y=cnt, color=location_description)) +
  geom_line(size=1.2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(24), expand=c(0,0)) +
  scale_color_discrete(name='Location description') +
  ylab('Crime count') +
  xlab('Hour') +
  ggtitle('Total number of crimes in specific locations per hour in Chicago')


######### Crimes per hour per primary type
ggplot(subset(CrimesPerPrimaryTypePerHour, primary_type %in% c('BURGLARY')), aes(x=time_crime_hour, y=cnt, color=primary_type)) +
  geom_line(size=1.2) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(24), expand=c(0,0)) +
  scale_color_discrete(name='Primary type', guide=FALSE) +
  ylab('Crime count') +
  xlab('Hour') +
  ggtitle('Total number of crimes by type per hour in Chicago')


########## Crime breakdown (percentage)
top20typec <- subset(CrimesBreakdownPct, primary_type %in% top20type$primary_type)
ggplot(CrimesBreakdownPct, aes(x=year, y=percent, color=primary_type)) +
  geom_line(size=1.2) +
  facet_wrap(~primary_type) +
  scale_x_continuous(breaks = pretty_breaks(10), expand = c(0,0)) +
  scale_color_discrete(guide=FALSE) +
  ylab('Crime count (%)') +
  xlab('Year') +
  ggtitle('Percentage count of specific crimes per year in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### Plot of homicides per district
HomicidesPerDistrictPerYearMonth$date <- as.Date(paste(HomicidesPerDistrictPerYearMonth$date_crime_year, HomicidesPerDistrictPerYearMonth$date_crime_month, '01', sep='-'))
HomicidesPerDistrictPerYearMonth$district <- as.factor(HomicidesPerDistrictPerYearMonth$district)
ggplot(HomicidesPerDistrictPerYearMonth, aes(x=date, y=cnt, color=district)) +
  geom_line(size=1.2) +
  facet_wrap(~district) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0,0)) +
  scale_color_discrete(guide=FALSE) +
  ylab('Homicide count') +
  xlab('Year') +
  ggtitle('Total number of homicides per district per month in Chicago') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








########## Forecasting
crimes <- ts(CrimesPerYearMonth[-nrow(CrimesPerYearMonth),]$cnt, start=c(2001,1), frequency=12)
# Holt-Winters
plot(crimes, main = "Monthly crimes in Chicago", ylab = "Crimes", xlab = "Time")
crimes.hw <- HoltWinters(x = crimes, seasonal = "add")
plot(crimes.hw)
plot(forecast(crimes.hw, h = 72))
points(crimes, type = "l", col = "green")

#summary(m.hw)

# Check residuals
e <- crimes.hw$fitted[,1] - crimes
plot(e, main="Residuals vs Time")
abline(h=0, col="red")
qqnorm(e)
qqline(e)

# Forecasting
f <- forecast(crimes.hw, h=72, level=0.95)
l <- ts(f$lower, start = c(2017, 7), frequency = 12)  #95% PI LL
h <- ts(f$upper, start = c(2017, 7), frequency = 12) #95% PI UL
pred <- f$mean #predictions
par(mfrow=c(1,1))
plot(crimes, xlim=c(2001, 2023), ylim=c(10000, 50000), main = "Monthly crimes in Chicago",
     ylab = "Crime", xlab = "Time", lwd=2)
abline(v = 2017.5, lwd = 1, col = "black")
points(pred, type = "l", col = "blue", lwd=2)
points(l, type = "l", col = "red", lwd=2)
points(h, type = "l", col = "red", lwd=2)
points(f$fitted, type="l", col = "orange", lwd=2)
#points(crimes, type = "l", col = "black")
legend("topright", legend = c("Observed", "Fitted", "Predicted", "95% PI"),
       lty = 1, col = c("black", "orange", "blue", "red"), cex = 1, lwd=2)

# SARIMA
plot(crimes, main = "Monthly crime in Chicago", 
     ylab = "Crime", xlab = "Time")
acf(crimes, lag.max = 72)
adf.test(crimes) #need to difference
crimes.1 = diff(crimes)
acf(crimes.1, lag.max = 72)
adf.test(crimes.1) #stop -> d=1
ndiffs(x=crimes, test="adf", max.d=10)
# difference for season with s=12
crimes.1.12 = diff(crimes.1, lag = 12)
acf(crimes.1.12, lag.max = 72)
nsdiffs(crimes, 12)

# this looks good. d=1, D=1, s=12
par(mfrow=c(2,1))
acf(crimes.1.12, lag.max = 48) #q<=1, Q<=2
pacf(crimes.1.12, lag.max = 48) #p<=1, P<=2

# create SARIMA (1,1,1)x(1,1,2)[12]
m <- arima(crimes, order = c(1,1,1), seasonal = list(order = c(1,1,2), period = 12))

# Plot fitted values over the data
par(mfrow=c(1,1))
plot(crimes, main = "SARIMA filtering", 
     ylab = "Observed / Fitted", xlab = "Time")
fit <- crimes-m$residuals
lines(fit, col="red")

summary(m)
auto.arima(crimes, allowdrift = F)

# Check assumptions of model
par(mfrow=c(1,1))
tsdiag(m)
qqnorm(m$residuals)
qqline(m$residuals)

# Forecasting
f <- forecast(m, h=72, level=0.95)
l <- ts(f$lower, start = c(2017, 7), frequency = 12)  #95% PI LL
h <- ts(f$upper, start = c(2017, 7), frequency = 12) #95% PI UL
pred <- f$mean #predictions
par(mfrow=c(1,1))
plot(crimes, xlim=c(2001, 2023), ylim=c(10000, 50000), main = "Monthly crime in Chicago",
     ylab = "Crime", xlab = "Time", lwd=2)
abline(v = 2017.5, lwd = 1, col = "black")
points(pred, type = "l", col = "blue", lwd=2)
points(l, type = "l", col = "red", lwd=2)
points(h, type = "l", col = "red", lwd=2)
points(f$fitted, type="l", col = "orange", lwd=2)
legend("topright", legend = c("Observed", "Fitted", "Predicted", "95% PI"),
       lty = 1, col = c("black", "orange", "blue", "red"), cex = 1, lwd=2)




############## Drawing maps
library(rgdal)
district.shp <- readOGR('PoliceDistrict')
plot(district.shp)
