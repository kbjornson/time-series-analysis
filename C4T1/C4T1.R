# Install/load packages and import data
install.packages("RMySQL")
install.packages("lubridate")
library(RMySQL)
library(dplyr)
library(lubridate)


# Create a database connection
con = dbConnect(MySQL(), user = 'deepAnalytics', password = 'Sqltask1234!',
                dbname = 'dataanalytics2018', 
                host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List the tables contained in the database
dbListTables(con)

# Using the "iris" table as an example, list the attributes contained in a table
dbListFields(con, 'iris')

# Query the database. We can download all of the data, or choose specific attributes
# Use asterisk to specify all attributes
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

# Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

# Now we will focus on the other tables in the dataset
dbListFields(con, 'yr_2006')

yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                      Sub_metering_3 FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                      Sub_metering_3 FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                      Sub_metering_3 FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                      Sub_metering_3 FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                      Sub_metering_3 FROM yr_2010")

# Take a look at each data frame
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)
#Note: 2006 only encompasses Dec 16 - Dec 31

str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)
#Note: 2010 encompasses Jan 1 - Nov 26

# Combine tables into one dataframe using dplyer
data <- bind_rows(yr_2007, yr_2008, yr_2009)

str(data)
summary(data)
head(data)
tail(data)

# Combine date and time attributes in a new column
data <- cbind(data, paste(data$Date, data$Time), stringsAsFactors = FALSE)

# Change the name of the new datetime column
colnames(data)[6] <- "DateTime"

# Move the datetime attribute within the dataset
data <- data[,c(ncol(data), 1:(ncol(data)-1))]
head(data)

# Convert datetime to a DateTime data type POSIXct. Then add the timezone - France.
data$DateTime <- as.POSIXct(data$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
# Tried Paris at first, but everything was off by 1 hr
# Then tried London time, which fixed it. Then realized UTC was likely time zone.
attr(data$DateTime, "tzone") <- "UTC"

# Inspect the data types
str(data)

# Create a "year" attribute with lubridate
data$year <- year(data$DateTime)

# Create other attributes with lubridate
data$quarter <- quarter(data$DateTime)
data$month <- month(data$DateTime)
data$week <- week(data$DateTime)
data$day <- day(data$DateTime)
data$hour <- hour(data$DateTime)
data$minute <- minute(data$DateTime)
data$weekday <- wday(data$DateTime)

# View a summary of the data
summary(data)

# Plot the average power usage for each sub meter
submeter <- c(mean(data$Sub_metering_1), mean(data$Sub_metering_2), mean(data$Sub_metering_3))
barplot(submeter, names.arg = c("Submeter 1", "Submeter 2", "Submeter 3"), col = rgb(0.2,0.4,0.6,0.6),
        ylab = "Average Power Usage in kW/h", main = "Average Power Usage for Each Submeter")

# Plot a boxplot for each submeter
boxplot(data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3)

# Standard deviation
sd(data$Sub_metering_1)
sd(data$Sub_metering_2)
sd(data$Sub_metering_3)

# Look at average power usage for submeters for month of July and Aug in 2007, 2008, 2009
julyaug2007 <- filter(data, year == 2007 & (month == 7 | month == 8))
mean(julyaug2007$Sub_metering_1) #0.8897753
mean(julyaug2007$Sub_metering_2) #1.182661
mean(julyaug2007$Sub_metering_3) #4.268235

julyaug2008 <- filter(data, year == 2008 & (month == 7 | month == 8))
mean(julyaug2008$Sub_metering_1) #0.5733456
mean(julyaug2008$Sub_metering_2) #0.7465164
mean(julyaug2008$Sub_metering_3) #3.433689

julyaug2009 <- filter(data, year == 2009 & (month == 7 | month == 8))
mean(julyaug2009$Sub_metering_1) #0.5857555
mean(julyaug2009$Sub_metering_2) #0.9140578
mean(julyaug2009$Sub_metering_3) #4.299542

# Find the average power usage of each submeter in July 2007, 2008, 2009

jul2007 <- filter(data, year == 2007 & month == 7)
mean(jul2007$Sub_metering_1)  #0.967265
mean(jul2007$Sub_metering_2) #1.252196
mean(jul2007$Sub_metering_3) #3.481835

jul2008 <- filter(data, year == 2008 & month == 7)
mean(jul2008$Sub_metering_1)  #1.059927
mean(jul2008$Sub_metering_2) #0.9825933
mean(jul2008$Sub_metering_3) #5.105717

jul2009 <- filter(data, year == 2009 & month == 7)
mean(jul2009$Sub_metering_1) #0.4083251
mean(jul2009$Sub_metering_2) #0.9521014
mean(jul2009$Sub_metering_3) #4.21057

# Find the average power usage of each submeter in August 2007, 2008, 2009

aug2007 <- filter(data, year == 2007 & month == 8)
mean(aug2007$Sub_metering_1)  #0.8124748
mean(aug2007$Sub_metering_2) #1.113295
mean(aug2007$Sub_metering_3) #5.052714

aug2008 <- filter(data, year == 2008 & month == 8)
mean(aug2008$Sub_metering_1)  #0.086764
mean(aug2008$Sub_metering_2) #0.5104395
mean(aug2008$Sub_metering_3) #1.76166
#Power usage for Aug 2008 appears to be significantly lower on average than in other years/months

aug2009 <- filter(data, year == 2009 & month == 8)
mean(aug2009$Sub_metering_1) #0.7667832
mean(aug2009$Sub_metering_2) #0.8752429
mean(aug2009$Sub_metering_3) #4.390317


# Create a graph comparing power usage across all submeters in August of each year
august <- bind_rows(aug2007, aug2008, aug2009)
augSub1 <- c(mean(aug2007$Sub_metering_1), mean(aug2008$Sub_metering_1), mean(aug2009$Sub_metering_1))
augSub2 <- c(mean(aug2007$Sub_metering_2), mean(aug2008$Sub_metering_2), mean(aug2009$Sub_metering_2))
augSub3 <- c(mean(aug2007$Sub_metering_3), mean(aug2008$Sub_metering_3), mean(aug2009$Sub_metering_3))

plot_ly(august, x = ~august, y = ~augSub1, type = 'bar', name = 'Submeter 1') %>%
  add_trace(y = ~augSub2, name = 'Submeter 2') %>%
  add_trace(y = ~augSub3, name = 'Submeter 3') %>%
  layout(title = 'Power Consumption for each Submeter in August',
         xaxis = list(title = "", tickvals = c(0, 1, 2), ticktext = c("August 2007", "August 2008", "August 2009")),
         yaxis = list(title = "Average Power Consumption (watt-hours)"))


###########################################################################
# Exploratory Graphical Analysis
###########################################################################

# Plot all of submeter 1
plot(data$Sub_metering_1) #granularity is an issue

# Plot subset of submeter 1 over 1 week period
# Subset the 2nd week of 2008 - all observations
houseWeek <- filter(data, year == 2008 & week == 2)
plot(houseWeek$Sub_metering_1) #granularity needs to be reduced

# Install plotly for the next graphs
install.packages("plotly")
library(plotly)

# Plot subset of submeter 1 over 1 day period
# Subset 9th day of Jan 2008 - all observations
houseDay <- filter(data, year == 2008 & month == 1 & day == 9)
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,
        type = 'scatter', mode = 'lines')

# Now plot all of the submeters for Jan 9, 2008
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption - Jan. 9, 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Redude granularity of plot from one obs/min to one obs/10min
# Subset Jan 9, 2008 - 10 min frequency
houseDay10 <- filter(data, year == 2008 & month == 1 & day == 9 & 
                       (minute == 0 | minute == 10 | minute == 20 | minute == 30 |
                          minute == 40 | minute == 50))

# Plot all submeters on Jan 9, 2008 - 10 min frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Jan. 9, 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power ( watt-hours)"))

# Use plotly to plot all submeters for a week in 2008 (week 10) - 15 min freq
# Subset 10th week of 2008 - 10 min frequency
houseWeek15 <- filter(data, year == 2008 & week == 10 & 
                        (minute == 0 | minute == 15 | minute == 30 | minute == 45))
plot_ly(houseWeek15, x = ~houseWeek15$DateTime, y = ~houseWeek15$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek15$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek15$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for 10th Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Plot 1 day in September (Sept 6)
# Subset 7th day of Sept 2008 - 10 min frequency
houseDaysept <- filter(data, year == 2008 & month == 9 & day == 6 & 
                         (minute == 0 | minute == 30))
plot_ly(houseDaysept, x = ~houseDaysept$DateTime, y = ~houseDaysept$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDaysept$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDaysept$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for September 6, 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Now plot all submeters for week 10 in 2008 for 30 min freq
# Subset 10th week of 2008 - 30 min frequency
houseWeek30 <- filter(data, year == 2008 & week == 10 & (minute == 0 | minute == 30))
plot_ly(houseWeek30, x = ~houseWeek30$DateTime, y = ~houseWeek30$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek30$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek30$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for 10th Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Plot all submeters for second week of Sept 2008, every 30 min (week 36)
houseWeek30_sept <- filter(data, year == 2008 & week == 36 & (minute == 0 | minute == 30))
plot_ly(houseWeek30_sept, x = ~houseWeek30_sept$DateTime, y = ~houseWeek30_sept$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek30_sept$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek30_sept$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for 36th Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Plot all submeters for Sept 2008 - 30 min freq
# Subset 9th month of 2008 - 30 min freq
houseMonth30 <- filter(data, year == 2008 & month == 9 & (minute == 0 | minute == 30))
plot_ly(houseMonth30, x = ~houseMonth30$DateTime, y = ~houseMonth30$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth30$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth30$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for September 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Take a look at submeters for Oct 2008 - 30 min freq
houseMonth30_oct <- filter(data, year == 2008 & month == 10 & (minute == 0 | minute == 30))
plot_ly(houseMonth30_oct, x = ~houseMonth30_oct$DateTime, y = ~houseMonth30_oct$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth30_oct$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth30_oct$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for October 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Percentage of use at various times of day for Submeter3 - Sept 6, 2008
# Subset Sept 6
houseDay_0907 <- filter(data, year == 2008 & month == 9 & day == 6)
plot_ly(houseDay_0907, labels = ~houseDay_0907$hour, values = ~houseDay_0907$Sub_metering_3, type = 'pie',
        textinfo = 'label+percent', textposition = 'inside', insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text+percent', text = ~paste(houseDay_0907$hour,':00'), showlegend = FALSE) %>%
  layout(title = "Power Consumption for Each Hour of the Day for Submeter 3")

# Percentage of use at various times of day for Submeter2 - Sept 6, 2008
plot_ly(houseDay_0907, labels = ~houseDay_0907$hour, values = ~houseDay_0907$Sub_metering_2, type = 'pie',
        textinfo = 'label+percent', textposition = 'inside', insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text+percent', text = ~paste(houseDay_0907$hour,':00'), showlegend = FALSE) %>%
  layout(title = "Power Consumption for Each Hour of the Day for Submeter 2")

# Percentage of use at various times of day for Submeter1 - Sept 6, 2008
plot_ly(houseDay_0907, labels = ~houseDay_0907$hour, values = ~houseDay_0907$Sub_metering_1, type = 'pie',
        textinfo = 'label+percent', textposition = 'inside', insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text+percent', text = ~paste(houseDay_0907$hour,':00'), showlegend = FALSE) %>%
  layout(title = "Power Consumption for Each Hour of the Day for Submeter 1")

# Percentage of total power use over a day by each submeter - Sept 7, 2008
labels = c('Submeter 1', 'Submeter 2', 'Submeter 3')
values = c(sum(houseDay_0907$Sub_metering_1), sum(houseDay_0907$Sub_metering_2), sum(houseDay_0907$Sub_metering_3))
plot_ly(houseDay_0907, labels = labels, values = values, type = 'pie', textinfo = 'label+percent', 
        textposition = 'inside', insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text+percent', text = ~paste(labels), showlegend = FALSE)%>%
  layout(title = 'Power Consumption for Each Submeter in One Day')

# Percentage of total power use over a year by each submeter
houseYear <- filter(data, year == 2008)
valuesyear = c(sum(houseYear$Sub_metering_1), sum(houseYear$Sub_metering_2), sum(houseYear$Sub_metering_3))
plot_ly(houseYear, labels = labels, values = valuesyear, type = 'pie', textinfo = 'label+percent', 
        textposition = 'inside', insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text', text = ~paste(labels), showlegend = FALSE)%>%
  layout(title = 'Power Consumption for Each Submeter in 2008')

# Percentage of total power used in the month of September by each submeter
houseMonth_sept <- filter(data, year == 2008, month == 9)
valuesmonth = c(sum(houseMonth_sept$Sub_metering_1), sum(houseMonth_sept$Sub_metering_2), sum(houseMonth_sept$Sub_metering_3))
plot_ly(houseMonth_sept, labels = labels, values = valuesmonth, type = 'pie', textinfo = 'label+percent', 
        textposition = 'inside', insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text+percent', text = ~paste(labels), showlegend = FALSE)%>%
  layout(title = 'Power Consumption for Each Submeter in Sept 2008')

# Another way of subsetting the data
# Make separate dataframes that mark time every 15, 30, 45, 60 min
data15 <- data[seq(1, nrow(data), 15),]
data30 <- data[seq(1, nrow(data), 30),]
data45 <- data[seq(1, nrow(data), 45),]
data60 <- data[seq(1, nrow(data), 60),]

######################################################################
# Exploratory Time Series Plotting
######################################################################

# Now lets subset the data and create a time series object
# Subset to one observation per week on mondays at 8:00pm for 2007, 2008, and 2009
house070809weekly <- filter(data, weekday == 2 & hour == 20 & minute ==1)

# Create time series object with Sumbeter 3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency = 52, start = c(2007, 1))

# Now let's plot the time series data
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly)

# Add labels and color
autoplot(tsSM3_070809weekly, ts.colour = 'blue', xlab = "Time", ylab = "Watt-hours",
         main = "Sub-meter 3")

# Now try plotting with plot.ts
plot.ts(tsSM3_070809weekly)


# Let's experiment with TS plots for the other two submeters over a 1 month period
# Subset data to 1 month (sept 2008)
# 30 days * (24 hours * 2) = 1440 observations (frequency)
# Submetering 1
house0908_30min <- filter(data30, year == 2008 & month == 9)
tsSM1_0908_30min <- ts(house0908_30min$Sub_metering_1, frequency = 48)
plot.ts(tsSM1_0908_30min)

#Submetering 2
tsSM2_0908_30min <- ts(house0908_30min$Sub_metering_2, frequency = 48)
plot.ts(tsSM2_0908_30min)

#Submetering 3
tsSM3_0908_30min <- ts(house0908_30min$Sub_metering_3, frequency = 48)
plot.ts(tsSM3_0908_30min)


#Try with every hour instead of 30 min
house0908_60min <- filter(data60, year == 2008 & month == 9)
tsSM3_0908_60min <- ts(house0908_60min$Sub_metering_3, frequency = 24)
plot.ts(tsSM3_0908_60min)


# Now lets experiment with the other submeters over a 1 week period
# Subset data to 1 week in September (week 36)
# freq = 7days * 24hr * 2 (336)
# Submetering 3
house0908week_30min <- filter(data30, year == 2008 & month == 9 & week == 36)
tsSM3_0908week_30min <- ts(house0908week_30min$Sub_metering_3, frequency = 48)
plot(tsSM3_0908week_30min)

# Submetering 1
tsSM1_0908week_30min <- ts(house0908week_30min$Sub_metering_1, frequency = 48)
plot(tsSM1_0908week_30min)

# Submetering 2
tsSM2_0908week_30min <- ts(house0908week_30min$Sub_metering_2, frequency = 48)
plot(tsSM2_0908week_30min)

######################################################################
# Forecasting
######################################################################

# Apply time series linear regression to the ts object for one observation
# per week on mondays at 8:00pm for 2007, 2008, and 2009.
# Use summary to obtain R2 and RMSE from the model
library(forecast)
fitTs <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitTs)

# Create and plot the forecast. Forecast ahead 20 time periods
forecastfitTs <- forecast(fitTs, h = 20)
plot(forecastfitTs)

# Next we will change the confidence values of the plot, and only 
# forecast values above zero
# Create forecast with confidence levels 80 and 90
forecastfitTsC <- forecast(fitTs, h = 20, level = c(80, 90))

# Then plot the new forecast with limit y
plot(forecastfitTsC, ylim = c(0, 20), ylab = 'Watt-hours', xlab = 'Time')


# Now let's plot Sept 2008 forecast for all 3 submeters (30 min intervals)
# Submeter 1, Sept 2008
fitSM1_0908 <- tslm(tsSM1_0908_30min ~ trend + season)
summary(fitSM1_0908)
forecastfitSM1_0908 <- forecast(fitSM1_0908, h = 50, level = c(80, 90))
plot(forecastfitSM1_0908, ylim = c(0, 40))

# Submeter 2, Sept 2008
fitSM2_0908 <- tslm(tsSM2_0908_30min ~ trend + season)
summary(fitSM2_0908)
forecastfitSM2_0908 <- forecast(fitSM2_0908, h = 50, level = c(80, 90))
plot(forecastfitSM2_0908, ylim = c(0, 40))

# Submeter 3, Sept 2008
fitSM3_0908 <- tslm(tsSM3_0908_30min ~ trend + season)
summary(fitSM3_0908)
forecastfitSM3_0908 <- forecast(fitSM3_0908, h = 50, level = c(80, 90))
plot(forecastfitSM3_0908, ylim = c(0, 30))


# Now let's plot week 36 (September) for each submeter
# Submeter 1, week 36
fitSM1_week <- tslm(tsSM1_0908week_30min ~ trend + season)
summary(fitSM1_week) 
forecastfitSM1_week <- forecast(fitSM1_week, h = 50, level = c(80, 90))
plot(forecastfitSM1_week, ylim = c(0, 40))

# Submeter 2, week 36
fitSM2_week <- tslm(tsSM2_0908week_30min ~ trend + season)
summary(fitSM2_week)
forecastfitSM2_week <- forecast(fitSM2_week, h = 50, level = c(80, 90))
plot(forecastfitSM2_week, ylim = c(0, 70))

# Submeter 3, week 36
fitSM3_week <- tslm(tsSM3_0908week_30min ~ trend + season)
summary(fitSM3_week)
forecastfitSM3_week <- forecast(fitSM3_week, h = 50, level = c(80, 90))
plot(forecastfitSM3_week, ylim = c(0, 30))


##############################################################
# Decomposing Time Series
##############################################################

# Decompose ts object for one observation per week on mondays at 
# 8:00pm for 2007, 2008, and 2009 (submeter 3)
components070809SM3weekly <- decompose(tsSM3_070809weekly)
plot(components070809SM3weekly)
summary(components070809SM3weekly)


# Decompose ts object for each submeter for Sept 2008
# Submeter 1
compsSM1_0908 <- decompose(tsSM1_0908_30min)
plot(compsSM1_0908)
summary(compsSM1_0908)

# Submeter 2
compsSM2_0908 <- decompose(tsSM2_0908_30min)
plot(compsSM2_0908)
summary(compsSM2_0908)

# Submeter 3
compsSM3_0908 <- decompose(tsSM3_0908_30min)
plot(compsSM3_0908)
summary(compsSM3_0908)


# Decompose ts object for each submeter for week 36
# Submeter 1
compsSM1_week <- decompose(tsSM1_0908week_30min)
plot(compsSM1_week)
summary(compsSM1_week)

# Submeter 2
compsSM2_week <- decompose(tsSM2_0908week_30min)
plot(compsSM2_week)

# Submeter 3
compsSM3_week <- decompose(tsSM3_0908week_30min)
plot(compsSM3_week)
summary(compsSM3_week)

###################################################################
# Holt-Winters Forecasting
##################################################################

# Seasonal adjustment of submeter 3 by subtracting the seasonal component and plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

# Now test the seasonal adjustment by running decompose again --
# Note that seasonal now has a very small scale (-2e^-15 to 1e^-15)
plot(decompose(tsSM3_070809Adjusted))

# Now let's use Holt-Winters exponential smoothing
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))


# Now that we have a ts object that contains exponentially smoothed data with
# no seasonality, let's use forecast
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h = 48)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab = "Watt-hours", xlab = "Time - Submeter 3")

# Now let's adjust the confidence level and only plot the forecasted area
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h = 25, level = c(10, 25))
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab = "Watt-hours", xlab = "Time - Submeter 3",
     start(2010))
# This is not a very useful forecast .....


# Let's try HoltWinters for the month of Sept 2008 for each submeter
# Submeter 1
tsSM1_0908Adjusted <- tsSM1_0908_30min - compsSM1_0908$seasonal
autoplot(tsSM1_0908Adjusted)
plot(decompose(tsSM1_0908Adjusted))

tsSM1_HW0908 <- HoltWinters(tsSM1_0908Adjusted, beta = FALSE)
plot(tsSM1_HW0908)
tsSM1_HW0908

tsSM1_HW0908for <- forecast(tsSM1_HW0908, h = 80)
plot(tsSM1_HW0908for, ylim = c(0, 40))

# plot only forecasted area for Sub 1 w smaller conf levels
tsSM1_HW0908forC <- forecast(tsSM1_HW0908, h = 80, level=c(10,25))
plot(tsSM1_HW0908forC, ylim = c(0, 10), ylab= "Watt-Hours", xlab= "Time - Sub-meter 1", start(30))


# Submeter 2
tsSM2_0908Adjusted <- tsSM2_0908_30min - compsSM2_0908$seasonal
autoplot(tsSM2_0908Adjusted)
plot(decompose(tsSM2_0908Adjusted))

tsSM2_HW0908 <- HoltWinters(tsSM2_0908Adjusted, beta = FALSE)
plot(tsSM2_HW0908)

tsSM2_HW0908for <- forecast(tsSM2_HW0908, h = 80)
plot(tsSM2_HW0908for)

# plot only forecasted area for Sub 2 w smaller conf levels
tsSM2_HW0908forC <- forecast(tsSM2_HW0908, h = 80, level=c(10,25))
plot(tsSM2_HW0908forC, ylim = c(0, 10), ylab= "Watt-Hours", xlab= "Time - Sub-meter 3", start(30))

# Submeter 3
tsSM3_0908Adjusted <- tsSM3_0908_30min - compsSM3_0908$seasonal
autoplot(tsSM3_0908Adjusted)
plot(decompose(tsSM3_0908Adjusted))

tsSM3_HW0908 <- HoltWinters(tsSM3_0908Adjusted, beta = FALSE)
plot(tsSM3_HW0908)

tsSM3_HW0908for <- forecast(tsSM3_HW0908, h = 80)
plot(tsSM3_HW0908for) 

# plot only forecasted area for Sub 3 w smaller conf levels
tsSM3_HW0908forC <- forecast(tsSM3_HW0908, h = 80, level = c(10,25))
plot(tsSM3_HW0908forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab= "Time - Sub-meter 3", start(30))


# Now let's try for 1 week period for each submeter
# Submeter 1
tsSM1_weekAdjusted <- tsSM1_0908week_30min - compsSM1_week$seasonal
autoplot(tsSM1_weekAdjusted)
plot(decompose(tsSM1_weekAdjusted))

tsSM1_HWweek <- HoltWinters(tsSM1_weekAdjusted, beta = FALSE)
plot(tsSM1_HWweek)

tsSM1_HWweekfor <- forecast(tsSM1_HWweek, h = 48)
plot(tsSM1_HWweekfor)

# plot only forecasted area for Sub 1 w smaller conf levels
tsSM1_HWweekforC <- forecast(tsSM1_HWweek, h = 48, level=c(10,25))
plot(tsSM1_HWweekforC, ylim = c(0, 15), ylab= "Watt-Hours", xlab= "Time - Sub-meter 1", start(8))

# Submeter 2
tsSM2_weekAdjusted <- tsSM2_0908week_30min - compsSM2_week$seasonal
autoplot(tsSM2_weekAdjusted)
plot(decompose(tsSM2_weekAdjusted))

tsSM2_HWweek <- HoltWinters(tsSM2_weekAdjusted, beta = FALSE)
plot(tsSM2_HWweek)

tsSM2_HWweekfor <- forecast(tsSM2_HWweek, h = 48)
plot(tsSM2_HWweekfor)

# plot only forecasted area for Sub 2 w smaller conf levels
tsSM2_HWweekforC <- forecast(tsSM2_HWweek, h = 48, level=c(10,25))
plot(tsSM2_HWweekforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab= "Time - Sub-meter 2", start(8))

# Submeter 3
tsSM3_weekAdjusted <- tsSM3_0908week_30min - compsSM3_week$seasonal
autoplot(tsSM3_weekAdjusted)
plot(decompose(tsSM3_weekAdjusted))

tsSM3_HWweek <- HoltWinters(tsSM3_weekAdjusted, beta = FALSE)
plot(tsSM3_HWweek)

tsSM3_HWweekfor <- forecast(tsSM3_HWweek, h = 48)
plot(tsSM3_HWweekfor)

# plot only forecasted area for Sub 3 w smaller conf levels
tsSM3_HWweekforC <- forecast(tsSM3_HWweek, h = 48, level=c(10,25))
plot(tsSM3_HWweekforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab= "Time - Sub-meter 3", start(8))

