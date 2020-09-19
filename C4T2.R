# Visualizing & Analyzing Sub-meter data

### Loading packages
library(RMySQL)
library(lubridate)
library(tidyverse)
library(openxlsx)
library(knitr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(scales)
library(imputeTS)
library(ggfortify)
library(forecast)
library(dplyr)

# Importing data - actually need to import manually!!!!
subMeters <- read.csv(file.path('C:/Users/jlbro/OneDrive/C4T1', 'subMeters.csv'))

str(subMeters)
summary(subMeters)

subMeters$day <- wday(subMeters$DateTime, label=FALSE) 

# Submeter usage
YearlyAvg <- subMeters %>% 
  group_by(year) %>% 
  summarise(across(starts_with('sub'), mean))

###### YearlySum Regular and Gathered
YearlySum <- subMeters %>%
  group_by(DateTime, Date, year) %>% 
  summarise(across(starts_with('sub'), sum))

YearlySumGather <- gather(YearlySum, 'sub1', 'sub2', 'sub3',
                          key = 'submeter', value = 'amount')

###### QtrlySum Regular and Gathered
QtrlySum <- subMeters %>%
  group_by(year, quarter) %>% 
  summarise(across(starts_with('sub'), sum))

QtrlySumGather <- gather(QtrlySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')

###### MonthlySum Regular and Gathered
MonthlySum <- subMeters %>%
  group_by(year, month) %>% 
  summarise(across(starts_with('sub'), sum))

MonthSumGather <- gather(MonthlySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')

###### WdaySum Regular and Gathered
WdaySum <- subMeters %>%
  group_by(year, wday) %>% 
  summarise(across(starts_with('sub'), sum))

WdaySumGather <- gather(WdaySum, 'sub1', 'sub2', 'sub3',
                        key = 'submeter', value = 'amount')

###### DailySum Regular and Gathered
DailySum <- subMeters %>%
  group_by(Date, year, quarter) %>% 
  summarise(across(starts_with('sub'), sum))

DailySumGather <- gather(DailySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')


###### HourlySum Regular and Gathered
HourlySum <- subMeters %>% 
  group_by(DateTime, year, week, hour) %>% 
  summarise(across(starts_with('sub'), sum))

HourlySumGather <- gather(HourlySum, 'sub1', 'sub2', 'sub3',
                          key = 'submeter', value = 'amount')

###### AllSum Gathered
AllSumGathered <- gather(subMeters, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')



### EDA

# Plot all of sub-meter 1
plot(subMeters$sub1)

# Filtering out a week
houseWeek <- data.frame(filter(subMeters, year==2008 & week==2))

# Plotting 1 week for sub1 - not useful
plot(houseWeek$sub1, .2)

# Plotting sub1 fraction - Worked but no pattern
sampWeek <- sample_frac(houseWeek, .2)
plot(sampWeek$sub1)

## Plotly

# Subset 9th day of January 2008 - All observations
houseDay <- data.frame(filter(subMeters, year==2008 & month=='Jan' & day==9))

# Plot sub1 for Jan 9 2008
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$sub1, type = 'scatter', mode = 'lines')

# Plot sub1, 2, 3 - All observations - Grainy
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$sub1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseDay$sub2, name = 'Laundry Room', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseDay$sub3, name = 'Water Heater & AC', type = 'scatter', mode = 'lines') %>% 
  layout(title = 'Power Consumption January 9th, 2008',
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Power(Watt-hours'))

# Subset Jan 9 2008 - 10 minute frequency
houseDay10 <- data.frame(filter(subMeters, year==2008 & month=='Jan' & day==9 & (minute==0 | minute==10 | minute==20 | minute==30 | minute==40 | minute==50)))

houseDay11 <- data.frame(filter(AllSumGathered, year==2008 & month=='Jan' & day==9 & (minute==0 | minute==10 | minute==20 | minute==30 | minute==40 | minute==50)))
houseDay12 <- data.frame(filter(AllSumGathered, year==2008 & month=='Aug' & day==9 & (minute==0 | minute==10 | minute==20 | minute==30 | minute==40 | minute==50)))
houseDay13 <- data.frame(filter(AllSumGathered, year==2008 & week==34 & day == 2 & (minute==0 | minute==10 | minute==20 | minute==30 | minute==40 | minute==50)))
houseDay14 <- data.frame(filter(AllSumGathered, year==2008 & month=='Aug' & day==28 & (minute==0 | minute==10 | minute==20 | minute==30 | minute==40 | minute==50)))

houseWeek31 <- data.frame(filter(AllSumGathered, year==2008 & week==31 & minute==0))

###################################################################################################

###### PLOT 1 DAY, 10 minute Frequency - sub1, 2, 3 - All observations
# Water/AC: Early and late peaks may be for water heater. Mid-day peaks for AC
# Kitchen: Cooking Dinner
# Laundry: Regular small intervals may be for the fridge or light?
# It helps the homeowner see when and where energy is being used on one day
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$sub1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseDay10$sub2, name = 'Laundry Room', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseDay10$sub3, name = 'Water Heater & AC', type = 'scatter', mode = 'lines') %>% 
  layout(title = 'Power Consumption January 9th, 2008',
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Power(Watt-hours'))


###### PLOT 1 DAY, 10 minute Frequency - GGPLOT USE THESE FOR REPORT
houseDay11 %>%  
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .45,
      legend.position = 'top',
      legend.justification = 'left',
      legend.margin=margin(2,0,0,0),
      legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter: ', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Consumption January 9th, 2008')

houseDay12 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
      legend.position = 'top',
      legend.justification = 'left',
      legend.margin=margin(2,0,0,0),
      legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter:  ', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Consumption August 9th, 2008')

houseDay13 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter:  ', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Consumption August 18th, 2008')

houseDay14 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter:  ', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Consumption August 28th, 2008')


months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

## Monthly Sum by Year
subset(MonthSumGather, year != 2010) %>%
  ggplot(aes(month, amount, color=submeter)) +
  geom_line(size = 1) +
  facet_grid(year~.) +
  theme_bw() +
  theme(aspect.ratio = .3,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter:  ', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  scale_x_discrete(limits=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Monthly Power Consumption by Year')

## Daily Sum by Month in Summer 2008
subset(DailySumGather, year==2008 & quarter == 3) %>%
  ggplot(aes(Date, amount, color=submeter)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter:  ', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  scale_x_date(labels = date_format('%b %d'), breaks = date_breaks('1 week')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Daily Power Use July - September 2008')

## Daily Sum by Month in Summer across years
subset(DailySumGather, quarter==3) %>%
  ggplot(aes(Date, amount, color=submeter)) +
  geom_line(size = 1) +
  facet_wrap(~year, scales = 'free', nrow = 3, strip.position = 'right') +
  theme_bw() +
  theme(aspect.ratio = .25,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter:', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  scale_x_date(labels = date_format('%b %d'), breaks = date_breaks('2 weeks')) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 8)) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Daily Power Use July - September by Year')

## Hourly Sum by Week in July-August 2008
houseWeek31 %>%
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter:', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Use July 28 - Aug 4 2008')


# Changing wday and month from numbers to labels
subMeters$wday <- wday(subMeters$DateTime, label=TRUE)
subMeters$month <- month(subMeters$DateTime, label=FALSE)
subMeters$day <- wday(subMeters$DateTime, label=TRUE)



### Time Series Forecasts ### Needed for Report! for all 3 Submeters
str(subMeters)

##################################################### Submeter 3: Water Heater & AC
# Subset to one observation per week on Mondays at 8:00pm for 2007, 2008, 2009
house070809weekly <- filter(subMeters, wday=='Mon' & hour==20 & minute==0)

# Create TIME SERIES object for Submeter 3
tsSM3_070809weekly <- ts(house070809weekly$sub3, frequency = 52, start = c(2007,1))

#####STEP 2 TIME SERIES VISUALIZATION Submeter 3
autoplot(tsSM3_070809weekly, ts.colour = 'black', xlab = 'Time', ylab = 'Watt Hours', main = 'Submeter 3:  Water Heater & AC, Weekly Interval', size = .7) +
           theme_bw()


# Create TIME SERIES LINEAR REGRESSION
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3) #R2 and RMSE

# Create forecast, then PLOT
forecastfitSM3 <- forecast(fitSM3, h=10)
plot(forecastfitSM3) #80 and 95% confidence interval, has negative values

# FORECAST TIME SERIES LINEAR REGRESSION with confidence levels 80 and 90%, then PLOT
forecastfitSM3c <- forecast(fitSM3, h=10, level=c(25,50))
forecastfitSM3
######STEP 3 LINEAR REGRESSION FORECAST VISUALIZATION  #Also need chart built for R2 and RMSE
plot(forecastfitSM3c, ylim = c(0, 20), xlim = c(2009.5,2010.2), ylab = 'Watt-Hours', xlab = 'Time', main = 'Submeter 3: Water Heater & AC 10-Week Seasonal Forecast')

# Decompose SEASONAL TIME SERIES into trend, seasonal, remainder, then PLOT
# Remove seasonal component so you can analyze the trend independently
components070809SM3weekly <- decompose(tsSM3_070809weekly)
######STEP FOUR DECOMPOSITION VISUALIZATION WITH ANALYSIS
plot(components070809SM3weekly)
summary(components070809SM3weekly)  #Doesn't say alot

### Trend reveals power use steady decline from high of 6 mid-2007 to low of 2 late-2008, then picked back up
### Seasonal effects shows decreased use Q3 and peak use Q1 and Q2

components070809SM3weekly$seasonal 
components070809SM3weekly$trend
components070809SM3weekly$random

# HOLT WINTERS FORECASTING - Exponential Smoothing
# First subtract the seasonal component removed in prior step
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

# Test seasonal adjustment by running Decompose again. 
# Note the extremely small scale in seasonal line!
plot(decompose(tsSM3_070809Adjusted))

# Holt Winters Exponential Smoothing, then PLOT
tsSM3_HW <- HoltWinters(tsSM3_070809weekly)
tsSM3_HW
plot(tsSM3_HW, ylim = c(0, 25))

# HoltWinters forecast, then PLOT
tsSM3_HWforecast <- forecast(tsSM3_HW, h=10)
plot(tsSM3_HWforecast, ylim = c(0, 30), xlim = c(2009,2010.2), ylab = 'Watts', xlab = 'Time', main = 'Water Heater/AC: Holt-Winters 10-Week Forecast')

# Forecast Holt Winters with diminished confidence levels
tsSM3_HWforecast_C <- forecast(tsSM3_HW, h=10, level = c(25, 50))
tsSM3_HWforecast_C
###STEP FIVE HOLT WINTERS FORECASTING & ANALYSIS
plot(tsSM3_HWforecast_C, ylim = c(0, 30), ylab = 'Watts', xlab = 'Time', main = 'Water Heater/AC: Holt-Winters 10-Week Forecast, 25-50% CI', start(2010))

# Plot is different from Linear Regression Forecast bc it is smooth line, accounts for
# no variance or seasonality in the data. Linear Regression may be more useful, since daily variances occur

############################################################### Submeter 2: Laundry
# Subset to one observation per day at 7:00pm for 2007, 2008, 2009
house070809daily <- filter(subMeters, hour==19 & minute==0)
houseDec2009daily <- filter(subMeters, year==2009 & month==12 & minute==0)

# Create TIME SERIES object for Submeter 2
tsSM2_070809daily <- ts(house070809daily$sub2, frequency = 31)

######STEP 2 TIME SERIES VISUALIZATION Submeter 2
autoplot(tsSM2_070809daily, ts.colour = 'black', xlab = 'Time', ylab = 'Watt Hours', main = 'Submeter 2:  Laundry, Daily Interval') +
  theme_bw()

# Fit to time series linear regression, then SUMMARY to obtain R2 and RMSE
fitSM2 <- tslm(tsSM2_070809daily ~ trend + season)
summary(fitSM2)

# Create forecast, then PLOT
forecastfitSM2 <- forecast(fitSM2, h=25)
forecastfitSM2
plot(forecastfitSM2) #80 and 95% confidence interval, has negative values

# FORECAST TIME SERIES LINEAR REGRESSION with confidence levels 80 and 90%, then PLOT
forecastfitSM2c <- forecast(fitSM2, h=25, level=c(80,90))
######STEP 3 LINEAR REGRESSION FORECAST VISUALIZATION  #Also need chart built for R2 and RMSE
plot(forecastfitSM2c, ylim = c(0, 30), xlim = c(2009.5,2010.06), ylab = 'Watts', xlab = 'Time', main = 'Submeter 2, Laundry: 25-day Seasonal Forecast')

# Decompose SEASONAL TIME SERIES into trend, seasonal, remainder, then PLOT
components070809SM2daily <- decompose(tsSM2_070809daily)
######STEP FOUR DECOMPOSITION VISUALIZATION WITH ANALYSIS
plot(components070809SM2daily)
summary(components070809SM2daily)  #Doesn't say alot

### Trend reveals power use decline with high of 2.4 2008Q2 to low of 1.6 in 2009Q2, then rise to 2.1 mid-2009
### Seasonality is hard to view - no consistent seasonality

# HOLT WINTERS FORECASTING - Exponential Smoothing
# First subtract the seasonal component removed in prior step
tsSM2_070809Adjusted <- tsSM2_070809daily - components070809SM2daily$seasonal
autoplot(tsSM2_070809Adjusted)

# Test seasonal adjustment by running Decompose again. 
# Note the extremely small scale in seasonal line!
plot(decompose(tsSM2_070809Adjusted))

# Holt Winters Exponential Smoothing, then PLOT
tsSM2_HW <- HoltWinters(tsSM2_070809daily)
tsSM2_HW
plot(tsSM2_HW, ylim = c(0, 20))

# HoltWinters forecast, then PLOT
tsSM2_HWforecast <- forecast(tsSM2_HW, h=25)
tsSM2_HWforecast
plot(tsSM2_HWforecast, ylim = c(0, 20), xlim = c(2009.5, 2010.07), ylab = 'Watts', xlab = 'Time', main = 'Submeter 2 Laundry: Holt-Winters 25-Day Forecast')

# Forecast Holt Winters with diminished confidence levels
tsSM2_HWforecast_C <- forecast(tsSM2_HW, h=25, level = c(25,50))
tsSM2_HWforecast_C
###STEP FIVE HOLT WINTERS FORECASTING & ANALYSIS
plot(tsSM2_HWforecast_C, ylim = c(0, 15), ylab = 'Watts', xlab = 'Time', main = 'Submeter 2 Laundry: Holt-Winters 25-Day Forecast, 25-50% confidence interval', start(2010))


########################################################### Submeter 1: Kitchen
# Subset to one observation per day at 6:00pm for 2007, 2008, 2009
house070809Daily <- filter(subMeters, hour==18 & minute==0)
houseSub1Monthly <- filter(subMeters, week==1)

# Create TIME SERIES object for Submeter 1
tsSM1_070809Daily <- ts(house070809Daily$sub1, frequency = 365, start = c(2007,1))
tsSM1_070809Daily
######Step 2 TIME SERIES VISUALIZATION for Submeter 1
autoplot(tsSM1_070809Daily, ts.colour = 'black', xlab = 'Time', ylab = 'Watt Hours', main = 'Submeter 1:  Kitchen, Daily Interval', size=.7) +
  theme_bw()

# Fit to time series linear regression, then SUMMARY to obtain R2 and RMSE
fitSM1 <- tslm(tsSM1_070809Daily ~ trend + season)
summary(fitSM1)

# Create forecast, then PLOT
forecastfitSM1 <- forecast(fitSM1, h=25)
plot(forecastfitSM1) #80 and 95% confidence interval, has negative values

# FORECAST TIME SERIES LINEAR REGRESSION with confidence levels 80 and 90%, then PLOT
forecastfitSM1c <- forecast(fitSM1, h=25, level=c(80,90))
summary(forecastfitSM1c)
forecastfitSM1c
######STEP 3 LINEAR REGRESSION FORECAST VISUALIZATION #Also need chart built for R2 and RMSE
plot(forecastfitSM1c, ylim = c(0, 25), xlim = c(2009.5,2010.07), ylab = 'Watts', xlab = 'Time', main = 'Submeter 1 Kitchen: 25-day Linear Regression Forecast')

# Decompose SEASONAL TIME SERIES into trend, seasonal, remainder, then PLOT
components070809SM1daily <- decompose(tsSM1_070809Daily)
######STEP FOUR DECOMPOSITION VISUALIZATION WITH ANALYSIS  
plot(components070809SM1daily)
summary(components070809SM1daily)  #Doesn't say alot

### Trend reveals steady decline from 1.1 mid-2007 to low 0.6 mid-2008 then rise to 1.8 mid-2009
### Seasonality is hard to view - no consistent seasonality

# HOLT WINTERS FORECASTING - Exponential Smoothing
# First subtract the seasonal component removed in prior step
tsSM1_070809Adjusted <- tsSM1_070809Daily - components070809SM1daily$seasonal
autoplot(tsSM1_070809Adjusted)

# Test seasonal adjustment by running Decompose again. 
# Note the extremely small scale in seasonal line!
plot(decompose(tsSM1_070809Adjusted))

# Holt Winters Exponential Smoothing, then PLOT
tsSM1_HW <- HoltWinters(tsSM1_070809Daily)
tsSM1_HW
plot(tsSM1_HW, ylim = c(0, 5))

# HoltWinters forecast, then PLOT
tsSM1_HWforecast <- forecast(tsSM1_HW, h=25)
plot(tsSM1_HWforecast, ylim = c(0, 40), xlim = c(2009.5,2010.06), ylab = 'Watts', xlab = 'Time', main = 'Submeter 1 Kitchen: Holt-Winters 25 Day Forecast')

# Forecast Holt Winters with diminished confidence levels
tsSM1_HWforecast_C <- forecast(tsSM1_HW, h=25, level = c(10,25))
tsSM1_HWforecast_C
###STEP FIVE HOLT WINTERS FORECASTING & ANALYSIS
plot(tsSM1_HWforecast_C, ylim = c(0, 35), ylab = 'Watts', xlab = 'Time', main = 'Submeter 1 Kitchen: Holt-Winters 25 Day Forecast, 25-50% confidence interval', start(2010))



####################################################################################
# Must show comparison chart of each R2 and RMSE that I forecasted above
####################################################################################























