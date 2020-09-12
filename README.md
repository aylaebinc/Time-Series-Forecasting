# Time-Series-Forecasting in RStudio
Forecasting residential energy consumption using linear and Holt-Winters algorithms

* Please see 'rmarkdown.md' document above for full analysis
* Also available on RPubs at https://rpubs.com/brosnahj/time-forecasting

## Objective
We have been asked to forecast power consumption by submeter for a residential home. The objective is to visualize and forecast energy use by performing time series linear regression and Holt Winters modeling on each submeter. Forecasted results will help client determine energy use patterns and energy saving recommendations will be offered based on results.

## Skills

* Time Series preprocessing and manipulation
* Tidyverse data wrangling
* Time series linear regression 25-Day and 10-Week energy use forecasting
* Decomposition analysis of time series data (observed, trend, seasonal, random)
* Holt Winters 25-Day and 10-Week energy use forecasting
* Business objectives achieved

## Insights and Forecasts
* Holt Winters forecasted results are used for energy use predictions
  + Submeter 1 (Kitchen): mean of 2.107 Watt/hour energy use forecasted for 25 days
  + Submeter 2 (Laundry): mean of 5.172 Watts/hour energy use forecasted for 25 days
  + Submeter 3 (Water heater & AC): mean of 7.866 Watts/hour for 10 week forecast
  + Predicted average energy use in watts/hour for each submeter is projected to be higher than average energy used from 2007-2010 (Sub1 1.16 Watts/hour, Sub2 1.34 Watts/hour, Sub3 6.21 Watts/hour). Energy saving recommendations going in to 2010 are provided to help reduce energy and save money.

## Energy Saving Suggestions
* Reset HVAC controls to reduce daily Water Heater & AC spikes noted throughout the year to save energy
* Reduce the length of time AC and/or Water Heater is programmed to turn on during the day
* Reset AC thermometer in winter months to higher temperature setting to reduce energy use during peak season
* Regularly replace AC filters, turn off lights whenever you leave a room, and reduce amount of hot water used when possible
