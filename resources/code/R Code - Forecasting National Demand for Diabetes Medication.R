# Forecasting National Demand for Diabetes Medication

# Install the packages
install.packages("forecast")
install.packages("dplyr")

library(forecast)
library(dplyr)

# Read in the data and store as Spending
spending <- read.csv("Total Spending on Diabetes Drugs - National.csv", header = TRUE, stringsAsFactors = FALSE)

# Explore the columns and the top few rows
head(spending)

# Store the items, starting at 2010
ts_spending = ts(spending$items, start=2010, frequency=12)

# Plot the spending over as a time series chart
plot(ts_spending)

# Create a time series model using ets
m_ets = ets(ts_spending)
f_ets = forecast(m_ets, h=24)
plot(f_ets)

# Create a time series model using Arima, forecasting 36 months into the future
m_aa = auto.arima(ts_spending)
f_aa = forecast(m_aa, h=36)
plot(f_aa)

# Create a time series model using tbats, one of the more popular models
m_tbats = tbats(ts_spending)
f_tbats = forecast(m_tbats, h=24)
plot(f_tbats)

# Build a chart comparing the performance of each model
barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC), ylab="AIC")

par(bg="black", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE, xaxs="r", yaxs="i", mgp=c(2.8,0.3,0.5), col.lab="white", col.axis="white", col.main="white", font.main=1, cex.main=0.8, cex.axis=0.8, cex.lab=0.8, family="Helvetica", lend=1, tck=0)

# Plot the forecast of the Arima model, add titles and some basic design
plot(f_aa, main="FORECASTING DEMAND FOR DIABETES MEDICATION", col="white", cex.main=1, xlab="YEAR", ylab="ITEM COUNT")

# Add a grid
grid(NULL, NULL, lty="dotted", col="white")


# Create new field including predictions for each month, including lower and upper bounds
last_date = index(ts_spending)[length(ts_spending)]

forecast_df = data.frame(spending_predicted=f_aa$mean,
                         spending_lower=f_aa$lower[,2],
                         spending_upper=f_aa$upper[,2],
                         date=last_date + seq(1/12, 2, by=1/12))

forecast_df = forecast_df %>%
    mutate(year=floor(date)) %>%
    mutate(month=((date %% 1) * 12) + 1)

# Write a new file containing the forecasts
write.csv(forecast_df, file = "forecast_spending.csv")
