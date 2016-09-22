# Install necessary packages
install.packages("gtrendsR")
library(gtrendsR)
ls("package:gtrendsR")

usr <- "your_email"
psw <- "your_password"
gconnect(usr, psw)

# Specify what trends you're interested in
diabetes_trend <- gtrends(c("Type 1 Diabetes", "Type 2 Diabetes"))

# Plot the trends
plot(diabetes_trend)

# Plot by Region
plot(diabetes_trend, type = "regions")
