library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(rvest)
library(prophet)

#vignette('selectorgadget') used to identify table from "tsa.gov"

# loading in tsa passenger throughput data from "tsa.gov"
tsa_table <- html_text(html_nodes(read_html("https://www.tsa.gov/coronavirus/passenger-throughput"),'td'))
tsa_table <- as.vector(tsa_table)

#cleaning dataset from "tsa.gov" into 'tidy data set'
date <- tsa_table[seq(1, length(tsa_table), 3)]
date <- date[-1]
date <- as.Date(date, format = "%m/%d/%Y", tryFormats = c("%m/%d/%Y"))

passengers <- tsa_table[seq(2, length(tsa_table), 3)]
passengers <- passengers[-1]
passengers <- gsub(",","",passengers)
passengers <- as.numeric(passengers)

passengers_lastyr <- tsa_table[seq(3, length(tsa_table), 3)]
passengers_lastyr <- passengers_lastyr[-1]
passengers_lastyr <- gsub(",","",passengers_lastyr)
passengers_lastyr <- as.numeric(passengers_lastyr)

#creating data.frame for analysis with YoY as a % included
passenger_data <- data.frame(date,passengers,passengers_lastyr) %>%
  mutate(change_yoy = 100 * (passengers - passengers_lastyr)/passengers_lastyr)

#graphing flight data
ggplot(passenger_data, aes(x = date,y = passengers)) +
  geom_line()+
  theme_classic(
    base_size = 10
  ) +  labs(x="Date",y="passenger volume")

ggplot(passenger_data, aes(x = date,y = passengers_lastyr)) +
  geom_line()+
  theme_classic(
    base_size = 10
  ) +  labs(x="Date",y="passenger volume")

ggplot(passenger_data, aes(x = date,y = change_yoy)) +
  geom_line()+
  theme_classic(
    base_size = 10
  ) +  labs(x="Date",y="passenger volume YoY")


#import data
df <- passenger_data %>%
  arrange(date) %>%
  select(date,passengers)
colnames(df) <- c("ds","y")

df_forecast <- df %>%
  mutate(ds = date(ds)) %>%
  group_by(ds) %>%
  summarize(y = sum(y))

#setting up the forecast model
m <- prophet(df_forecast)
future <- make_future_dataframe(m, periods = 30)

#forecasting
forecast <- predict(m, future)
plot(m, forecast) +
  theme_classic(
    base_size = 10
  ) +  labs(x="Date",y="passenger volume")


#forecasting two new graphs that are pretty
prophet_plot_components(m, forecast)