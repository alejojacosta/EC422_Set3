library(tidyverse)
library(fpp3)

#Read CSV and Inspect Date Column
retail_raw <- read.csv("data/retail_sales_raw.csv")
head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

#Reshape Data and Tsibble
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
head(retail_raw_long)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)
head(retail_tsibble)
has_gaps(retail_tsibble)

#Canadian Gas Production
key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)
has_gaps(can_gas)  
head(can_gas)  

#Dates in Tsibble
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))
head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(
    year = year(Month),
    month = month(Month, label = TRUE),
    quarter = quarter(Month)
  )

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(
    next_month = Month + 1,
    same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = mean(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)
#The series still appears to be seasonal, with greater volumes in Q1 and lower 
#volumes in Q3. Taking the mean across quarters would remove month to month effects
#within each quarter, and since each quarter does not have the same number of days 
#taking the mean would overgeneralize the data.

#Plotting with Autoplot
retail_ts|>
  autoplot(sales,alpha = 0.5) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
#The NSA series has a large spike every December and a large drop every 
#June/July. The seasonally adjusted series does not have the extreme cyclical
#patterns. Both series have the same trend but the effect of recessions is 
#less visible with the non-adjusted series than it is in the adjusted series.

can_gas |> autoplot(Volume) +
  geom_smooth() +
  labs(
    title = "Canadian Natural Gas Production",
    x = "Date", y = "Volume (millions of cubic metres)"
  ) +
  theme_minimal() 
#The geom_smooth line through the data helps visualize the overall trend in the
#data.

#Log Transformations

#The seasonal swings in can_gas grow with the level of the series until the end
#of the 1980's where the swings reduce in size and visually clump together. To 
#combat the inconsistency in seasonal swings a log transformation is needed. 

can_gas <- can_gas |>
  mutate(log_volume = log(Volume))
can_gas |>
  autoplot(log_volume) +
  geom_smooth() +
  labs(
       title = "Canadian Natural Gas Production (Log Scale)", 
   x = "Date", y = "Log Volume (millions of cubic metres)"
  )+
  theme_minimal()
#The seasonal trends are much more consistent in size in the log transformed series.
#The trend line becomes less linear (more concave) than the non-log transformed 
#series. I think logs are an appropriate transformation for the series, since
#they help stabilize the seasonal swing, and though it doesn't remove the trend,
#it makes the scale of the seasonal swings more proportional. 

rsafsna <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

rsafsna |>
  autoplot (log_sales) +
  geom_smooth() +
  labs(
    title = "U.S. Retail and Food Services Sales (Log Scale)",
    subtitle = "Not Seasonally Adjusted",
    x = NULL, y = "Log Millions of dollars"
  ) +
  theme_minimal()

rsafsna |> 
  autoplot (sales) +
  geom_smooth() +
  labs(
    title= "U.S. Retail and Food Services Sales",
    subtitle = "Not Seasonally Adjusted",
    x= NULL, y= "Millions of dollars"
  ) +
  theme_minimal()
#The log version of sales slightly evens out the seasonal cycles, however the
#difference between both graphs is not very drastic since both series are already
#fairly stable in seasonal swings. The level also doesn't change much, with the
#original series having a relatively linear shape.

### Growth Rates

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )
can_gas |>
  autoplot(growth_direct, na.rm = TRUE) +
  labs(
    title = "Canadian Natural Gas Production Growth Rate",
    x = "Date", y = "Growth Rate (%)"
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
  )
can_gas |>
  autoplot(growth_gap, na.rm = TRUE) +
  labs(
    title = "Canadian Natural Gas Production Growth Rate Gap",
    x= "Date", y= "Growth Rate Gap (%)") +
  theme_minimal()
#The log difference approximation is quite close to the direct growth rate, with 
#most differences between the growth rates being less than 1%.

#SA vs NSA Growth Rates
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()
retail_ts |>
  autoplot(growth_mom, na.rm = TRUE, alpha = 0.5) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()
#The non adjusted series is much noisier than the adjusted series. This is 
#because the non adjusted series has greater seasonal swings. Without accounting
#for seasonality the growth rates of the un-adjusted series reflect the 
#seasonal trend. 


### Modeling Seasonality

#Seasonal Plots
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()
#The seasonal plot of the non-adjusted series has lines that follow a similar
#pattern over time (ie the lines look similar to each other, especially those
#close in time). The lines of the seasonally adjusted series are flatter, and 
#don't exhibit the same patterns as the non-adjusted series.

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Natural Gas Production",
    y = "Volume (millions of cubic metres)"
  ) +
  theme_minimal()
#The summer months have consistently lower natural gas production, whilst the 
#winter months have consistently higher production. This makes sense when 
#considering that natural gas is used for heating and electricity, both of which
#spike in winter when the weather is colder and the days are shorter.

#Subseries Plots
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)")


#X-13 ARIMA SEATS
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()
#The seasonal factor is the highest in December. The sales in December are 
#roughly 20% greater than the trend (or 4 billion more in dollar sales). The 
#seasonal swings stay fairly constant over time.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()
#The approximation matches fairly well (less than 1% difference) with the 
#Census Bureau's, especially before 2020 (COVID). This is consistent with the 
#fact that the Census Bureau's data is already adjusted for outliers and shocks.

#Linear Model + Seasonal Dummies
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))
report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)
#The average monthly growth rate is 0.4%. December has the highest seasonal 
#effect (t stat of 27.8).

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()
#The fitted values follow the series closley after 1980, however before that
#the fitted values don't caputre the level or the trend of the seires very
#well.

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
#The residuals do not look like white noise, meaning there is still structure
#in the data that was not captured by the model. The Ljung-Box test reaffirms
#this, with a p-value near zero the null hypothesis that the series is white 
#noise is rejected.

### Forecast Evaluation 
gas_cv_fc_Expanding <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)
gas_cv_fc_Expanding |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
#The SNAIVE model has a lower RMSE and MAE than the TSLM model, 
#meaning it is a better fit for the data. The first forecast date is January
#1980, and the last is March of 2005.

errs <- gas_cv_fc_Expanding |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)
#The DM test finds statistically significant evidence to reject the null 
#hypothesis that both models have the same predictive accuracy. This means
#that one model performs better than the other.

gas_cv_fc_rolling <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)
gas_cv_fc_rolling |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
#The seasonal trend with dummies model changes more when using a rolling
#window. A rolling window improves the model since older data becomes less
#relevant and can cause the model to become outdated. As seen in the 4.4 figure,
#the fitted trend better predicts the acutal outcome in the past few decades,
#thus excluding older data would improve the models accuracy. 


