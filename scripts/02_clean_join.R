# =========================================================
# 02_clean_join.R
# Clean + aggregate to daily + join UK-AIR (DEFRA) and Open-Meteo
# =========================================================

library(tidyverse)
library(lubridate)
library(readr)
library(stringr)

# ---- paths ----
AQ_RAW_PATH <- "data/ukair_raw_2023.rds"
WX_RAW_PATH <- "data/openmeteo_raw_2023.rds"

AQ_CLEAN_PATH <- "data/air_quality_daily_2023.rds"
WX_CLEAN_PATH <- "data/weather_daily_2023.rds"
MERGED_PATH   <- "data/aq_weather_daily_2023.rds"

# ---- read raw data ----
aq_raw <- readRDS(AQ_RAW_PATH)
wx_raw <- readRDS(WX_RAW_PATH)

# =========================================================
# 1) Clean UK-AIR air quality data (daily NO2 mean)
# Key fix: UK-AIR Date is "dd-mm-YYYY", so use dmy()
# =========================================================


no2_col <- names(aq_raw)[str_detect(names(aq_raw), regex("^Nitrogen dioxide$", ignore_case = TRUE))]

if (length(no2_col) == 0) {
  stop("Could not find a NO2 column. Please check names(aq_raw) to see how NO2 is labelled.")
}
no2_col <- no2_col[1]  # take only the first match

aq_clean <- aq_raw %>%
  mutate(
    date = dmy(Date),
    time_hms = hms(time),
    datetime = as.POSIXct(date) + as.numeric(time_hms),
    site_id = as.character(site_id),
    NO2 = as.numeric(.data[[no2_col]])
  ) %>%
  filter(!is.na(date)) %>%
  group_by(site_id, date) %>%
  summarise(
    NO2_mean = mean(NO2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(NO2_mean))  

saveRDS(aq_clean, AQ_CLEAN_PATH)
message(" Saved cleaned air quality daily data: ", AQ_CLEAN_PATH)
message("  Rows in aq_clean: ", nrow(aq_clean))

# =========================================================
# 2) Clean Open-Meteo weather data (daily means/sums)
# Open-Meteo time is ISO like "2023-01-01T00:00"
# =========================================================


if (!("time" %in% names(wx_raw))) {
  stop("Open-Meteo raw data does not have a 'time' column. Check names(wx_raw).")
}

wx_clean <- wx_raw %>%
  mutate(
    time2 = str_replace(time, "T", " "),
    datetime = ymd_hm(time, tz = "Europe/London"),
    date = as.Date(datetime)
  ) %>%
  group_by(date) %>%
  summarise(
    temp_mean = mean(temperature_2m, na.rm = TRUE),
    rh_mean   = mean(relative_humidity_2m, na.rm = TRUE),
    wind_mean = mean(wind_speed_10m, na.rm = TRUE),
    rain_sum  = sum(precipitation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(date))

saveRDS(wx_clean, WX_CLEAN_PATH)
message(" Saved cleaned weather daily data: ", WX_CLEAN_PATH)
message("  Rows in wx_clean: ", nrow(wx_clean))

# =========================================================
# 3) Join air quality with weather (by date)
# aq_clean has site_id + date, wx_clean has date
# =========================================================

aq_weather <- aq_clean %>%
  left_join(wx_clean, by = "date")

saveRDS(aq_weather, MERGED_PATH)
message(" Saved merged daily dataset: ", MERGED_PATH)
message("  Rows in aq_weather: ", nrow(aq_weather))

# ---- quick sanity checks ----
message("Date range (AQ): ", paste(range(aq_clean$date), collapse = " to "))
message("Date range (WX): ", paste(range(wx_clean$date), collapse = " to "))

# Look at first rows
print(dplyr::glimpse(aq_weather))

# View(aq_weather)（view the full version and verify)
