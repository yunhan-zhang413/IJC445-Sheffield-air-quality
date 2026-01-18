# =========================================================
# 03_analysis_plots.R
# Analysis & visualisation for Sheffield NO2 & weather (2023)
# =========================================================

library(tidyverse)
library(lubridate)
library(ggplot2)

# ---- load merged daily data ----
aq_weather <- readRDS("data/aq_weather_daily_2023.rds")

# quick check
glimpse(aq_weather)

ggplot(aq_weather, aes(x = date, y = NO2_mean, colour = site_id)) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Daily NO₂ concentrations in Sheffield (2023)",
    x = "Date",
    y = "Daily mean NO₂ (µg/m³)",
    colour = "Monitoring site"
  ) +
  theme_minimal()

aq_weather <- aq_weather %>%
  mutate(
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    )
  )

ggplot(aq_weather, aes(x = season, y = NO2_mean, fill = season)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Seasonal distribution of daily NO₂ concentrations (2023)",
    x = "Season",
    y = "Daily mean NO₂ (µg/m³)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(aq_weather, aes(x = temp_mean, y = NO2_mean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "blue") +
  labs(
    title = "Relationship between temperature and NO₂ concentrations",
    x = "Daily mean temperature (°C)",
    y = "Daily mean NO₂ (µg/m³)"
  ) +
  theme_minimal()

ggplot(aq_weather, aes(x = wind_mean, y = NO2_mean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "darkgreen") +
  labs(
    title = "Relationship between wind speed and NO₂ concentrations",
    x = "Daily mean wind speed (m/s)",
    y = "Daily mean NO₂ (µg/m³)"
  ) +
  theme_minimal()

ggplot(aq_weather, aes(x = rh_mean, y = NO2_mean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "purple") +
  labs(
    title = "Relationship between relative humidity and NO₂ concentrations",
    x = "Daily mean relative humidity (%)",
    y = "Daily mean NO₂ (µg/m³)"
  ) +
  theme_minimal()

cor_results <- aq_weather %>%
  select(NO2_mean, temp_mean, wind_mean, rh_mean, rain_sum) %>%
  drop_na() %>%
  cor(method = "pearson")

round(cor_results["NO2_mean", ], 3)
