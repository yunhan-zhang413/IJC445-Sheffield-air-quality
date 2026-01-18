# =========================================================
# 01_get_data.R
# Download air quality from DEFRA UK-AIR (flat files)
# + weather from Open-Meteo (archive)
# =========================================================

library(tidyverse)
library(lubridate)
library(httr)
library(rvest)
library(readr)
library(jsonlite)
library(purrr)
library(stringr)
library(dplyr)

dir.create("data", showWarnings = FALSE)

# ---- settings ----
SITE_IDS <- c("SHDG", "SHE", "SHBR")  # Sheffield sites (UK-AIR)
YEAR <- 2023                                   # year you want
TARGET_POLLUTANT <- "NO2"                    

# Open-Meteo location (Sheffield centre approx.)
LAT <- 53.3811
LON <- -1.4701
DATE_FROM <- as.Date("2023-01-01")
DATE_TO   <- as.Date("2023-12-31")

AQ_RAW_PATH <- file.path("data", paste0("ukair_raw_", YEAR, ".rds"))
WX_RAW_PATH <- file.path("data", paste0("openmeteo_raw_", YEAR, ".rds"))

# ---------------------------------------------------------
# Helper: find CSV links from UK-AIR flat_files page
# ---------------------------------------------------------


get_ukair_csv_links <- function(site_id) {
  url <- paste0("https://uk-air.defra.gov.uk/data/flat_files?site_id=", site_id)
  
  pg <- read_html(url)
  
  links <- pg %>%
    html_elements("a") %>%
    html_attr("href") %>%
    discard(is.na) %>%
    keep(~ str_detect(.x, "\\.csv"))
  
  links <- map_chr(
    links,
    ~ if_else(
      str_starts(.x, "http"),
      .x,
      paste0("https://uk-air.defra.gov.uk", .x)
    )
  )
  
  unique(links)
}

# ---------------------------------------------------------
# Helper: read UK-AIR CSV robustly (skip metadata rows)
# UK-AIR CSV often has a few metadata lines before header.
# We find the first line that begins with "Date" and read from there.
# ---------------------------------------------------------
read_ukair_csv <- function(csv_url) {
  tmp <- tempfile(fileext = ".csv")
  download.file(csv_url, tmp, mode = "wb", quiet = TRUE)
  
  lines <- readLines(tmp, warn = FALSE)
  header_row <- which(str_detect(lines, "^Date"))[1]
  
  if (is.na(header_row)) {
    # fallback: try "date" in case different capitalisation
    header_row <- which(str_detect(lines, "^[Dd]ate"))[1]
  }
  if (is.na(header_row)) stop("Could not find header row starting with 'Date' in file: ", csv_url)
  
  df <- readr::read_csv(
    tmp,
    skip = header_row - 1,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  df
}

# ---------------------------------------------------------
# 1) Download UK-AIR data for each site (one year)
# Strategy:
#   - scrape flat_files page for site
#   - pick the CSV that matches the year (e.g., contains "2023")
#   - read and keep
# ---------------------------------------------------------
site_data <- list()

for (sid in SITE_IDS) {
  message("Fetching links for site: ", sid)
  links <- get_ukair_csv_links(sid)
  
  # Pick a "one-year" file for that site and year.
  # UK-AIR link patterns vary; most include the year in filename.
  year_links <- links[str_detect(links, as.character(YEAR))]
  
  if (length(year_links) == 0) {
    stop("No CSV link found for site ", sid, " in year ", YEAR,
         ". Open the flat_files page and check available years: https://uk-air.defra.gov.uk/data/flat_files?site_id=", sid)
  }
  
  # Prefer a file that looks like "all pollutants" (often contains "all" or no pollutant token)
  pick <- year_links[1]
  if (any(str_detect(year_links, regex("all", ignore_case = TRUE)))) {
    pick <- year_links[str_detect(year_links, regex("all", ignore_case = TRUE))][1]
  }
  
  message("Downloading: ", pick)
  df <- read_ukair_csv(pick)
  
  df <- df %>%
    mutate(site_id = sid)
  
  site_data[[sid]] <- df
}

aq_raw <- bind_rows(site_data)
saveRDS(aq_raw, AQ_RAW_PATH)
message("Saved UK-AIR raw data to: ", AQ_RAW_PATH)

# ---------------------------------------------------------
# 2) Download Open-Meteo weather (archive)
# ---------------------------------------------------------
get_json <- function(url, query = list()) {
  resp <- httr::GET(url, query = query, httr::user_agent("IJC437-coursework/1.0"))
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

openmeteo_archive <- "https://archive-api.open-meteo.com/v1/archive"

wx_json <- get_json(
  url = openmeteo_archive,
  query = list(
    latitude = LAT,
    longitude = LON,
    start_date = as.character(DATE_FROM),
    end_date = as.character(DATE_TO),
    hourly = paste(c("temperature_2m", "relative_humidity_2m", "wind_speed_10m", "precipitation"), collapse = ","),
    timezone = "Europe/London"
  )
)

wx_raw <- as_tibble(wx_json$hourly)
saveRDS(wx_raw, WX_RAW_PATH)
message("Saved Open-Meteo raw data to: ", WX_RAW_PATH)

stopifnot(file.exists(AQ_RAW_PATH))
stopifnot(file.exists(WX_RAW_PATH))
message("Step 1 finished. Files exist.")
