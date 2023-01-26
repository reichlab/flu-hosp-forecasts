library(zoltr)
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)

# Establish connection
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

# Get projects and associated info
the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "CDC Influenza Hospitalization Forecasts", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
the_models <- models(zoltar_connection, project_url)

# Format truth data
source("code/format_truth.R")
raw_truth <- read_csv("../Flusight-forecast-data/data-truth/truth-Incident Hospitalizations.csv")
truth_date <- floor_date(today(), unit="weeks", week_start=getOption("lubridate.week.start", 1))
truth_csv_path <- paste("data-truth/truth_inc-hosp_", truth_date, ".csv", sep="")
formatted_truth <- format_truth(raw_truth, date_column="date", unit_column="location", value_column = "value", target_name="1 wk ahead inc flu hosp", start_date=as.Date("2021-12-13"), save_file=TRUE, save_file_path=truth_csv_path)

# upload truth data
upload_truth(zoltar_connection, project_url, truth_csv_path)


