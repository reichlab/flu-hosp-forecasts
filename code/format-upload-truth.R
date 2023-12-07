library(zoltr)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

# Establish connection
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

# Get projects and associated info
the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "CDC FluSight Forecast Hub", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
the_models <- models(zoltar_connection, project_url)

# Format truth data
source("code/format_truth.R")
raw_truth <- read_csv("../FluSight-forecast-hub/target-data/target-hospital-admissions.csv")
truth_date <- today()
truth_csv_path <- paste("data-truth/truth_inc-hosp_", truth_date, ".csv", sep="")
  formatted_truth <- raw_truth |>
    calculate_categories() |>
    format_multiple_target_truth(
      date_column="date", unit_column="location", 
      target_variables=c("1 wk inc flu hosp", "1 wk flu hosp rate change"), 
      target_columns=c("value", "weekly_rate"),
      start_date = as.Date("2023-09-03"), 
      save_file=TRUE, save_file_path=truth_csv_path
    )

# upload truth data
upload_truth(zoltar_connection, project_url, truth_csv_path)


