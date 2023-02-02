library(zoltr)
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
source("code/flusight_csv_to_json.R")

# Establish connection
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "CDC Influenza Hospitalization Forecasts", "url"]
the_models <- models(zoltar_connection, project_url)
model_abbreviations <- the_models$model_abbr
forecast_jobs <- vector("list", length(model_abbreviations))

for (j in 1:length(the_models$model_abbr)) {
  model_url <- the_models[the_models$model_abbr == model_abbreviations[j], "url"]
  forecast_paths <- list.files(path=paste("../Flusight-forecast-data/data-forecasts/", model_abbreviations[j], "/", sep=""), pattern=".csv", full.names=TRUE)

  for (i in 1:length(forecast_paths)) {
    start_position <- str_locate(forecast_paths[i], "20")[1, 1]
    temp_date <- str_sub(forecast_paths[i], start = start_position, end = start_position + 9)
    if (as.Date(temp_date, format="%Y-%m-%d") >=  floor_date(today(), unit="weeks", week_start=getOption("lubridate.week.start", 7))) {
      message(paste("Start converting and uploading", model_abbreviations[j]))
      forecast_data <- forecast_data_from_flusight_csv_file(forecast_paths[i])

      forecast_jobs[[i]] <- upload_forecast(
        zoltar_connection,
        model_url,
        timezero_date = temp_date,
        forecast_data = forecast_data,
        notes = ""
      )
    }
  }
}


for (i in 1:length(forecast_jobs)) if (length(forecast_jobs[[i]]) != 0) job_info(zoltar_connection, forecast_jobs[[i]])$status
