#' Format a flusight truth CSV with a single target to Zoltar format
#'
#' @param truth_file a data frame of truth data
#' @param date_column date column name as a string
#' @param unit_column unit (location) column name as a string
#' @param value_column value column name as a string
#' @param target_name forecast target name as a string
#' @param start_date date from which truth data should start as a string
#' @param save_file logical describing whether the formatted truth file should be saved, defaults to FALSE
#' @param save_file_path filepath to which the formatted truth data should be saved if `save_file` = TRUE
#'
#' @return A data frame in the Zoltar truth CSV format with columns `timezero`, `unit`, `target`, and `value`
#' @export
#'
#' @examples
format_single_target_truth <- function(truth_file, date_column="date", unit_column="location", value_column = "value", target_name=NULL, start_date=NULL, save_file=FALSE, save_file_path=NULL) {
  library(dplyr)
  library(lubridate)

  formatted_truth <- truth_file %>%
    rename("timezero" = all_of(date_column), "unit" = all_of(unit_column), "value" = all_of(value_column)) %>%
    select(timezero, unit, value) %>%
    mutate(
      target = target_name, .before = value, 
      timezero = floor_date(timezero + weeks(1), "week", 6)
    )

  if (!is.null(start_date)) formatted_truth <- filter(formatted_truth, timezero >= start_date)

  if (save_file) { write.csv(formatted_truth, file=save_file_path, row.names=FALSE)}

  return (formatted_truth)
}


#' Format a flusight truth CSV with a single target to Zoltar format
#'
#' @param truth_file a data frame of truth data
#' @param date_column date column name as a string
#' @param unit_column unit (location) column name as a string
#' @param value_column value column name as a string
#' @param target_variables forecast target names as a vector of strings
#' @param target_columns forecast target column names as a vector of strings
#' @param start_date date from which truth data should start as a string
#' @param save_file logical describing whether the formatted truth file should be saved, defaults to FALSE
#' @param save_file_path filepath to which the formatted truth data should be saved if `save_file` = TRUE
#'
#' @return A data frame in the Zoltar truth CSV format with columns `timezero`, `unit`, `target`, and `value`
#' @export
#'
#' @examples
format_multiple_target_truth <- function(truth_file, date_column="date", unit_column="location", target_variables=NULL, target_columns=NULL, start_date=NULL, save_file=FALSE, save_file_path=NULL) {
  formatted_truth <- truth_file |>
    dplyr::mutate(value=as.character(value)) |>
    dplyr::rename("timezero" = all_of(date_column), "unit" = all_of(unit_column), "target_1" = all_of(target_columns[1])) |>
    tidyr::pivot_longer(cols=(ncol(truth_file)-length(target_variables)+1):ncol(truth_file), names_to="target", values_to="value") |>
    dplyr::mutate(
      target = ifelse(target=="target_1", target_variables[1], target_variables[2]),
      timezero = lubridate::floor_date(timezero, "week", 6) - weeks(1)
    ) |>
    dplyr::select(timezero, unit, target, value)
  
  if (!is.null(start_date)) formatted_truth <- dplyr::filter(formatted_truth, timezero >= start_date)

  if (save_file) { write.csv(formatted_truth, file=save_file_path, row.names=FALSE)}
  
  return (formatted_truth)
}


#' Calculate hosp rate change category for flu truth
#'
#' @param raw_truth a data frame of raw, unformatted truth data
#'
#' @return A data frame in the raw, unformatted truth CSV format with columns `date`, `location`, `value`, and `cat`
#' @export
#'
#' @examples
calculate_categories <- function(raw_truth) {
  location_data <- readr::read_csv(file = "../Flusight-forecast-hub/auxiliary-data/locations.csv") |>
    dplyr::mutate(geo_value = tolower(abbreviation)) |>
    dplyr::select(location, count_rate1:count_rate2p5)
  
  prior_truth <- raw_truth |>
    dplyr::select(date, location, value) |>
    dplyr::rename(prior_value = value)
  
  categorical_truth <- raw_truth |>
    dplyr::mutate(prior_week = date-weeks(1)) |>
    dplyr::left_join(prior_truth, by=c("prior_week"="date", "location")) |>
    dplyr::left_join(location_data, by="location") |>
    dplyr::mutate(rate_change=value-prior_value) |>
    dplyr::select(date, location, value, rate_change, count_rate1,count_rate2) |>
    dplyr::mutate(cat=case_when(
      rate_change >= ifelse(count_rate2 > 10, count_rate2, 10) ~ "large_increase",
      rate_change >= ifelse(count_rate1 > 10, count_rate1, 10) & rate_change < ifelse(count_rate2 > 10, count_rate2, 10) ~ "increase",
      rate_change <= ifelse(count_rate1 > 10, -count_rate1, -10) & rate_change > ifelse(count_rate2 > 10, -count_rate2, -10) ~ "decrease",
      rate_change <= ifelse(count_rate2 > 10, -count_rate2, -10) ~ "large_decrease",
      rate_change < ifelse(count_rate1 > 10, count_rate1, 10) & rate_change > ifelse(count_rate1 > 10, -count_rate1, -10) ~ "stable"
    )) |>
    dplyr::select(date, location, value, cat)
  
  return (categorical_truth)
}
