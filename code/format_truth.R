#' Format a flusight truth CSV to Zoltar format
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
format_truth <- function(truth_file, date_column="date", unit_column="location", value_column = "value", target_name=NULL, start_date=NULL, save_file=FALSE, save_file_path=NULL) {
  library(dplyr)
  library(lubridate)

  formatted_truth <- truth_file %>%
    rename("timezero" = all_of(date_column), "unit" = all_of(unit_column), "value" = all_of(value_column)) %>%
    select(timezero, unit, value) %>%
    mutate(target = target_name, .before = value, timezero = floor_date(timezero, "week", 1))

  if (!is.null(start_date)) formatted_truth <- filter(formatted_truth, timezero >= start_date)

  if (save_file) { write.csv(formatted_truth, file=save_file_path, row.names=FALSE)}

  return (formatted_truth)
}
