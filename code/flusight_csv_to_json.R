POINT_PREDICTION_CLASS <- "point"  # JSON prediction class for point prediction elements
BIN_PREDICTION_CLASS <- "bin"  # "" bin ""
QUANTILE_PREDICTION_CLASS <- "quantile"  # "" quantile ""



#' Convert a flusight forecasts CSV file to JSON
#'
#' @param flusight_csv_path path to file for reading in
#'
#' @return list object in JSON format ready for upload to Zoltar
#'
#' @details CSV file in flusight_csv_path should have the following columns
#'   - forecast_date
#'   - target
#'   - target_end_date
#'   - location
#'   - type
#'   - quantile
#'   - value
#'
#' @examples
forecast_data_from_flusight_csv_file <- function(flusight_csv_path) {
  flusight_data_frame <- readr::read_csv(flusight_csv_path) %>%
    mutate(location = as.character(location)) %>%
    filter(location != "78")
  forecast_data_from_flusight_data_frame(flusight_data_frame)
}


#' Convert a flusight forecasts data frame to JSON
#'
#' @param flusight_data_frame data frame of forecasts
#'
#' @return list object in JSON format ready for upload to Zoltar
#' @export
#'
#' @examples
forecast_data_from_flusight_data_frame <- function(flusight_data_frame) {
  library(dplyr)
#  names(flusight_data_frame) <- sapply(names(flusight_data_frame), tolower) # make col names lowercase

  # validate flusight_data_frame
  if (!(inherits(flusight_data_frame, "data.frame"))) {
    stop("flusight_data_frame was not a `data.frame`", call. = FALSE)
  }

  # ensure flusight_data_frame has all required columns, reorder if necessary
  col_names <- c("reference_date", "horizon", "target","target_end_date", "location", "output_type", "output_type_id", "value")
  if ((length(flusight_data_frame) == 0) || !all(names(flusight_data_frame) %in% col_names)) {
    stop("flusight_data_frame did not have required columns", call. = FALSE)
  } else if (!all(names(flusight_data_frame) == col_names) && all(names(flusight_data_frame) %in% col_names)) {
    flusight_data_frame <- flusight_data_frame %>% 
      relocate(reference_date, horizon, target, target_end_date, location, output_type, output_type_id, value)
  }
  
  predictions <- list()
  
  # group flusight_data_frame by forecast type (point or quantile)
  flusight_data_frame_grouped <- flusight_data_frame %>%
    tidyr::unite(horizon, target, col="target", sep=" ") %>%
    dplyr::group_by(.data$location, .data$target, .data$output_type) %>%
    dplyr::group_data()

    
  for (group_idx in seq_len(nrow(flusight_data_frame_grouped))) {
    group_row <- flusight_data_frame_grouped[group_idx,]
   if (!group_row$target %in% c(paste(-1:3, "wk inc flu hosp"), paste(-1:3, "wk flu hosp rate change"))) {
      stop("invalid target_name: '", group_row$target, "'", call. = FALSE)
    }

    point_values <- list()
    bincat_cats <- list()
    bincat_probs <- list()
    quantile_quants <- list()
    quantile_values <- list()
    
  # format forecasts into lists based on type
    for (group_rows_idx in seq_along(group_row$.rows[[1]])) {
      flusight_data_frame_idx <- group_row$.rows[[1]][group_rows_idx]
      flusight_row <- flusight_data_frame[flusight_data_frame_idx,]
      
      if (group_row$output_type == POINT_PREDICTION_CLASS) {
        point_values <- append(point_values, as.numeric(flusight_row$value))
      } else if (group_row$output_type == QUANTILE_PREDICTION_CLASS) {
        quantile_quant_and_value <- list(as.numeric(flusight_row$output_type_id), as.numeric(flusight_row$value))
        quantile_quants <- append(quantile_quants, quantile_quant_and_value[[1]])
        quantile_values <- append(quantile_values, quantile_quant_and_value[[2]])
      } else {
        bin_cat_and_prob <- list(flusight_row$output_type_id, flusight_row$value)
        bincat_cats <- append(bincat_cats, bin_cat_and_prob[[1]])
        bincat_probs <- append(bincat_probs, bin_cat_and_prob[[2]])
      }
    }

    # format nested list for point predictions
    if (length(point_values) > 0) {
      if (length(point_values) > 1) {
        stop("length(point_values) > 1: ", point_values, call. = FALSE)
      }
      point_value <- point_values[[1]]
      prediction <- list("unit" = group_row$location, "target" = group_row$target, "class" = POINT_PREDICTION_CLASS,
                         "prediction" = list("value" = point_value))
      predictions[[length(predictions) + 1]] <- prediction
    }
    
    # format nested list for quantile predictions
    if (length(quantile_quants) >= 1) {
      prediction <- list("unit" = group_row$location, "target" = group_row$target, "class" = QUANTILE_PREDICTION_CLASS,
                         "prediction" = list("quantile" = quantile_quants, "value" = quantile_values))
      predictions[[length(predictions) + 1]] <- prediction
    }
    # format nested list for bin predictions
    if (length(bincat_cats) >= 1) {  # yes warning: "NAs introduced by coercion"
      prediction <- list("unit" = group_row$location, "target" = group_row$target, "class" = BIN_PREDICTION_CLASS,
                         "prediction" = list("cat" = bincat_cats, "prob" = bincat_probs)) # create list object of single group
      predictions[[length(predictions) + 1]] <- prediction # create list of prediction objects
    }
  }

  list("predictions" = predictions)

}

