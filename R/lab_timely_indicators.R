#' Calculate Lab Timeliness Indicators
#'
#' @description
#' The function calculate the median timeliness interval for the current year up to the end date and
#' the median of the previous three years.
#'
#' @details
#' To request updated lab data, email Minh-Ly's group.
#'
#' The yearly median is only calculated up to the same month/day of the end date starting on Jan 1st.
#' That is if the end date is 2025-08-15, then the 2025 median will be from 2025-01-01 to 2025-08-15,
#' the 2024 median will be from 2024-01-01 to 2024-08-15. The three year median will be based on
#' 2022-2024 medians.
#'
#' @param lab_data `tibble` Lab data.
#' @param end_date `str` End date of the calculations. Defaults to `Sys.Date()`.
#'
#' @returns `tibble` A summary of median timeliness
#' @export
#'
#' @examples
#' \dontrun{
#' lab_data <- readr::read_csv("lab_data.csv")
#' lab_timely_indicators(lab_data)
#' }
lab_timely_indicators <- function(lab_data, end_date = Sys.Date()) {
  lab_data <- get_lab_intervals(lab_data)
  start_date_3_year <- lubridate::floor_date(end_date - lubridate::years(3), unit = "years")
  end_date_3_year <- end_date - lubridate::years(1)
  timely_intervals <- c(
    "days.lab.culture",
    "days.culture.itd",
    "days.seq.ship",
    "days.seq.rec.res"
  )

  # Calculate 3-year medians
  medians_3_years <- NULL
  for (i in timely_intervals) {
    indicator_median <- suppressMessages(get_year_lab_median(lab_data, i,
                                               start_date_3_year,
                                               end_date_3_year))
    medians_3_years <- dplyr::bind_rows(medians_3_years, indicator_median)

  }

  # Current year
  median_current_year <- NULL
  for (i in timely_intervals) {
    indicator_median <- suppressMessages(get_year_lab_median(lab_data, i,
                                            lubridate::floor_date(end_date, unit = "year"),
                                            end_date))
    median_current_year <- dplyr::bind_rows(median_current_year, indicator_median)
  }
  # Rename to only have 1 year
  colnames(median_current_year)[length(median_current_year)] <- paste0(lubridate::year(end_date), " median")

  lab_interval_summary <- dplyr::left_join(medians_3_years, median_current_year,
                                           by = dplyr::join_by(whoregion, country, interval))

  # Doing it this way because column names dynamically change but not the position
  lab_interval_summary["difference"] <- lab_interval_summary[5] - lab_interval_summary[4]
  lab_interval_summary <- lab_interval_summary |>
    dplyr::mutate(difference = as.numeric(difference),
                  absolute_diff = abs(difference)) |>
    dplyr::arrange(interval, dplyr::desc(difference))

  return(lab_interval_summary)

}

# Private function

#' Obtain the median of medians across years
#'
#' @description
#' The function calculates the median interval for the lab timeliness indicators.
#' It summarizes data at the yearly level and then takes the median/average of the years
#' between the start and end dates.
#'
#'
#' @param lab_data `tibble` Lab data obtained from WHO SLD team.
#' @param indicator `str` Lab timeliness interval. Valid values are:
#' - days.lab.culture
#' - days.culture.itd
#' - days.seq.ship
#' - days.seq.rec.res
#' @param start_date `str` Start date of the analysis. YYYY-MM-DD format.
#' @param end_date `str` End date of the analysis. YYYY-MM-DD format.
#'
#' @returns `tibble` Summary of the median
#' @keywords internal
#'
#' @examples
get_year_lab_median <- function(lab_data, indicator, start_date, end_date) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  valid_indicators <- c(
    "days.lab.culture",
    "days.culture.itd",
    "days.seq.ship",
    "days.seq.rec.res"
  )

  if (!indicator %in% valid_indicators) {
    cli::cli_alert_warning("Not a valid indicator. Please use: ")
    cli::cli_li(valid_indicators)
    cli::cli_abort("Pass a valid indicator and try again.")

  }


  # This is t1-t4, where TRUE values are valid dates
  indicator_filter <- switch(
    indicator,
    "days.lab.culture" = "t1",
    "days.culture.itd" = "t2",
    "days.seq.ship" = "t3",
    "days.seq.rec.res" = "t4"
  )

  summary <- lab_data |>
    dplyr::filter(
      dplyr::between(DateStoolCollected, start_date, end_date),
      !!dplyr::sym(indicator_filter)
    ) |>
    dplyr::group_by(whoregion, country, year) |>
    dplyr::summarize(dplyr::across(
      dplyr::any_of(indicator),
      \(x) median(x, na.rm = TRUE)
    )) |>
    dplyr::ungroup() |>
    dplyr::group_by(whoregion, country) |>
    dplyr::summarize(dplyr::across(
      dplyr::any_of(indicator),
      \(x) median(x, na.rm = TRUE)
    )) |>
    tidyr::pivot_longer(cols =  dplyr::any_of(indicator),
                        names_to = "interval",
    values_to = paste0(lubridate::year(start_date), "-",
                       lubridate::year(end_date),
                       " median"))

  return(summary)
}
