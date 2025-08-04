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

  # Calculate 3-year median
  median_3_years <- lab_data |>
    dplyr::filter(dplyr::between(
      DateStoolCollected,
      start_date_3_year,
      end_date_3_year
    )) |>
    dplyr::group_by(whoregion, country, year) |>
    dplyr::summarize(dplyr::across(
      dplyr::any_of(c(
        "days.lab.culture",
        "days.culture.itd",
        "days.seq.ship",
        "days.seq.rec.res"
      )),
      \(x) median(x, na.rm = TRUE)
    )) |>
    dplyr::ungroup() |>
    dplyr::group_by(whoregion, country) |>
    dplyr::summarize(dplyr::across(
      dplyr::any_of(c(
        "days.lab.culture",
        "days.culture.itd",
        "days.seq.ship",
        "days.seq.rec.res"
      )),
      \(x) median(x, na.rm = TRUE)
    )) |>
    tidyr::pivot_longer(cols =  dplyr::any_of(c(
      "days.lab.culture",
      "days.culture.itd",
      "days.seq.ship",
      "days.seq.rec.res"
    )), names_to = "interval",
    values_to = paste0(lubridate::year(start_date_3_year), "-",
                       lubridate::year(end_date_3_year), " median"))

  # Calculate current year
  median_current_year <- lab_data |>
    dplyr::filter(dplyr::between(
      DateStoolCollected,
      lubridate::as_date(paste0(lubridate::year(end_date), "-01-01")),
      end_date
    )) |>
    dplyr::group_by(whoregion, country) |>
    dplyr::summarize(dplyr::across(
      dplyr::any_of(c(
        "days.lab.culture",
        "days.culture.itd",
        "days.seq.ship",
        "days.seq.rec.res"
      )),
      \(x) median(x, na.rm = TRUE)
    )) |>
    tidyr::pivot_longer(cols =  dplyr::any_of(c(
      "days.lab.culture",
      "days.culture.itd",
      "days.seq.ship",
      "days.seq.rec.res"
    )), names_to = "interval", values_to = paste0(lubridate::year(end_date), " median"))

  lab_interval_summary <- dplyr::left_join(median_3_years, median_current_year)

  # Doing it this way because column names dynamically change but not the position
  lab_interval_summary["difference"] <- lab_interval_summary[5] - lab_interval_summary[4]
  lab_interval_summary <- lab_interval_summary |>
    dplyr::mutate(difference = as.numeric(difference),
                  absolute_diff = abs(difference)) |>
    dplyr::arrange(interval, dplyr::desc(difference))

  return(lab_interval_summary)

}
