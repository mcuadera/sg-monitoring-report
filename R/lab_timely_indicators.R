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
