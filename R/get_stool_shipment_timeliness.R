#' Get timeliness of stool collection to shipment
#'
#' @description
#' Calculates the median timeliness from the year specified at the end date for
#' the time interval between stool collection to arrival in lab.
#'
#' @param lab_data `tibble` Lab data.
#' @param end_date `str`End date of the analysis. Defaults to current date.
#'
#' @returns `tibble` Summary of stool collection to shipment timeliness
#' @export
#'
#' @examples
#' \dontrun{
#' get_stool_shipment_timeliness(lab_data)
#' }
get_stool_shipment_timeliness <- function(lab_data, end_date = Sys.Date()) {

  end_date <- lubridate::as_date(end_date)

  summary <- lab_data |>
    dplyr::mutate(month = lubridate::month(CaseDate, label = TRUE),
                  days.collect.rec.lab = as.numeric(DateStoolReceivedinLab - DateStoolCollected)) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  month %in% format(seq(lubridate::floor_date(end_date, unit = "years"),
                                        end_date,
                                        by = "months"), format = "%b"),
                  !is.na(days.collect.rec.lab),
                  dplyr::between(days.collect.rec.lab, 0, 365)) |>
    dplyr::group_by(whoregion, country, culture.itd.cat, year, month) |>
    dplyr::summarize(median = median(days.collect.rec.lab, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(year) |>
    tidyr::pivot_wider(names_from = year, values_from = median)

  cli::cli_alert_info("Note: If end date is not the month end, comparisons from the previous year may be inaccurate")

  return(summary)

}
