#' Obtain the timeliness interval between collection to final results of negative samples
#'
#' @description
#' Calculates the median timeliness of negative samples between collection date to final lab results.
#' The function will calculate for the year specified in the end date and the previous year.
#'
#' @param lab_data `tibble` Lab data.
#' @param end_date `str` End date of the analysis. Defaults to the current date.
#'
#' @returns `tibble` Summary table of timeliness
#' @export
#'
#' @examples
#' \dontrun{
#' get_negative_lab_processing_timeliness(lab_data)
#' }
get_negative_lab_processing_timeliness <- function(lab_data, end_date = Sys.Date()) {
  summary <- lab_data |>
    dplyr::mutate(month = lubridate::month(CaseDate, label = TRUE),
                  days.collect.notif.hq = as.numeric(DateNotificationtoHQ - DateStoolCollected)) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  !is.na(days.collect.notif.hq),
                  dplyr::between(days.collect.notif.hq, 0, 365),
                  FinalCellCultureResult %in% c("Negative", "NPEV", NA)
                  ) |>
    dplyr::group_by(whoregion, country, culture.itd.cat, year, month) |>
    dplyr::summarize(median = median(days.collect.notif.hq, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(year) |>
    tidyr::pivot_wider(names_from = year, values_from = median)

  return(summary)
}
