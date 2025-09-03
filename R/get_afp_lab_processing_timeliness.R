#' Timeliness of lab processing
#'
#' @description
#' Calculates the median time AFP samples arrive in lab to final rRT-PCR results
#' from the previous three months for the past three years.
#'
#' @param lab_data `tibble` Lab data with AFP samples.
#' @param end_date `str` End date of the analysis. Defaults to today's date.
#'
#' @returns `tibble` Summary table of median timeliness of sample received in lab
#' to final lab results.
#' @export
#'
#' @examples
#' \dontrun{
#' get_afp_lab_processing_timeliness(lab_data)
#' }
get_afp_lab_processing_timeliness <- function(lab_data, end_date = Sys.Date()) {

  end_date <- lubridate::as_date(end_date)
  end_date_month <- end_date - months(1)
  start_date_month <- end_date - months(3)

  # Define the three month periods
  included_months <- dplyr::tibble(dates = seq(start_date_month,
                                               end_date_month,
                                               by = "months")) |>
    dplyr::mutate(month = months(dates, abbreviate = TRUE)) |>
    dplyr::pull(month)

  # Check if there are data available for the current year
  check <- lab_data |>
    dplyr::filter(dplyr::between(CaseDate, start_date_month, end_date_month))
  if (nrow(check) == 0) {
    cli::cli_alert_warning(paste0("No data available for the previous three months from the end date.",
                                  "No calculations will be available in the year of the end date."))
  }

  three_month_summary <- lab_data |>
    # Lab processing timeliness
    # From date stool received in lab to final rRTPCR results
    dplyr::mutate(days.rec.lab.final = DateFinalrRTPCRResults - DateStoolReceivedinLab,
                  year_month = lubridate::floor_date(CaseDate, unit = "months"),
                  month = lubridate::month(CaseDate, label = TRUE)) |>
    # Filter erroneous data
    dplyr::filter(!is.na(.data$days.rec.lab.final),
                  (.data$days.rec.lab.final >= 0 & .data$days.rec.lab.final <= 365),
                  month %in% included_months) |>
    dplyr::group_by(whoregion, country, year, month) |>
    dplyr::summarize(median = median(days.rec.lab.final, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = year, values_from = median)

  return(three_month_summary)
}
