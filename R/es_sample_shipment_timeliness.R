#' #' Timeliness of ES Shipment
#' @description Days between ES sample collection and lab receipt of shipped samples (Target: 3 days for Domestic Labs and 7 days for International Labs)
#' @param es_data,region,country,year,group_by,end_date Data frame and optional filters: region(s), country(ies), year(s) (single year for starting year or range), grouping (group by = ""), and reference date (default Sys.Date())
#' @return Grouped tibble with lab receipt timeliness and median as of current month per year
es_sample_shipment_timeliness <- function(es_data, region = NULL, country = NULL, year = NULL, group_by = NULL, end_date = Sys.Date()) {
  # Apply filters
  if(!is.null(region)) es_data <- es_data |> dplyr::filter(toupper(trimws(who.region)) %in% toupper(trimws(region)))
  if(!is.null(country)) es_data <- es_data |> dplyr::filter(toupper(trimws(ADM0_NAME)) %in% toupper(trimws(country)))

  # Determine years and filter
  available_years <- unique(lubridate::year(es_data$collection.date))
  current_year <- lubridate::year(end_date)
  if(!is.null(year)) {requested_years <- if(length(year) == 1) year:current_year else year
    years_to_use <- intersect(requested_years, available_years)}
  else {years_to_use <- available_years}

  es_data <- es_data |> dplyr::filter(lubridate::year(collection.date) %in% years_to_use)
  if(nrow(es_data) == 0) {message("* No data available for the selected year(s) and location. Change filtering parameters or default to process all available data.")
    return(dplyr::tibble())}

  # Data validation and lab type processing
  total_samples <- nrow(es_data)
  both_dates_available <- sum(!is.na(es_data$collection.date) & !is.na(es_data$date.received.in.lab))
  invalid_records <- sum(as.Date(es_data$date.received.in.lab) < as.Date(es_data$collection.date), na.rm = TRUE)

  valid_data <- es_data |>
    dplyr::filter(!is.na(collection.date) & !is.na(date.received.in.lab) & as.Date(date.received.in.lab) >= as.Date(collection.date)) |>
    dplyr::mutate(
      days_to_lab_receipt = as.numeric(as.Date(date.received.in.lab) - as.Date(collection.date)),
      year = lubridate::year(collection.date),
      lab_type = case_when(tolower(trimws(es.lab.type)) == "in-country" ~ "domestic_lab", tolower(trimws(es.lab.type)) == "international" ~ "international_lab", TRUE ~ "no.es.lab.type"),
      target_days = ifelse(lab_type == "domestic_lab", 3, 7),
      meets_target = ifelse(lab_type == "no.es.lab.type", NA, days_to_lab_receipt <= target_days))

  domestic_count <- sum(valid_data$lab_type == "domestic_lab"); international_count <- sum(valid_data$lab_type == "international_lab"); no_lab_type_count <- sum(valid_data$lab_type == "no.es.lab.type")
  has_no_lab_types <- no_lab_type_count > 0

  # Conditional quality messaging
  if(total_samples != both_dates_available || invalid_records > 0 || has_no_lab_types) {
    message(paste("* Data quality: Total ES Samples =", total_samples, "| Data with Collection and Lab receipt dates =", both_dates_available, "| Invalid Data (Lab receipt < Collection) =", invalid_records, "|| Valid Data Lab Type: Domestic =", domestic_count, "| International =", international_count, "| No Lab Type =", no_lab_type_count))}

  # Determine grouping
  group_vars <- if(isTRUE(group_by == "region")) c("who.region", "year") else if(isTRUE(group_by == "country")) c("who.region", "ADM0_NAME", "year") else if(!is.null(region) && is.null(group_by)) c("who.region", "year") else c("who.region", "ADM0_NAME", "year")

  # Calculate ES lab receipt timeliness results
  result <- valid_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      valid_es_samples = n(),
      valid_with_lab_type = sum(!is.na(meets_target)),
      lab_receipt_within_target = sum(meets_target, na.rm = TRUE),
      pct_lab_receipt_within_target = ifelse(sum(!is.na(meets_target)) == 0, NA, round(lab_receipt_within_target / sum(!is.na(meets_target)) * 100, 1)),
      valid_with_no_lab_type_data = if(has_no_lab_types) sum(lab_type == "no.es.lab.type") else NULL,
      pct_no_lab_type_within_3days = if(has_no_lab_types) ifelse(sum(lab_type == "no.es.lab.type") == 0, NA, round(sum(lab_type == "no.es.lab.type" & days_to_lab_receipt <= 3, na.rm = TRUE) / sum(lab_type == "no.es.lab.type") * 100, 1)) else NULL,
      pct_no_lab_type_within_7days = if(has_no_lab_types) ifelse(sum(lab_type == "no.es.lab.type") == 0, NA, round(sum(lab_type == "no.es.lab.type" & days_to_lab_receipt <= 7, na.rm = TRUE) / sum(lab_type == "no.es.lab.type") * 100, 1)) else NULL,
      .groups = "keep")

  # Calculate median up to current month for each year
  current_month <- lubridate::month(end_date)
  median_column_name <- paste0("Median Days Taken (Jan-", month.abb[current_month], ")")

  median_df <- valid_data |>
    dplyr::filter(lubridate::year(collection.date) != current_year |
                    (lubridate::year(collection.date) == current_year & lubridate::month(collection.date) <= current_month)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(!!median_column_name := round(median(days_to_lab_receipt, na.rm = TRUE), 1),
                     .groups = "drop")

  result <- dplyr::left_join(result, median_df, by = group_vars)

  # Result for selection
  result <- result |>
    dplyr::select_if(~ !all(is.null(.))) |>
    dplyr::mutate(year = ifelse(year == current_year, paste0(year, "*"), as.character(year))) |>
    dplyr::rename_with(~ case_when(. == "who.region" ~ "region", . == "ADM0_NAME" ~ "country", TRUE ~ .))

  if(current_year %in% years_to_use) message(paste0("* Note: ", current_year, " is still in progress..."))
  message("* ES samples lab receipt timeliness (in-country: 3 days, international: 7 days)")
  result |> print(width = Inf)
}
