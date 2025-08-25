#' Timeliness of WPV/VDPV Detection from ES Samples
#' @description Days between ES sample collection and final laboratory result for WPV/VDPV positive ES samples (Targets: 35 days for full capacity labs, 46 days for limited capacity labs; ≥80% target)
#' @param es_data,region,country,year,group_by,end_date Data frame and optional filters: region(s), country(ies), year(s) (single year for starting year or range), grouping (group by = ""), and reference date (default Sys.Date())
#' @return Grouped tibble with WPV/VDPV detection timeliness, percentage meeting 35- and 46-day targets, and median days to final result as of current month per year
es_wpv.vdpv_detection_timeliness <- function(es_data, region = NULL, country = NULL, year = NULL, group_by = NULL, end_date = Sys.Date()) {
  # Apply filters
  if(!is.null(region)) es_data <- es_data |> dplyr::filter(toupper(trimws(who.region)) %in% toupper(trimws(region)))
  if(!is.null(country)) es_data <- es_data |> dplyr::filter(toupper(trimws(ADM0_NAME)) %in% toupper(trimws(country)))
  
  # Filter for WPV/VDPV positive ES samples first
  es_data <- es_data |> dplyr::filter(wpv == 1 | vdpv == 1)
  if(nrow(es_data) == 0) {message("* No WPV/VDPV positive ES samples found for the selected parameters.")
    return(dplyr::tibble())}
  
  # Determine years and filter
  available_years <- unique(lubridate::year(es_data$collection.date))
  current_year <- lubridate::year(end_date)
  years_to_use <- if(!is.null(year)) {
    intersect(if(length(year) == 1) year:current_year else year, available_years)} else available_years
  
  es_data <- es_data |> dplyr::filter(lubridate::year(collection.date) %in% years_to_use)
  if(nrow(es_data) == 0) {message("* No WPV/VDPV positive ES samples data available for the selected year(s) and location. Change filtering parameters or default to process all available data.")
    return(dplyr::tibble())}
  
  # Data validation and processing
  total_wpv_vdpv_detected <- nrow(es_data)
  both_dates_available <- sum(!is.na(es_data$collection.date) & !is.na(es_data$date.final.combined.result))
  invalid_records <- sum(as.Date(es_data$date.final.combined.result) < as.Date(es_data$collection.date), na.rm = TRUE)
  
  valid_data <- es_data |>
    dplyr::filter(!is.na(collection.date) & !is.na(date.final.combined.result) & 
                    as.Date(date.final.combined.result) >= as.Date(collection.date)) |>
    dplyr::mutate(
      days_to_final_result = as.numeric(as.Date(date.final.combined.result) - as.Date(collection.date)),
      year = lubridate::year(collection.date),
      meets_full_capacity_target = days_to_final_result <= 35,
      meets_limited_capacity_target = days_to_final_result <= 46)
  
  wpv_count <- sum(valid_data$wpv == 1, na.rm = TRUE)
  vdpv_count <- sum(valid_data$vdpv == 1, na.rm = TRUE)
  both_count <- sum(valid_data$wpv == 1 & valid_data$vdpv == 1, na.rm = TRUE)
  
  # Data quality messaging
  if(total_wpv_vdpv_detected != both_dates_available || invalid_records > 0) {
    message(paste("* Data quality: Total WPV/VDPV detected for ES samples =", total_wpv_vdpv_detected, 
                  "| Data with Collection and Final Result dates =", both_dates_available, 
                  "| Invalid Data (Final Result < Collection) =", invalid_records, 
                  "|| Valid Data by Type: WPV =", wpv_count, "| VDPV =", vdpv_count, "| Both WPV+VDPV =", both_count))}
  
  # Determine grouping
  group_vars <- if(isTRUE(group_by == "region")) c("who.region", "year") else 
    if(isTRUE(group_by == "country")) c("who.region", "ADM0_NAME", "year") else 
      if(!is.null(region) && is.null(group_by)) c("who.region", "year") else 
        c("who.region", "ADM0_NAME", "year")
  
  # Calculate results and median
  current_month <- lubridate::month(end_date)
  median_column_name <- paste0("Median Days to Final Result (Jan-", month.abb[current_month], ")")
  
  median_df <- valid_data |>
    dplyr::filter(lubridate::year(collection.date) != current_year |
                    (lubridate::year(collection.date) == current_year & 
                       lubridate::month(collection.date) <= current_month)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(!!median_column_name := round(median(days_to_final_result, na.rm = TRUE), 1), .groups = "drop")
  
  result <- valid_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      total_wpv_vdpv_detected = n(),
      wpv_detected = sum(wpv == 1, na.rm = TRUE),
      vdpv_detected = sum(vdpv == 1, na.rm = TRUE),
      detection_within_35days = sum(meets_full_capacity_target, na.rm = TRUE),
      pct_full_capacity_lab = round(detection_within_35days / n() * 100, 1),
      meets_80pct_target_35days = ifelse(pct_full_capacity_lab >= 80, "Yes", "No"),
      detection_within_46days = sum(meets_limited_capacity_target, na.rm = TRUE),
      pct_without_full_capacity_lab = round(detection_within_46days / n() * 100, 1),
      meets_80pct_target_46days = ifelse(pct_without_full_capacity_lab >= 80, "Yes", "No"),
      detection_beyond_46days = sum(!meets_limited_capacity_target, na.rm = TRUE),
      pct_beyond_46days = round(detection_beyond_46days / n() * 100, 1),
      .groups = "keep") |>
    dplyr::left_join(median_df, by = group_vars) |>
    dplyr::mutate(year = ifelse(year == current_year, paste0(year, "*"), as.character(year))) |>
    dplyr::rename_with(~ case_when(. == "who.region" ~ "region", . == "ADM0_NAME" ~ "country", TRUE ~ .))
  
  if(current_year %in% years_to_use) message(paste0("* Note: ", current_year, " is still in progress..."))
  message("* WPV/VDPV detection timeliness (targets: 35 days full capacity, 46 days limited capacity, ≥80% target for both)")
  result |> print(width = Inf)
}
