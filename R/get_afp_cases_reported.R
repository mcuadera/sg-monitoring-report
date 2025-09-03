get_afp_cases_reported <- function(afp_data, end_date = Sys.Date()) {

  afp_only <- afp_data |>
    dplyr::filter(!cdc.classification.all2 %in% c("NPAFP", "NOT-AFP")) |>
    dplyr::mutate(month = lubridate::month(dateonset, label = TRUE),
                  year = lubridate::year(dateonset))

  summary <- afp_only |>
    dplyr::group_by(whoregion, place.admin.0, year, month) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(month %in% format(seq(lubridate::floor_date(end_date, unit = "years"),
                                        end_date, by = "months"), format = "%b"),
                  year >= lubridate::year(end_date) - 3)

  current_year <- summary |>
    dplyr::filter(year == lubridate::year(end_date)) |>
    tidyr::pivot_wider(names_from = year, values_from = n)

  previous_year_min <- lubridate::year(end_date) - 3
  previous_year_max <- lubridate::year(end_date) - 1
  previous_years <- summary |>
    dplyr::filter(year != lubridate::year(end_date)) |>
    dplyr::group_by(whoregion, place.admin.0, month) |>
    dplyr::summarize(n = median(n)) |>
    dplyr::rename(!!paste0(previous_year_min, "-", previous_year_max) := n)

  final_summary <- dplyr::left_join(previous_years, current_year)

}
