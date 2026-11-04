#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr across all_of any_of arrange as_tibble bind_rows coalesce
#'   desc distinct everything filter group_by if_else inner_join left_join
#'   mutate n n_distinct pick pull reframe select semi_join summarise tibble
#'   ungroup
#' @importFrom rlang .env .data := %||%
#' @importFrom Rdpack reprompt
#' @importFrom tidyr complete pivot_longer pivot_wider
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

# Column names used in dplyr calls
globalVariables(c(
  "amplitude", "angle", "applicable", "brk", "category", "cluster",
  "crest_factor", "delta", "deviation", "diameter", "dim1", "dim2",
  "duration", "extended", "feature", "filled", "first", "gain",
  "gain_duration", "gain_per_year", "gap_ok", "group", "h", "hex_x",
  "hex_y", "hi", "label", "limit", "lo", "max_gap",
  "max_util", "measure", "min_reserve", "n_groups", "n_reserve",
  "n_series", "n_violation", "n_weeks", "param", "peak_angle",
  "peak_week", "phase", "prevalence", "prevalent", "remainder", "reserve",
  "scale_group", "season", "season_autumn", "season_summer", "seasonal",
  "seasonal_strength", "series", "share_missing", "significant", "site",
  "site_label", "size", "svd_entropy",
  "trend", "trend_overall", "trend_strength", "unit", "util", "util_exceed",
  "util_max", "util_mean", "util_p95", "value", "value_scaled", "var_index",
  "variable", "vertex", "voltage", "week", "x", "xend", "y", "y_100",
  "y_scaled", "yend", "year"
))
