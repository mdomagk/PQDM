#' Validate weekly 95th percentiles
#'
#' Checks weekly 95th percentiles and marks for each method whether the data
#' availability of a time series (one combination of `site`, `voltage`,
#' `phase` and `param`) suffices. Time series are only marked, not removed.
#'
#' Steps:
#' 1. Check the format.
#' 2. Replace `NaN`, `Inf` and, if `zero_is_missing`, `0` by `NA`.
#' 3. Complete the weekly grid of each time series.
#' 4. Data availability: the measurement duration counts from the first to
#'    the last week with a value.
#' 5. Applicability: every method requires at least `min_weeks` weeks;
#'    season, trend and forecast additionally a longest gap of at most
#'    `max_gap` weeks and a share of missing weeks of at most `max_missing`.
#'    The defaults follow \insertCite{domagk2022,domagk2023;textual}{PQDM}.
#'
#' Gaps are filled by [pq_fill()], not here.
#'
#' @param data A data frame in long format with the columns `site`, `voltage`,
#'   `week` (the Monday of the calendar week, class `Date`), `phase`, `param`
#'   and `value` (the weekly 95th percentile).
#' @param min_weeks Named numeric vector with the minimum measurement duration
#'   in weeks for `quality_reserve`, `season`, `trend` and `forecast`; at
#'   least one year (52 weeks) for `season` and 53 weeks for `trend`.
#' @param max_gap Maximum length of a gap in weeks.
#' @param max_missing Maximum share of missing weeks, between 0 and 1.
#' @param zero_is_missing Whether a value of exactly 0 marks a missing
#'   measurement, as for continuous quantities such as harmonics; `FALSE`
#'   keeps zeros, e.g. for counts.
#'
#' @return An object of class `pqdm_validation`, a list with
#'   \describe{
#'     \item{data}{The cleaned data on a complete weekly grid.}
#'     \item{series}{One row per time series with `first_week`, `last_week`,
#'       `n_weeks` (measurement duration), `n_missing`, `share_missing`,
#'       `max_gap` and one logical column per method.}
#'     \item{n_set_missing, n_added}{Number of values set to `NA` and of weeks
#'       added to the grid.}
#'     \item{settings}{The criteria used.}
#'   }
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' v <- pq_validate(pq_weekly_example)
#' v
#' summary(v)
#'
#' @export
pq_validate <- function(data,
                        min_weeks = c(quality_reserve = 1, season = 52,
                                      trend = 53, forecast = 157),
                        max_gap = 10,
                        max_missing = 0.2,
                        zero_is_missing = TRUE) {
  methods <- c("quality_reserve", "season", "trend", "forecast")
  check_min_weeks(min_weeks, methods)
  min_weeks <- min_weeks[methods]
  check_number(max_gap, min = 0)
  check_number(max_missing, min = 0, max = 1)
  check_flag(zero_is_missing)
  check_weekly_format(data)

  invalid <- is.nan(data$value) | is.infinite(data$value) |
    (zero_is_missing & data$value %in% 0)
  data <- mutate(data, value = replace(value, invalid, NA))

  grid <- data |>
    group_by(across(all_of(series_keys))) |>
    complete(week = seq(min(week), max(week), by = "week")) |>
    ungroup() |>
    arrange(across(all_of(series_keys)), week) |>
    select(all_of(names(data)))
  series <- grid |>
    summarise(series_availability(week, value), .by = all_of(series_keys)) |>
    mark_applicable(min_weeks, max_gap, max_missing)

  structure(
    list(
      data = grid,
      series = series,
      n_set_missing = sum(invalid),
      n_added = nrow(grid) - nrow(data),
      settings = list(min_weeks = min_weeks, max_gap = max_gap,
                      max_missing = max_missing,
                      zero_is_missing = zero_is_missing)
    ),
    class = "pqdm_validation"
  )
}

# Check that min_weeks has one value per method and at least one year for
# season and trend
check_min_weeks <- function(min_weeks, methods) {
  if (!is.numeric(min_weeks) || !setequal(names(min_weeks), methods) ||
      min_weeks[["season"]] < 52 || min_weeks[["trend"]] < 53) {
    stop("`min_weeks` must be a named numeric vector with the names ",
         paste(methods, collapse = ", "), ", at least 52 for season and 53 ",
         "for trend.", call. = FALSE)
  }
}

# Check the columns and the weekly grid of the long format
check_weekly_format <- function(data) {
  cols <- c(series_keys, "week", "value")
  if (!is.data.frame(data) || !all(cols %in% names(data))) {
    stop("`data` must be a data frame with the columns ",
         paste(cols, collapse = ", "), ".", call. = FALSE)
  }
  if (!inherits(data$week, "Date") || !is.numeric(data$value)) {
    stop("Column 'week' must be of class Date and 'value' numeric.",
         call. = FALSE)
  }
  if (anyNA(data[c(series_keys, "week")])) {
    stop("The columns ", paste(c(series_keys, "week"), collapse = ", "),
         " must not contain NA.", call. = FALSE)
  }
  if (any(format(data$week, "%u") != "1")) {
    stop("Column 'week' must contain the Monday of each week.", call. = FALSE)
  }
  if (anyDuplicated(data[c(series_keys, "week")])) {
    stop("Each week may occur only once per time series.", call. = FALSE)
  }
}

# Whether the data availability of each series suffices for each method:
# min_weeks for all, season, trend and forecast also max_gap and max_missing
mark_applicable <- function(series, min_weeks, max_gap, max_missing) {
  series |>
    mutate(
      gap_ok = n_weeks > 0 & max_gap <= .env$max_gap &
        share_missing <= .env$max_missing,
      quality_reserve = n_weeks >= min_weeks[["quality_reserve"]],
      season = gap_ok & n_weeks >= min_weeks[["season"]],
      trend = gap_ok & n_weeks >= min_weeks[["trend"]],
      forecast = gap_ok & n_weeks >= min_weeks[["forecast"]]
    ) |>
    select(!gap_ok)
}

# Data availability of one time series (sorted by week, complete grid)
series_availability <- function(week, value) {
  has_value <- which(!is.na(value))
  if (length(has_value) == 0) {
    return(tibble(first_week = as.Date(NA), last_week = as.Date(NA),
                  n_weeks = 0L, n_missing = 0L, share_missing = NA_real_,
                  max_gap = NA_integer_))
  }
  span <- range(has_value)
  missing <- is.na(value[span[1]:span[2]])
  runs <- rle(missing)
  tibble(
    first_week = week[span[1]],
    last_week = week[span[2]],
    n_weeks = length(missing),
    n_missing = sum(missing),
    share_missing = mean(missing),
    max_gap = as.integer(max(0L, runs$lengths[runs$values]))
  )
}

#' @export
print.pqdm_validation <- function(x, ...) {
  methods <- names(x$settings$min_weeks)
  n_applicable <- vapply(methods, \(m) sum(x$series[[m]]), integer(1))
  cat("<pqdm_validation> weekly 95th percentiles\n")
  cat(sprintf("  %d time series, %d value(s) set to NA, %d week(s) added%s\n",
              nrow(x$series), x$n_set_missing, x$n_added,
              if (is_filled(x)) sprintf(", %d filled", x$n_filled) else ""))
  cat("  applicable:", paste(methods, n_applicable, collapse = ", "), "\n")
  invisible(x)
}

#' @export
summary.pqdm_validation <- function(object, ...) {
  object$series
}
