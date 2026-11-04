#' Trends of weekly 95th percentiles
#'
#' Extracts the trend component of time series of weekly 95th percentiles and
#' quantifies the recent trend development \insertCite{domagk2022}{PQDM}.
#'
#' Steps:
#' 1. STL \insertCite{cleveland1990}{PQDM} decomposes each series into
#'    trend, seasonal and remainder component. STL requires more than 104
#'    weeks; series of 53 to 104 weeks are extended by repeating their first
#'    52 weeks in front of the series if `extend` is `TRUE`.
#' 2. Trend and seasonal strength \insertCite{wang2006}{PQDM}.
#' 3. Recent gain: duration and change of the trend since its last turning
#'    point, and the gain per year.
#'
#' Only time series that [pq_validate()] marks as applicable for the trend
#' are analysed. Missing weeks are filled by [pq_fill()] if not done yet.
#'
#' @param data A `pqdm_validation` object or a data frame in long format,
#'   which is passed to [pq_validate()].
#' @param extend Whether series of 53 to 104 weeks are extended artificially;
#'   otherwise they yield `NA`.
#' @param ... Passed to [pq_validate()] if `data` is a data frame.
#'
#' @return An object of class `pqdm_trend`, a list with
#'   \describe{
#'     \item{series}{`validation$series` with `extended`, `trend_strength`,
#'       `seasonal_strength` (both between 0 and 1), `gain_duration` (weeks
#'       since the last turning point of the trend), `gain` (change of the
#'       trend in these weeks, in the unit of `value`) and `gain_per_year`.
#'       `NA` for series that are not analysed.}
#'     \item{components}{The filled `value` and the components `trend`,
#'       `seasonal` and `remainder` per week.}
#'     \item{settings}{The settings used.}
#'     \item{validation}{The `pqdm_validation` object used.}
#'   }
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' tr <- pq_weekly_example |> pq_validate() |> pq_fill() |> pq_trend()
#' tr
#' summary(tr)
#'
#' @export
pq_trend <- function(data, extend = TRUE, ...) {
  check_flag(extend)

  validation <- pq_fill(data, ...)
  # applicable series without the missing weeks at start and end; without
  # extension, STL needs more than 104 weeks
  weekly <- validation$data |>
    semi_join(filter(validation$series, trend), by = series_keys) |>
    filter(!is.na(value)) |>
    filter(.env$extend | n() > 104, .by = all_of(series_keys))
  if (nrow(weekly) == 0) stop("No time series can be analysed.", call. = FALSE)

  components <- weekly |>
    reframe(apply_stl(pick(week, value), artificially_extend = extend),
            .by = all_of(series_keys))
  per_series <- components |>
    reframe(get_trend_features(pick(everything())),
            .by = all_of(series_keys)) |>
    mutate(extended = .env$extend & duration <= 104,
           .after = all_of(series_keys)) |>
    select(!duration)

  structure(
    list(
      series = left_join(validation$series, per_series, by = series_keys),
      components = components,
      settings = list(extend = extend),
      validation = validation
    ),
    class = "pqdm_trend"
  )
}

# Decompose a time series into trend, seasonal and remainder component (STL)
#
# `tidy_data` has the column `value`, sorted and without gaps.
apply_stl <- function(tidy_data,
                      period = 52,
                      s.window = 53,
                      artificially_extend = FALSE) {
  nextodd <- function(x) {
    x <- round(x)
    if (x %% 2 == 0) x <- x + 1
    as.integer(x)
  }

  n_values <- nrow(tidy_data)
  if (artificially_extend && n_values > period && n_values <= 2 * period) {
    # repeat only the earliest full year
    # quarters:          (--|ab)(cdef)
    # extend to: (--ab)(cd|ab)(cdef)
    tidy_data <- rbind(tidy_data[1:period, ], tidy_data)
  }

  t.window <- nextodd(ceiling(1.5 * period / (1 - 1.5 / s.window)))
  series <- stats::ts(tidy_data$value, frequency = period, start = 0)
  decomposition <- stats::stl(series, t.window = t.window, s.window = s.window)

  tidy_data |>
    mutate(
      trend = as.numeric(decomposition$time.series[, "trend"]),
      seasonal = as.numeric(decomposition$time.series[, "seasonal"]),
      remainder = as.numeric(decomposition$time.series[, "remainder"])
    ) |>
    # cut the extension off again
    utils::tail(n = n_values)
}

# Trend and seasonal strength and recent trend gain of one time series
get_trend_features <- function(data, period = 52) {
  calc_strength <- function(component, remainder) {
    max(0, 1 - stats::var(remainder) / stats::var(component + remainder))
  }

  data |>
    summarise(
      duration = n(),
      trend_strength = calc_strength(trend, remainder),
      seasonal_strength = calc_strength(seasonal, remainder),
      get_recent_trend_gain(trend),
      gain_per_year = gain / gain_duration * period
    )
}

# Duration and change of the most recent increase or decrease of `x`
#
# Counted backwards from the end until the direction changes (turning point)
# or the beginning is reached; flat steps do not end the gain.
get_recent_trend_gain <- function(x) {
  # differences backwards from the end: < 0 is an increase, > 0 a decrease
  x_diff <- diff(rev(x))
  direction <- sign(x_diff)
  first_direction <- direction[direction != 0][1]

  gain_duration <- 0L
  for (step in direction) {
    if (!step %in% c(first_direction, 0)) break
    gain_duration <- gain_duration + 1L
  }

  tibble(
    gain_duration = gain_duration,
    gain = -sum(x_diff[seq_len(gain_duration)])
  )
}

#' @export
print.pqdm_trend <- function(x, ...) {
  series <- x$series
  cat("<pqdm_trend> trends of weekly 95th percentiles\n")
  cat(sprintf(paste("  %d time series, %d analysed (%d extended),",
                    "%d rising, %d falling\n"),
              nrow(series), sum(!is.na(series$trend_strength)),
              sum(series$extended, na.rm = TRUE),
              sum(series$gain > 0, na.rm = TRUE),
              sum(series$gain < 0, na.rm = TRUE)))
  invisible(x)
}

#' @export
summary.pqdm_trend <- function(object, ...) {
  object$series |>
    filter(!is.na(trend_strength)) |>
    select(site, voltage, phase, param, extended, trend_strength,
           seasonal_strength, gain_duration, gain_per_year)
}
