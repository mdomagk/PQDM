#' Fill gaps in weekly 95th percentiles
#'
#' Fills missing weeks within each time series by linear interpolation;
#' missing weeks at the start or end stay `NA`. Whether a series is suitable
#' for a method is decided by [pq_validate()]; gaps of up to 10 weeks have a
#' negligible impact on the trend \insertCite{domagk2022}{PQDM}.
#' [pq_quality_reserve()] ignores filled weeks.
#'
#' @param data A `pqdm_validation` object or a data frame in long format,
#'   which is passed to [pq_validate()].
#' @param ... Passed to [pq_validate()] if `data` is a data frame.
#'
#' @return The `pqdm_validation` object with filled values, the logical
#'   column `data$filled` and the number of filled weeks `n_filled`.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' v <- pq_validate(pq_weekly_example) |> pq_fill()
#' v
#' v$data[v$data$filled, ]
#'
#' @export
pq_fill <- function(data, ...) {
  validation <- data
  if (!inherits(data, "pqdm_validation")) validation <- pq_validate(data, ...)
  if (is_filled(validation)) return(validation)

  validation$data <- validation$data |>
    mutate(
      filled = is.na(value),
      value = fill_gaps(value),
      filled = filled & !is.na(value),
      .by = all_of(series_keys)
    )
  validation$n_filled <- sum(validation$data$filled)
  validation
}

# Whether the gaps of a `pqdm_validation` object have been filled
is_filled <- function(x) {
  !is.null(x$n_filled)
}

# Linear interpolation of the gaps of one time series (sorted by week on a
# complete grid); missing values at the start or the end stay NA
fill_gaps <- function(value) {
  if (sum(!is.na(value)) < 2) return(value)
  position <- seq_along(value)
  stats::approx(position, value, xout = position)$y
}
