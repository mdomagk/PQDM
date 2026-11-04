#' Seasonal variations of weekly 95th percentiles
#'
#' Identifies and quantifies seasonal variations (period 52 weeks) in time
#' series of weekly 95th percentiles.
#'
#' Steps:
#' 1. Every 52-week window of a series is transformed by a discrete Fourier
#'    transform; the variation index relates the annual component to the
#'    other components \insertCite{domagk2015}{PQDM}.
#' 2. The windows are aggregated into the prevailing phasor
#'    \insertCite{meyer2017}{PQDM}; the prevailing ratio assesses its
#'    representativeness \insertCite{blanco2016a}{PQDM}.
#' 3. The thresholds mark a series as `seasonal`, `prevalent` and
#'    `significant`.
#'
#' Only time series that [pq_validate()] marks as applicable for the season
#' are analysed. Missing weeks are filled by [pq_fill()] if not done yet.
#'
#' @param data A `pqdm_validation` object or a data frame in long format,
#'   which is passed to [pq_validate()].
#' @param min_var_index A series is `seasonal` if its mean variation index is
#'   above this threshold.
#' @param min_prevalence A series is `prevalent` if its prevailing ratio is at
#'   or above this threshold.
#' @param min_amplitude Optional threshold of the amplitude in the unit of
#'   `value`; `NULL` applies none.
#' @param ... Passed to [pq_validate()] if `data` is a data frame.
#'
#' @return An object of class `pqdm_season`, a list with
#'   \describe{
#'     \item{series}{`validation$series` with `n_windows`, `var_index` (mean
#'       variation index), `amplitude` (root mean square amplitude), `delta`
#'       (variation range, twice the amplitude), `angle` (phase angle of the
#'       maximum in degrees, 0 = calendar week 1), `peak_week` (calendar week
#'       of the maximum), `prevalence` (prevailing ratio) and the logical
#'       columns `seasonal`, `prevalent` and `significant` (all thresholds
#'       met). `NA` for series that are not applicable.}
#'     \item{windows}{`var_index`, `amplitude`, `angle` and `peak_week` per
#'       52-week window; `window_start` is its first week.}
#'     \item{settings}{The thresholds used.}
#'     \item{validation}{The `pqdm_validation` object used.}
#'   }
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' s <- pq_weekly_example |> pq_validate() |> pq_fill() |> pq_season()
#' s
#' summary(s)
#'
#' @export
pq_season <- function(data, min_var_index = 1, min_prevalence = 0.9,
                      min_amplitude = NULL, ...) {
  check_number(min_var_index, min = 0)
  check_number(min_prevalence, min = 0, max = 1)
  check_number(min_amplitude, min = 0, null_ok = TRUE)

  validation <- pq_fill(data, ...)
  # applicable series without the missing weeks at start and end
  weekly <- validation$data |>
    semi_join(filter(validation$series, season), by = series_keys) |>
    filter(!is.na(value))
  if (nrow(weekly) == 0) stop("No time series can be analysed.", call. = FALSE)

  windows <- weekly |>
    reframe(calculate_seasonal_variation(pick(week, value)),
            .by = all_of(series_keys))
  per_series <- windows |>
    reframe(average_seasonal_variation(pick(everything())),
            .by = all_of(series_keys)) |>
    mutate(
      seasonal = var_index > .env$min_var_index,
      prevalent = prevalence >= .env$min_prevalence,
      significant = seasonal & prevalent &
        amplitude > (.env$min_amplitude %||% -Inf)
    )

  structure(
    list(
      series = left_join(validation$series, per_series, by = series_keys),
      windows = windows,
      settings = list(min_var_index = min_var_index,
                      min_prevalence = min_prevalence,
                      min_amplitude = min_amplitude),
      validation = validation
    ),
    class = "pqdm_season"
  )
}

# Seasonal variation of one time series
#
# `data` has the columns `week` and `value`, sorted by week and without gaps.
# One row per 52-week window; the windows are averaged by
# average_seasonal_variation().
#
# Harmonic k of a window of N weeks is abs * cos(2 * pi * k * t / N + arg),
# t = 0, ..., N - 1, with abs = 2 / N * |F_k| and the phase angle arg of F_k
# in degrees (F: discrete Fourier transform).
#
# Deviation from the script: instead of apply_dft() with calculate_dft() per
# window, all windows are the columns of one matrix and transformed by one
# mvfft(), with the same amplitude and angle. Same results, about 40 times
# faster: the time went into one table per window.
calculate_seasonal_variation <- function(data, period = 52) {
  # one 52-week window per column, starting at each week
  positions <- seq(1, nrow(data) - period + 1)
  windows <- matrix(data$value[outer(seq_len(period) - 1, positions, "+")],
                    nrow = period)

  # harmonics 1 to period / 2 - 1 (row 1 of the FFT is the mean)
  harmonics <- stats::mvfft(windows)[seq(2, period / 2), , drop = FALSE]
  amplitudes <- (2 / period) * Mod(harmonics)
  power <- amplitudes^2
  window_start <- data$week[positions]

  tibble(
    window_start = window_start,
    var_index = sqrt(power[1, ] / colSums(power[-1, , drop = FALSE])),
    amplitude = amplitudes[1, ],
    angle = reference_phase_angle((180 / pi) * Arg(harmonics[1, ]),
                                  window_start, period),
    peak_week = angle_to_week(angle, period)
  )
}

# Prevailing phasor of all windows of one time series
average_seasonal_variation <- function(windows, period = 52) {
  phasors <- complex(modulus = windows$amplitude,
                     argument = windows$angle / 180 * pi)
  tibble(
    n_windows = nrow(windows),
    var_index = mean(windows$var_index),
    amplitude = sqrt(mean(windows$amplitude^2)),
    delta = 2 * amplitude,
    angle = (Arg(sum(phasors)) / pi * 180) %% 360,
    peak_week = angle_to_week(angle, period),
    prevalence = abs(sum(phasors)) / sum(abs(phasors))
  )
}

# Phase angle of the maximum in degrees, referenced to calendar week 1
#
# The component abs * cos(w * t + arg) has its maximum at w * t = -arg,
# counted from the first week of the window. Adding the ISO calendar week of
# that week refers the angle to calendar week 1 (0 degrees).
reference_phase_angle <- function(arg, window_start, period = 52) {
  year_week <- as.numeric(format(as.Date(window_start), "%V"))
  (-arg + (year_week - 1) * 360 / period) %% 360
}

# Calendar week of a phase angle: 0 degrees is week 1, 180 degrees week 27
angle_to_week <- function(angle, period = 52) {
  round(angle / 360 * period) %% period + 1
}

#' @export
print.pqdm_season <- function(x, ...) {
  series <- x$series
  cat("<pqdm_season> seasonal variations of weekly 95th percentiles\n")
  cat(sprintf("  %d time series, %d analysed, %d significant\n",
              nrow(series), sum(!is.na(series$var_index)),
              sum(series$significant, na.rm = TRUE)))
  invisible(x)
}

#' @export
summary.pqdm_season <- function(object, ...) {
  object$series |>
    filter(significant) |>
    select(site, voltage, phase, param, var_index, delta, peak_week,
           prevalence)
}
