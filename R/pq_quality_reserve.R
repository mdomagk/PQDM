#' Quality reserve with respect to user-supplied limits
#'
#' Calculates the distance of weekly 95th percentiles to a limit, normalised
#' on the limit \insertCite{gasch2015;textual}{PQDM}:
#' `reserve = (1 - value / limit) * 100` and `util = 100 - reserve`, both in
#' %. A reserve at or below 0 % is a violation of the limit.
#'
#' Limits are not shipped with the package; only one-sided upper limits are
#' supported. Weeks without a value, weeks filled by [pq_fill()], parameters
#' without a limit and time series that [pq_validate()] marks as not
#' applicable yield `NA`.
#'
#' `summary()` aggregates the weekly reserves into indices of any level
#' \insertCite{gasch2016,abdelrahman2016}{PQDM}: the columns in `by` are
#' kept, all others are aggregated by the minimum or the `probs` quantile of
#' the reserves (`util` then is the maximum or upper quantile). Besides the
#' columns of the data, `by` accepts `year`, `quarter` (e.g. `"2024-Q1"`)
#' and `month` (e.g. `"2024-01"`) of the week and the columns of `groups`.
#' Groups without a reserve yield `NA`.
#'
#' @param data A `pqdm_validation` object or a data frame in long format,
#'   which is passed to [pq_validate()].
#' @param limits A data frame with the columns `param`, `limit` and
#'   optionally `voltage`. Without `voltage` a limit applies to all voltage
#'   levels.
#' @param limit_set Optional name of the set of limits, for labelling only.
#' @param ... Passed to [pq_validate()] if `data` is a data frame; unused
#'   in `summary()`.
#' @param object A `pqdm_quality_reserve` object.
#' @param by Columns to keep, by default one row per series; `NULL`
#'   aggregates everything into one index.
#' @param method `"min"` or `"quantile"`.
#' @param probs Probability of the quantile for `method = "quantile"`.
#' @param groups Optional data frame with further groupings, e.g. sites and
#'   their network region, matched by the columns it shares with the data.
#'
#' @return An object of class `pqdm_quality_reserve`, a list with
#'   \describe{
#'     \item{data}{The validated data with `limit`, `reserve`, `util` and
#'       `category` (`high` > 50 % >= `medium` > 25 % >= `low` > 0 % >=
#'       `violation`).}
#'     \item{series}{`validation$series` with `limit`, `n_reserve` (weeks
#'       with a reserve), `min_reserve`, `max_util`, `category` of the
#'       minimum reserve and `n_violation` (weeks with a violation).}
#'     \item{limits, limit_set}{The limits used and their name.}
#'     \item{validation}{The `pqdm_validation` object used.}
#'   }
#'
#'   `summary()` returns a tibble with one row per group: the columns of
#'   `by`, `limit` (`NA` if it differs within the group), `reserve`, `util`,
#'   `category`, `n` (number of reserves) and `n_violation`.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # Freely chosen limits for the synthetic example data, not from a standard
#' limits <- data.frame(param = c("H3", "H5", "Plt"), limit = c(1.5, 4, 1))
#' qr <- pq_quality_reserve(pq_weekly_example, limits)
#' qr
#' summary(qr)
#' summary(qr, by = c("site", "year"))
#' summary(qr, by = "month", method = "quantile")
#'
#' regions <- data.frame(site = sprintf("S%02d", 1:24),
#'                       region = rep(c("north", "south"), each = 12))
#' summary(qr, by = "region", groups = regions)
#'
#' @export
pq_quality_reserve <- function(data, limits, limit_set = NULL, ...) {
  limit_keys <- intersect(c("param", "voltage"), names(limits))
  check_limits(limits, limit_keys)
  limits <- as_tibble(limits)[c(limit_keys, "limit")]
  validation <- data
  if (!inherits(data, "pqdm_validation")) validation <- pq_validate(data, ...)

  weekly <- weekly_reserve(validation, limits, limit_keys)
  per_series <- weekly |>
    filter(!is.na(reserve)) |>
    summarise(
      n_reserve = n(),
      min_reserve = min(reserve),
      max_util = max(util),
      n_violation = sum(reserve <= 0),
      .by = all_of(series_keys)
    )
  series <- validation$series |>
    left_join(limits, by = limit_keys) |>
    left_join(per_series, by = series_keys) |>
    mutate(
      n_reserve = coalesce(n_reserve, 0L),
      n_violation = coalesce(n_violation, 0L),
      category = cut_quality_reserve(min_reserve),
      .before = n_violation
    )

  structure(
    list(
      data = weekly,
      series = series,
      limits = limits,
      limit_set = limit_set,
      validation = validation
    ),
    class = "pqdm_quality_reserve"
  )
}

# Check that limits has one finite positive limit per param (and voltage)
check_limits <- function(limits, limit_keys) {
  if (!is.data.frame(limits) || !all(c("param", "limit") %in% names(limits)) ||
      !is.numeric(limits$limit) || !all(is.finite(limits$limit)) ||
      any(limits$limit <= 0) || anyNA(limits[limit_keys]) ||
      anyDuplicated(limits[limit_keys])) {
    stop("`limits` must be a data frame with the columns 'param', 'limit' ",
         "and optionally 'voltage', one row per 'param' (and 'voltage') ",
         "and finite positive limits.", call. = FALSE)
  }
}

# Reserve, utilization and category per week; NA for weeks that were filled
# and for series not applicable to the quality reserve
weekly_reserve <- function(validation, limits, limit_keys) {
  applicable <- validation$series |>
    select(all_of(series_keys), applicable = "quality_reserve")
  weekly <- validation$data
  if (!is_filled(validation)) weekly$filled <- FALSE

  weekly |>
    left_join(limits, by = limit_keys) |>
    left_join(applicable, by = series_keys) |>
    mutate(
      reserve = if_else(applicable & !filled, (1 - value / limit) * 100,
                        NA_real_),
      util = 100 - reserve,
      category = cut_quality_reserve(reserve)
    ) |>
    select(!applicable)
}

# Categories of the quality reserve, ordered from high reserve to violation
cut_quality_reserve <- function(value) {
  value_cut <- cut(value, c(-Inf, 0, 25, 50, 100),
                   labels = c("violation", "low", "medium", "high"))
  value_levels <- levels(value_cut)
  value_cut <- factor(value_cut, levels = rev(value_levels))
  return(value_cut)
}

#' @export
print.pqdm_quality_reserve <- function(x, ...) {
  series <- x$series
  cat("<pqdm_quality_reserve> quality reserve in %",
      if (!is.null(x$limit_set)) paste0("(", x$limit_set, ")"), "\n")
  cat(sprintf("  %d time series, %d with a reserve, %d with a violation\n",
              nrow(series), sum(series$n_reserve > 0),
              sum(series$n_violation > 0)))
  cat("\n  Weeks per category:\n")
  counts <- table(x$data$category)
  cat(sprintf("    %-10s %6d\n", names(counts), counts), sep = "")
  invisible(x)
}

#' @rdname pq_quality_reserve
#' @export
summary.pqdm_quality_reserve <- function(object,
                                         by = c("site", "voltage", "phase",
                                                "param"),
                                         method = c("min", "quantile"),
                                         probs = 0.05, groups = NULL, ...) {
  method <- match.arg(method)
  check_number(probs, min = 0, max = 1)

  object$data |>
    join_groups(groups) |>
    mutate(year = format(week, "%Y"),
           quarter = paste0(year, "-", quarters(week)),
           month = format(week, "%Y-%m")) |>
    group_by(across(all_of(by))) |>
    summarise(
      limit = if (length(unique(limit)) == 1) limit[1] else NA_real_,
      n = sum(!is.na(reserve)),
      n_violation = sum(reserve <= 0, na.rm = TRUE),
      reserve = aggregate_reserve(reserve, .env$method, .env$probs),
      .groups = "drop"
    ) |>
    mutate(util = 100 - reserve, category = cut_quality_reserve(reserve)) |>
    select(all_of(by), limit, reserve, util, category, n, n_violation)
}

# Join the further groupings by the columns they share with the data
join_groups <- function(data, groups) {
  if (is.null(groups)) return(data)
  join <- intersect(names(groups), names(data))
  if (!is.data.frame(groups) || length(join) == 0 ||
      anyDuplicated(groups[join])) {
    stop("`groups` must be a data frame with one row per combination of ",
         "the columns it shares with the data.", call. = FALSE)
  }
  left_join(data, groups, by = join)
}

# Minimum or lower quantile of the reserves of a group, NA without any
aggregate_reserve <- function(reserve, method, probs) {
  if (all(is.na(reserve))) return(NA_real_)
  if (method == "min") return(min(reserve, na.rm = TRUE))
  unname(stats::quantile(reserve, probs, na.rm = TRUE))
}
