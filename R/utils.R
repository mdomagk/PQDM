# Columns that identify a time series
series_keys <- c("site", "voltage", "phase", "param")

# Check that an argument is a single number within [min, max]
check_number <- function(x, min = -Inf, max = Inf, null_ok = FALSE) {
  if (null_ok && is.null(x)) return(invisible(x))
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x > max) {
    stop(sprintf("`%s` must be %sa single number in [%s, %s].",
                 deparse(substitute(x)), if (null_ok) "NULL or " else "",
                 min, max), call. = FALSE)
  }
  invisible(x)
}

# Check that an argument is TRUE or FALSE
check_flag <- function(x) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", deparse(substitute(x))),
         call. = FALSE)
  }
  invisible(x)
}

# Evaluate `expr` with the random seed `seed` and restore the previous state
# of the random number generator afterwards; `NULL` leaves it as it is
with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  old <- globalenv()$.Random.seed
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv())
          else assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  expr
}
