# Synthetic weekly 95th percentiles of H3, H5 and Plt at 24 MV sites from
# 2022 to 2025. Each measurement (site and parameter) follows one type of
# course. Each effect is drawn in a block of its own, one row per
# measurement; simulate() puts them together into the series of the phases.

library(dplyr)
set.seed(42)

# time axis: four years of weeks, starting on Monday
n_weeks <- 208
weeks <- seq(as.Date("2022-01-03"), by = "week", length.out = n_weeks)
years <- as.numeric(weeks - weeks[1]) / 365.25  # time since the start
year <- floor(years) + 1                        # 1 to 4
n_years <- max(year)
woy <- as.numeric(format(weeks, "%V"))          # ISO calendar week

# sites: alternately 10 kV and 20 kV
n_sites <- 24
sites <- tibble(site = sprintf("S%02d", seq_len(n_sites)),
                voltage = rep(c("10 kV", "20 kV"), length.out = n_sites))

# types of course: number of sites per parameter and type
assignment <- tribble(
  ~param, ~type,           ~n_sites,
  "H3",   "flat",          15,
  "H3",   "rising",        5,
  "H3",   "falling",       4,
  "H5",   "flat",          9,
  "H5",   "summer",        7,
  "H5",   "summer_rising", 3,
  "H5",   "winter",        3,
  "H5",   "rising",        2,
  "Plt",  "spiky",         15,
  "Plt",  "flat",          5,
  "Plt",  "summer",        4
)
stopifnot(all(tapply(assignment$n_sites, assignment$param, sum) == n_sites))

# measurements: every site with every parameter, types shuffled over the sites
measurements <-
  assignment |>
  tidyr::uncount(n_sites) |>
  mutate(site = sample(sites$site), .by = param) |>
  left_join(sites, by = "site") |>
  arrange(site, param) |>
  select(site, voltage, param, type)
n_measurements <- nrow(measurements)

# level: base level per parameter, +/-20 % per measurement
base_level <- c(H3 = 0.7, H5 = 1.7, Plt = 0.35)
level <- unname(base_level[measurements$param]) * runif(n_measurements, 0.8, 1.2)

# trend: change per year relative to the level by type, +/-20 % per measurement
trend_per_year <- c(rising = 0.15, summer_rising = 0.12, falling = -0.15)
gain <- coalesce(unname(trend_per_year[measurements$type]), 0) *
  runif(n_measurements, 0.8, 1.2)
trend <- outer(1 + gain, years, `^`)

# season: annual cycle relative to the level by type, +/-20 % per
# measurement; amplitude (+/-20 %) and peak week (+/-2 weeks) also vary
# from year to year
season_amplitude <- c(flat = 0.03, rising = 0.03, falling = 0.03, spiky = 0,
                      summer = 0.25, summer_rising = 0.25, winter = 0.25)
season_peak <- c(winter = 2)  # all other types peak in week 27
amplitude <- unname(season_amplitude[measurements$type]) *
  runif(n_measurements, 0.8, 1.2)
peak_week <- coalesce(unname(season_peak[measurements$type]), 27)
yearly_scale <- matrix(rnorm(n_measurements * n_years, 1, 0.2), n_measurements)[, year]
yearly_shift <- matrix(rnorm(n_measurements * n_years, 0, 2), n_measurements)[, year]
weeks_from_peak <- outer(peak_week, woy, \(peak, week) week - peak) - yearly_shift
season <- 1 + amplitude * yearly_scale * cos(2 * pi * weeks_from_peak / 52.18)

# noise: multiplicative, log-sd 5 to 10 % per measurement, lag-1
# autocorrelation 0.4
ar <- 0.4
noise_sd <- runif(n_measurements, 0.05, 0.10)
noise <- t(sapply(noise_sd, \(sd) {
  exp(as.numeric(arima.sim(list(ar = ar), n_weeks, sd = sd * sqrt(1 - ar^2))))
}))

# events: spiky measurements only, 10 weeks in four years raised by a factor
# of 1.5 to 3
n_events <- 10
events <- matrix(1, n_measurements, n_weeks)
for (i in which(measurements$type == "spiky")) {
  events[i, sample(n_weeks, n_events)] <- runif(n_events, 1.5, 3)
}

# phases: static unbalance of +/-10 % and noise of 3 % of their own
phases <- c("L1", "L2", "L3")
unbalance <- matrix(runif(n_measurements * 3, 0.9, 1.1), n_measurements)
phase_noise <- array(exp(rnorm(n_measurements * n_weeks * 3, sd = 0.03)),
                     c(n_measurements, n_weeks, 3))

# outages: one to seven weeks at half of the sites, all parameters and
# phases; inside the period (from week 2), so that pq_fill() fills them
outages <- tibble(site = sample(sites$site, n_sites / 2),
                  start = sample(2:200, n_sites / 2, replace = TRUE),
                  duration = sample(1:7, n_sites / 2, replace = TRUE)) |>
  reframe(week = weeks[start + seq_len(duration) - 1], outage = TRUE, .by = site)

# course of measurement i, the same in all phases, times the unbalance and
# noise of each phase
simulate <- function(i) {
  course <- level[i] * trend[i, ] * season[i, ] * noise[i, ] * events[i, ]
  tibble(measurements[i, ],
         phase = rep(phases, each = n_weeks),
         week = rep(weeks, 3),
         value = rep(course, 3) * rep(unbalance[i, ], each = n_weeks) *
           as.vector(phase_noise[i, , ]))
}

pq_weekly_example <- bind_rows(lapply(seq_len(n_measurements), simulate)) |>
  left_join(outages, by = c("site", "week")) |>
  mutate(value = if_else(is.na(outage), round(value, 2), NA)) |>
  select(site, voltage, week, phase, param, value)
usethis::use_data(pq_weekly_example, overwrite = TRUE)
