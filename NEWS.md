# PQDM 0.5.1

First public release.

## Preprocessing

* `pq_validate()` checks weekly 95th percentiles and marks for each method
  whether the data availability of a time series suffices;
  `zero_is_missing = FALSE` keeps values of exactly 0, e.g. for counts.
* `pq_fill()` fills gaps within time series by linear interpolation.

## Methods

* `pq_quality_reserve()` computes the quality reserve and the utilization
  with respect to user-supplied limits; `summary(x, by = ...)` aggregates
  them into indices.
* `pq_season()` identifies and quantifies seasonal variations.
* `pq_trend()` computes trend components and recent trend gains.

## Data

* `pq_weekly_example`: synthetic weekly 95th percentiles of H3, H5 and Plt
  at 24 medium-voltage sites over four years, each measurement following
  one type of course.
