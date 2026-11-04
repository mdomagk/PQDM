# PQDM <img src="man/figures/logo.png" align="right" height="139" alt="PQDM logo" />

**Power Quality Data Mining** – an R package for analysing power quality
measurements.

PQDM implements published methods for the evaluation of long-term power
quality measurements. The analyses are based on 10-minute values and on
weekly 95th percentiles derived from them. Each function refers to the
publication that describes its method.

## Installation

The development version can be installed from GitHub:

```r
# install.packages("remotes")
remotes::install_github("mdomagk/PQDM", build_vignettes = TRUE)
```

## Data format

The functions take weekly 95th percentiles in long format with the columns
`site`, `voltage`, `week` (the Monday of the week, class `Date`), `phase`,
`param` and `value`; `NA` marks a missing measurement. A time series is one
combination of `site`, `voltage`, `phase` and `param`.

## Usage

The data are validated, gaps are filled where needed, and the result is passed
to a method:

```r
library(PQDM)

pq_weekly_example |>
  pq_validate() |>   # data availability per time series
  pq_fill() |>       # fill gaps by linear interpolation
  pq_season()        # seasonal variations
```

`pq_weekly_example` contains synthetic weekly 95th percentiles of H3, H5 and
Plt of 24 fictitious medium-voltage sites from 2022 to 2025; each
measurement follows one typical course. The
vignette `vignette("PQDM")` explains the workflow.

## Functions

### Preprocessing

- `pq_validate()` – check weekly 95th percentiles and their data
  availability
  ([Domagk et al. 2022](https://doi.org/10.1109/ICHQP53011.2022.9808645),
  [2023](https://doi.org/10.1049/icp.2023.1082))
- `pq_fill()` – fill gaps within time series by linear interpolation
  ([Domagk et al. 2022](https://doi.org/10.1109/ICHQP53011.2022.9808645))

### Methods

- `pq_quality_reserve()` – quality reserve with respect to user-supplied
  limits, indices via `summary()`
  ([Gasch et al. 2016](https://doi.org/10.1109/ICHQP.2016.7783325);
  [Abdelrahman et al. 2016](https://doi.org/10.1109/ICHQP.2016.7783326))
- `pq_season()` – identification and quantification of seasonal variations
  ([Domagk et al. 2015](https://doi.org/10.1109/PTC.2015.7232396))
- `pq_trend()` – trend components and recent trend gains
  ([Domagk et al. 2022](https://doi.org/10.1109/ICHQP53011.2022.9808645))

The help page of each function, e.g. `?pq_season`, lists its steps and all
references, including those of the algorithms; formulas and derivations are
given in the cited papers.

## Vignettes

- `vignette("PQDM")` – data format, validation, filling gaps and the
  methods

## Status

PQDM is under development; see `NEWS.md` for the changes per version.

## Citation

If you use PQDM, please cite the publication of the method you applied, as
listed above. The package itself can be cited
via:

```r
citation("PQDM")
```

## Development

The methods and their original implementations are the author's own work,
published in the papers cited for each function. The package was built
around them with the assistance of Claude (Anthropic); results were checked
against the original scripts, and all code is reviewed by the author.

## License

GPL (>= 3) © Max Domagk
