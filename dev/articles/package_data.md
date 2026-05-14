# GeoTox Package Data

The GeoTox package includes example data `geo_tox_data`. This article
provides a description of the data and example code for how it was
gathered.

> **Note**
>
> The websites linked in this vignette were last accessed on March 20,
> 2026. The latest R package versions were used at that time.

> **Warning**
>
> The package data are being pulled from various sources and are
> connected using FIPS codes. FIPS codes are a simple way to connect
> data, but they can change. For example,
> [Connecticut](https://www.bls.gov/cew/classifications/areas/new-2024-connecticut-counties.htm)
> began the process of going from 8 legacy counties to 9 planning
> regions starting in 2022 and became effective in 2024.

## Setup

Load packages and create an empty list to store data.

``` r

library(dplyr)
library(httk)
library(httr2)
library(purrr)
library(readr)
library(readxl)
library(sf)
library(stringr)
library(tibble)
library(tidyr)

geo_tox_data <- list()
```

## Chemicals

### External exposure

Download modeled exposure data from
[AirToxScreen](https://www.epa.gov/AirToxScreen). Results from 2020 for
a subset of chemicals in North Carolina counties are included in the
GeoTox package data as `geo_tox_data$exposure`.

> **Note**
>
> The
> [2020](https://www.epa.gov/AirToxScreen/2020-airtoxscreen-assessment-results)
> version has census block level data, which is provided in several
> large files separated by region. The
> [2019](https://www.epa.gov/AirToxScreen/2019-airtoxscreen-assessment-results)
> version has census tract level data, which is provided as a single
> file and is much smaller to download.

``` r

filename <- "Region4b_2020ATS_Exposure_Concentrations.xlsx"
tmp <- tempfile(filename)
download.file(
  paste0(
    "https://gaftp.epa.gov/rtrmodeling_public/AirToxScreen/2020/",
    "Exposure%20Concentrations/",
    filename
  ),
  tmp
)
exposure <- read_xlsx(tmp)

# Normalization function
min_max_norm = function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if (min_x == max_x) {
    rep(0, length(x))
  } else {
    (x - min_x) / (max_x - min_x)
  }
}

geo_tox_data$exposure <- exposure |>
  # North Carolina counties
  filter(State == "NC", !grepl("0$", FIPS)) |>
  # Aggregate chemicals by county
  summarize(
    across(
      -c(State:Population), # Note: use "-c(State:Tract)" for 2019 data
      list(mean = mean, sd = sd),
      .names = "{col}|||{fn}"
    ),
    .by = FIPS
  ) |>
  pivot_longer(-FIPS) |>
  separate_wider_delim(name, "|||", names = c("chemical", "stat")) |>
  pivot_wider(names_from = stat) |>
  # Normalize concentrations
  mutate(norm = min_max_norm(mean), .by = chemical)
```

#### Get chemical identifiers from CompTox Dashboard

The exposure data is labeled with chemical names, but names can vary
across resources and a more reliable way to connect datasets is by using
Chemical Abstracts Service Registry Number (CAS-RN) identifiers. Use the
batch search feature of the [CompTox
Dashboard](https://comptox.epa.gov/dashboard/batch-search) to retrieve
CAS-RN and PREFERRED_NAME for the chemicals in the exposure dataset.

``` r

# Copy/paste the chemical names into the batch search field
# https://comptox.epa.gov/dashboard/batch-search
cat(geo_tox_data$exposure |> distinct(chemical) |> pull(), sep = "\n")
# Export results with "CAS-RN" identifiers as a csv file, then process in R

# Remove rows without clear chemical matches, investigate manually if desired
exposure_casrn <- read_csv("CCD-Batch-Search.csv") |>
  filter(DTXSID != "N/A", !grepl("WARNING", FOUND_BY))

# Update exposure data with CompTox Dashboard data
geo_tox_data$exposure <- geo_tox_data$exposure |>
  inner_join(exposure_casrn, by = join_by(chemical == INPUT)) |>
  select(FIPS, casn = CASRN, chnm = PREFERRED_NAME, mean, sd, norm)
```

### Active chemicals

Use the Integrated Chemical Environment
([ICE](https://ice.ntp.niehs.nih.gov/)) curated high-throughput
screening
([cHTS](https://ice.ntp.niehs.nih.gov/DATASETDESCRIPTION?section=cHTS))
data to identify active chemicals for a given set of assays.

> **Note**
>
> The following can be used to get a list of available assays for a
> given chemical set.
>
> ``` r
>
> # Get all supported assays
> help_text <- request("https://ice.ntp.niehs.nih.gov/api/v1/search/help") |>
>   req_perform() |>
>   resp_body_string()
> supported_assays <- str_split_i(help_text, "Supported assay\\(s\\):", 2) |>
>   str_split_1("\",\"") |>
>   str_replace_all(c("\"" = "")) |>
>   str_trim()
>
> # Search for assays available for given chemids
> chemids <- unique(geo_tox_data$exposure$casn)
> resp <- request("https://ice.ntp.niehs.nih.gov/api/v1/search") |>
>   req_body_json(list(chemids = chemids, assays = supported_assays)) |>
>   req_perform()
> result <- resp |> resp_body_json() |> pluck("endPoints")
> fields <- c("assay", "casrn", "dtxsid", "substanceName", "endpoint", "value")
> df <- map(fields, \(x) map_chr(result, x)) |>
>   set_names(fields) |>
>   bind_cols()
> available_assays <- df |> distinct(assay) |> pull() |> sort()
> ```

``` r

get_cHTS_hits <- function(assays = NULL, chemids = NULL) {
  if (is.null(assays) & is.null(chemids)) {
    stop("Must provide at least one of 'assays' or 'chemids'")
  }

  # Format query parameters
  req_params <- list()

  if (!is.null(assays)) {
    if (!is.list(assays)) assays <- as.list(assays)
    req_params$assays <- assays
  }

  if (!is.null(chemids)) {
    if (!is.list(chemids)) chemids <- as.list(chemids)
    req_params$chemids <- chemids
  }

  # Query ICE API
  resp <- request("https://ice.ntp.niehs.nih.gov/api/v1/search") |>
    req_body_json(req_params) |>
    req_perform()

  if (resp$status_code != 200) {
    stop("Failed to retrieve data from ICE API")
  }

  # Return active chemicals
  result <- resp |> resp_body_json() |> pluck("endPoints")

  fields <- c("assay", "casrn", "dtxsid", "substanceName", "endpoint", "value")

  map(fields, \(x) map_chr(result, x)) |>
    set_names(fields) |>
    bind_cols() |>
    filter(endpoint == "Call", value == "Active") |>
    select(-c(endpoint, value)) |>
    distinct()
}

assays <- c(
  "APR_HepG2_p53Act_1hr",
  "APR_HepG2_p53Act_24hr",
  "APR_HepG2_p53Act_72hr",
  "ATG_p53_CIS",
  "TOX21_DT40_LUC",
  "TOX21_DT40_100_LUC",
  "TOX21_DT40_657_LUC",
  "TOX21_ELG1_LUC_Agonist",
  "TOX21_H2AX_HTRF_CHO_Agonist_ratio",
  "TOX21_p53_BLA_p1_ratio",
  "TOX21_p53_BLA_p2_ratio",
  "TOX21_p53_BLA_p3_ratio",
  "TOX21_p53_BLA_p4_ratio",
  "TOX21_p53_BLA_p5_ratio"
)

chemids <- unique(geo_tox_data$exposure$casn)

cHTS_hits <- get_cHTS_hits(assays = assays, chemids = chemids)
```

### Dose-response

Use the ICE API to retrieve dose-response data for selected assays and
chemicals, which is included in the GeoTox package data as
`geo_tox_data$dose_response`.

``` r

get_ICE_dose_resp <- function(assays = NULL, chemids = NULL) {
  if (is.null(assays) & is.null(chemids)) {
    stop("Must provide at least one of 'assays' or 'chemids'")
  }

  # Format query parameters
  req_params <- list()

  if (!is.null(assays)) {
    if (!is.list(assays)) assays <- as.list(assays)
    req_params$assays <- assays
  }

  if (!is.null(chemids)) {
    if (!is.list(chemids)) chemids <- as.list(chemids)
    req_params$chemids <- chemids
  }

  # Query ICE API
  resp <- request("https://ice.ntp.niehs.nih.gov/api/v1/curves") |>
    req_body_json(req_params) |>
    req_perform()

  if (resp$status_code != 200) {
    stop("Failed to retrieve data from ICE API")
  }

  # Return dose-response data
  result <- resp |> resp_body_json() |> pluck("curves")

  map(result, function(x) {
    tibble(
      endp = x[["endpoint"]],
      casn = x[["casrn"]],
      chnm = x[["substance"]],
      call = x[["call"]],
      logc = map_dbl(x[["concentrationResponses"]], "concentration") |> log10(),
      resp = map_dbl(x[["concentrationResponses"]], "response")
    )
  }) |>
    bind_rows()
}

assays <- unique(cHTS_hits$assay)

chemids <- intersect(cHTS_hits$casrn, geo_tox_data$exposure$casn)

dose_response <- get_ICE_dose_resp(assays = assays, chemids = chemids)

# Only keep active calls for assay/chemical combinations
geo_tox_data$dose_response <- dose_response |>
  filter(call == "Active") |>
  select(-call)

# Update dose-response chemical names using CompTox Dashboard data
geo_tox_data$dose_response <- geo_tox_data$dose_response |>
  inner_join(exposure_casrn, by = join_by(casn == CASRN)) |>
  select(endp, casn, chnm = PREFERRED_NAME, logc, resp)
```

## Population

### Age

Download age data from the [U.S. Census Bureau](https://www.census.gov/)
by searching for “County Population by Characteristics”. A subset of
data for North Carolina from
[2020](https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html)
is included in the GeoTox package data as `geo_tox_data$age`.

``` r

# Data for North Carolina
url <- paste0(
  "https://www2.census.gov/programs-surveys/popest/datasets/",
  "2020-2024/counties/asrh/cc-est2024-alldata-37.csv"
)
age <- read_csv(url)

geo_tox_data$age <- age |>
  # 7/1/2020 population estimate
  filter(YEAR == 2) |>
  # Create FIPS
  mutate(FIPS = str_c(STATE, COUNTY)) |>
  # Keep selected columns
  select(FIPS, AGEGRP, TOT_POP)
```

### Obesity status

Search [CDC data](https://data.cdc.gov/) for “places county data”. Go to
the desired dataset webpage, e.g. [2020 county
data](https://data.cdc.gov/500-Cities-Places/PLACES-County-Data-GIS-Friendly-Format-2020-releas/mssc-ksj7/about_data),
and download the data by selecting Actions → API → Download file. A
subset of data for North Carolina is included in the GeoTox package data
as `geo_tox_data$obesity`.

``` r

places <- read_csv(
  "PLACES__County_Data_(GIS_Friendly_Format),_2020_release.csv"
)

# Convert confidence interval to standard deviation
extract_SD <- function(x) {
  range <- as.numeric(str_split_1(str_sub(x, 2, -2), ","))
  diff(range) / 3.92
}

geo_tox_data$obesity <- places |>
  # North Carolina Counties
  filter(StateAbbr == "NC") |>
  # Select obesity data
  select(FIPS = CountyFIPS, OBESITY_CrudePrev, OBESITY_Crude95CI) |>
  # Change confidence interval to standard deviation
  rowwise() |>
  mutate(OBESITY_SD = extract_SD(OBESITY_Crude95CI)) |>
  ungroup() |>
  select(-OBESITY_Crude95CI)
```

### Steady-state plasma concentration (C_ss)

Use the `httk` package to generate C_ss values for combinations of age
group and weight status for each chemical. The generation of these
values is a time-intensive step, so one approach is to generate
populations of C_ss values initially and then sample them later. The
simulated C_ss values are included in the GeoTox package data as
`geo_tox_data$simulated_css`.

``` r

set.seed(2345)
n_samples <- 500

# Get CASN for which httk simulation is possible. Try using load_dawson2021,
# load_sipes2017, or load_pradeep2020 to increase availability.
load_sipes2017()
casn <- intersect(
  unique(geo_tox_data$dose_response$casn),
  get_cheminfo(suppress.messages = TRUE)
)

# Define population demographics for httk simulation
pop_demo <- cross_join(
  tibble(age_group = list(
    c(0, 2), c(3, 5), c(6, 10), c(11, 15), c(16, 20), c(21, 30), c(31, 40),
    c(41, 50), c(51, 60), c(61, 70), c(71, 100)
  )),
  tibble(weight = c("Normal", "Obese"))
)

# Create wrapper function around httk steps
simulate_css <- function(
  chem.cas,
  agelim_years,
  weight_category,
  samples,
  verbose = TRUE
) {
  if (verbose) {
    cat(
      chem.cas,
      paste0("(", paste(agelim_years, collapse = ", "), ")"),
      weight_category,
      "\n"
    )
  }

  httkpop <- list(
    method = "vi",
    gendernum = NULL,
    agelim_years = agelim_years,
    agelim_months = NULL,
    weight_category = weight_category,
    reths = c(
      "Mexican American",
      "Other Hispanic",
      "Non-Hispanic White",
      "Non-Hispanic Black",
      "Other"
    )
  )

  css <- try(
    suppressWarnings({
      mcs <- create_mc_samples(
        chem.cas = chem.cas,
        samples = samples,
        httkpop.generate.arg.list = httkpop,
        suppress.messages = TRUE
      )

      calc_analytic_css(
        chem.cas = chem.cas,
        parameters = mcs,
        model = "3compartmentss",
        suppress.messages = TRUE
      )
    }),
    silent = TRUE
  )

  # Return
  if (is(css, "try-error")) {
    warning("simulate_css() failed to generate data for CASN ", chem.cas)
    list(NA)
  } else {
    list(css)
  }
}

# Simulate C_ss values
simulated_css <- map(casn, function(chem.cas) {
  pop_demo |>
    rowwise() |>
    mutate(
      css = simulate_css(.env$chem.cas, age_group, weight, .env$n_samples)
    ) |>
    ungroup()
})
simulated_css <- setNames(simulated_css, casn)

# Remove CASN that failed simulate_css
casn_keep <- map_lgl(simulated_css, function(df) {
  !(length(df$css[[1]]) == 1 && is.na(df$css[[1]]))
})
simulated_css <- simulated_css[casn_keep]

geo_tox_data$simulated_css <- simulated_css |>
  map(\(x) {
    x |>
      mutate(
        age_lb = map_int(age_group, first),
        age_ub = map_int(age_group, last)
      ) |>
      select(age_lb, age_ub, weight, css) |>
      unnest(css)
  }) |>
  enframe(name = "casn") |>
  unnest(value)
```

## Retain overlap

Retain only those chemicals found in exposure, dose-response and C_ss
datasets.

``` r

casn_keep <- unique(geo_tox_data$simulated_css$casn)

geo_tox_data$exposure <- geo_tox_data$exposure |>
  filter(casn %in% casn_keep)

geo_tox_data$dose_response <- geo_tox_data$dose_response |>
  filter(casn %in% casn_keep)
```

## County/State boundaries

Download cartographic boundary files for counties and states from the
[U.S. Census
Bureau](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html).
The geometry data for North Carolina counties and the state are included
in the GeoTox package data as `geo_tox_data$boundaries`.

``` r

county <- st_read("cb_2020_us_county_5m/cb_2020_us_county_5m.shp")
state <- st_read("cb_2020_us_state_5m/cb_2020_us_state_5m.shp")

geo_tox_data$boundaries <- list(
  county = county |>
    filter(STATEFP == 37) |>
    select(FIPS = GEOID, geometry),
  state = state |>
    filter(STATEFP == 37) |>
    select(geometry)
)
```
