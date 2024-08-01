library(dplyr)
library(sf)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(readxl)
library(httk)
library(httr2)
rm(list = ls())

# This dataset is documented in R/data.R and vignettes/package_data.Rmd

# Updating the raw data is an interactive process. There are several steps
# that require manual intervention. These steps will create errors if run
# in a non-interactive environment.
update_data_raw <- FALSE # Last updated 7/29/2024

geo_tox_data <- list()

################################################################################
## Chemical data
################################################################################

########################################
### ICE cHTS data
########################################

if (update_data_raw) {
  
  assays <- c("APR_HepG2_p53Act_1h_dn",
              "APR_HepG2_p53Act_1h_up",
              "APR_HepG2_p53Act_24h_dn",
              "APR_HepG2_p53Act_24h_up",
              "APR_HepG2_p53Act_72h_dn",
              "APR_HepG2_p53Act_72h_up",
              "ATG_p53_CIS_up",
              "TOX21_DT40",
              "TOX21_DT40_100",
              "TOX21_DT40_657",
              "TOX21_ELG1_LUC_Agonist",
              "TOX21_H2AX_HTRF_CHO_Agonist_ratio",
              "TOX21_p53_BLA_p1_ratio",
              "TOX21_p53_BLA_p2_ratio",
              "TOX21_p53_BLA_p3_ratio",
              "TOX21_p53_BLA_p4_ratio",
              "TOX21_p53_BLA_p5_ratio")
  
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
    
    fields <- c("assay", "casrn", "dtxsid", "substanceName",
                "endpoint", "value")
    
    map(fields, \(x) map_chr(result, x)) |> 
      set_names(fields) |>
      bind_cols() |> 
      filter(endpoint == "Call", value == "Active") |> 
      select(-c(endpoint, value)) |> 
      distinct()
  }
  
  cHTS_hits_API <- get_cHTS_hits(assays = assays)
  saveRDS(cHTS_hits_API, "data-raw/cHTS_hits_API.rds")
} else {
  cHTS_hits_API <- readRDS("data-raw/cHTS_hits_API.rds")
}

########################################
### Exposure data
########################################

if (update_data_raw) {
  download.file(
    paste0("https://www.epa.gov/system/files/documents/2022-12/",
           "2019_Toxics_Exposure_Concentrations.xlsx"),
    "data-raw/2019_Toxics_Exposure_Concentrations.xlsx"
  )
}
exposure <- read_xlsx("data-raw/2019_Toxics_Exposure_Concentrations.xlsx")

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

geo_tox_data$exposure <- exposure %>%
  # North Carolina counties
  filter(State == "NC", !grepl("0$", FIPS)) %>%
  # Aggregate chemicals by county
  summarize(across(-c(State:Tract), c(mean, sd)), .by = FIPS) %>%
  pivot_longer(-FIPS, names_to = "chemical") %>%
  mutate(
    stat = if_else(grepl("_1$", chemical), "mean", "sd"),
    chemical = gsub('.{2}$', '', chemical)
  ) %>%
  pivot_wider(names_from = stat) %>%
  # Normalize concentrations
  mutate(norm = min_max_norm(mean), .by = chemical)

########################################
# Get CASRN and PREFERRED_NAME from CompTox Dashboard
########################################

if (update_data_raw) {
  stop("Manually downlaod CompTox Dashboard data from:\n",
       "https://comptox.epa.gov/dashboard/batch-search")
  # Copy/paste the chemical names into the batch search field
  cat(geo_tox_data$exposure |> distinct(chemical) |> pull(), sep = "\n")
  # Save the results as "data-raw/CCD-Batch-Search.csv"
}

exposure_casrn <- read_csv("data-raw/CCD-Batch-Search.csv",
                           show_col_types = FALSE) |> 
  filter(DTXSID != "N/A") |> 
  # Prioritize results based on FOUND_BY status
  arrange(INPUT,
          grepl("Approved Name", FOUND_BY),
          grepl("^Synonym", FOUND_BY)) |> 
  # Keep one result per INPUT
  group_by(INPUT) |> 
  slice(1) |> 
  ungroup()

# Update exposure data with CompTox Dashboard data
geo_tox_data$exposure <- geo_tox_data$exposure |> 
  inner_join(exposure_casrn, by = join_by(chemical == INPUT)) |> 
  select(FIPS, casn = CASRN, chnm = PREFERRED_NAME, mean, sd, norm)

########################################
### Dose-response data
########################################

if (update_data_raw) {
  assays <- unique(cHTS_hits_API$assay)
  chemids <- intersect(cHTS_hits_API$casrn, geo_tox_data$exposure$casn)
  
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
    
    lapply(result, function(x) {
      tibble(
        endp = x[["assay"]],
        casn = x[["casrn"]],
        call = x[["dsstoxsid"]],
        chnm = x[["substance"]],
        call = x[["call"]],
        logc = map_dbl(x[["concentrationResponses"]], "concentration") |> log10(),
        resp = map_dbl(x[["concentrationResponses"]], "response")
      )
    }) |> 
      bind_rows()
  }
  
  dose_response <- get_ICE_dose_resp(assays = assays, chemids = chemids)
  saveRDS(dose_response, "data-raw/dose_response.rds")
} else {
  dose_response <- readRDS("data-raw/dose_response.rds")
}

# Only keep active calls for assay/chemical combinations
geo_tox_data$dose_response <- dose_response |> 
  filter(call == "Active") |> 
  select(-call)

# Update dose-response data with CompTox Dashboard data
geo_tox_data$dose_response <- geo_tox_data$dose_response |>
  inner_join(exposure_casrn, by = join_by(casn == CASRN)) |>
  select(endp, casn, chnm = PREFERRED_NAME, logc, resp)

################################################################################
## Population data
################################################################################

########################################
### Age
########################################

if (update_data_raw) {
  download.file(
    paste0(
      "https://www2.census.gov/programs-surveys/popest/datasets/",
      "2010-2019/counties/asrh/cc-est2019-alldata-37.csv"),
    "data-raw/cc-est2019-alldata-37.csv")
}
age <- read_csv("data-raw/cc-est2019-alldata-37.csv", show_col_types = FALSE)

geo_tox_data$age <- age %>%
  # 7/1/2019 population estimate
  filter(YEAR == 12) %>%
  # Create FIPS
  mutate(FIPS = str_c(STATE, COUNTY)) %>%
  # Keep selected columns
  select(FIPS, AGEGRP, TOT_POP)

########################################
### Obesity
########################################

if (update_data_raw) {
  stop("Manually download obesity data from:\n",
       "https://data.cdc.gov/500-Cities-Places/",
       "PLACES-County-Data-GIS-Friendly-Format-2020-releas/",
       "mssc-ksj7/about_data",
       "\nvia Actions -> API -> Download file")
}
places <- read_csv(
  "data-raw/PLACES__County_Data__GIS_Friendly_Format___2020_release.csv",
  show_col_types = FALSE)

# Convert confidence interval to standard deviation
extract_SD <- function(x) {
  range <- as.numeric(str_split_1(str_sub(x, 2, -2), ","))
  diff(range) / 3.92
}

geo_tox_data$obesity <- places %>%
  # North Carolina Counties
  filter(StateAbbr == "NC") %>%
  # Select obesity data
  select(FIPS = CountyFIPS, OBESITY_CrudePrev, OBESITY_Crude95CI) %>%
  # Change confidence interval to standard deviation
  rowwise() %>%
  mutate(OBESITY_SD = extract_SD(OBESITY_Crude95CI)) %>%
  ungroup() %>%
  select(-OBESITY_Crude95CI)

########################################
### Steady-state plasma concentration (Css)
########################################

if (update_data_raw) {
  
  load_sipes2017()
  
  set.seed(2345)
  n_samples <- 500
  
  # Define population demographics for httk simulation
  pop_demo <- cross_join(
    tibble(age_group = list(c(0, 2), c(3, 5), c(6, 10), c(11, 15),
                            c(16, 20), c(21, 30), c(31, 40), c(41, 50),
                            c(51, 60), c(61, 70), c(71, 100))),
    tibble(weight = c("Normal", "Obese"))) |>
    # Create column of lower age_group values
    rowwise() |>
    mutate(age_min = age_group[1]) |>
    ungroup()
  
  # Create wrapper function around httk steps
  simulate_css <- function(chem.cas, agelim_years, weight_category,
                           samples, verbose = TRUE) {
    
    if (verbose) {
      cat(chem.cas,
          paste0("(", paste(agelim_years, collapse = ", "), ")"),
          weight_category,
          "\n")
    }
    
    httkpop <- list(method = "vi",
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
                    ))
    
    css <- try(
      suppressWarnings({
        mcs <- create_mc_samples(chem.cas = chem.cas,
                                 samples = samples,
                                 httkpop.generate.arg.list = httkpop,
                                 suppress.messages = TRUE)
        
        calc_analytic_css(chem.cas = chem.cas,
                          parameters = mcs,
                          model = "3compartmentss",
                          suppress.messages = TRUE)
      }),
      silent = TRUE
    )
    
    # Return
    if (is(css, "try-error")) {
      warning(paste0("simulate_css failed to generate data for CASN ", chem.cas))
      list(NA)
    } else {
      list(css)
    }
  }
  
  # Get CASN
  casn <- unique(geo_tox_data$dose_response$casn)
  
  # Simulate Css values
  simulated_css <- lapply(casn, function(chem.cas) {
    pop_demo |>
      rowwise() |>
      mutate(
        css = simulate_css(.env$chem.cas, age_group, weight, .env$n_samples)
      ) |>
      ungroup()
  })
  simulated_css <- setNames(simulated_css, casn)
  
  # Get median Css values for each age_group
  simulated_css <- lapply(
    simulated_css,
    function(cas_df) {
      cas_df |>
        nest(.by = age_group) |>
        mutate(
          age_median_css = sapply(data, function(df) median(unlist(df$css),
                                                            na.rm = TRUE))
        ) |>
        unnest(data)
    }
  )
  
  # Get median Css values for each weight
  simulated_css <- lapply(
    simulated_css,
    function(cas_df) {
      cas_df |>
        nest(.by = weight) |>
        mutate(
          weight_median_css = sapply(data, function(df) median(unlist(df$css),
                                                               na.rm = TRUE))
        ) |>
        unnest(data) |>
        arrange(age_min, weight)
    }
  )
  
  saveRDS(simulated_css, "data-raw/simulated_css.rds")
} else {
  simulated_css <- readRDS("data-raw/simulated_css.rds")
}

geo_tox_data$simulated_css <- simulated_css

########################################
### Prune data
########################################

casn_keep <- names(geo_tox_data$simulated_css)

geo_tox_data$exposure <- geo_tox_data$exposure |> 
  filter(casn %in% casn_keep)

geo_tox_data$dose_response <- geo_tox_data$dose_response |> 
  filter(casn %in% casn_keep)

################################################################################
## County/State boundaries
################################################################################

if (update_data_raw) {
  stop("Manually download county and state boundaries from:\n",
       "https://www.census.gov/geographies/mapping-files/",
       "time-series/geo/cartographic-boundary.html")
}
county <- st_read("data-raw/cb_2019_us_county_5m/cb_2019_us_county_5m.shp")
state <- st_read("data-raw/cb_2019_us_state_5m/cb_2019_us_state_5m.shp")

geo_tox_data$boundaries <- list(
  county = county %>%
    filter(STATEFP == 37) %>%
    select(FIPS = GEOID, geometry),
  state = state %>%
    filter(STATEFP == 37) %>%
    select(geometry)
)

################################################################################
## Save
################################################################################

usethis::use_data(geo_tox_data, overwrite = TRUE)
