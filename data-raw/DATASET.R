library(tidyverse)
rm(list = ls())

# This dataset is documented in R/data.R

################################################################################
# Concentration response
################################################################################

load("~/dev/GeoTox/data/LTEA_HepaRG_CYP1A1_up 41 chems for Kyle 220131.RData")
ice <- cdat; rm(cdat)

################################################################################
# Sensitivity
################################################################################

MC_iter <- 25 # Max 1000
states <- c(37) # NC

########################################
# county_cyp1a1_up, fips
# fips is used to index the Css data below
########################################
load("~/dev/GeoTox/data/county_cyp1a1_up_20220201.RData")
fips <- unique(county_cyp1a1_up$FIPS)
county_cyp1a1_up <- county_cyp1a1_up %>%
  filter(STATE %in% as.character(states))
fips_idx <- fips %in% county_cyp1a1_up$FIPS

########################################
# age
########################################
age <- read.csv("~/dev/GeoTox/data/cc-est2019-alldata.csv")
age <- age %>%
  filter(
    YEAR == 7,  # 7/1/2014 Census population
    STATE %in% states
  ) %>%
  mutate(FIPS = as.numeric(sprintf("%d%03d", STATE, COUNTY))) %>%
  select(FIPS, AGEGRP, TOT_POP) %>%
  arrange(FIPS, AGEGRP)

########################################
# Css
########################################
Css <- list()

####################
# Css age
####################
load("~/dev/GeoTox/data/css_by_county_sensitivity_age_20220228.RData")
css.sensitivity.age <- lapply(
  css.sensitivity.age[fips_idx], function(mat) mat[1:MC_iter, ]
)
names(css.sensitivity.age) <- fips[fips_idx]
Css[["age"]] <- css.sensitivity.age

####################
# Css obesity
####################
load("~/dev/GeoTox/data/css_by_county_sensitivity_obesity_20220228.RData")
css.sensitivity.obesity <- lapply(
  css.sensitivity.obesity[fips_idx], function(mat) mat[1:MC_iter, ]
)
names(css.sensitivity.obesity) <- fips[fips_idx]
Css[["obesity"]] <- css.sensitivity.obesity

####################
# Css httk
####################
load("~/dev/GeoTox/data/css_by_county_sensitivity_httk_20220228.RData")
css.sensitivity.httk <- lapply(
  css.sensitivity.httk[fips_idx], function(mat) mat[1:MC_iter, ]
)
names(css.sensitivity.httk) <- fips[fips_idx]
Css[["httk"]] <- css.sensitivity.httk

####################
# Css dose-resp
####################
load("~/dev/GeoTox/data/css_by_county_20220201.RData")
css.by.county <- lapply(
  css.by.county[fips_idx], function(mat) mat[1:MC_iter, ]
)
names(css.by.county) <- fips[fips_idx]
# Any NA to impute?
# TODO need to impute? Some missing with higher MC_iter values
table(sapply(css.by.county, \(x) any(is.na(x))))
# Use median values
Css[["dose-resp"]] <- lapply(
  css.by.county,
  function(x) rep(median(x, na.rm = TRUE), length.out = MC_iter)
)

####################
# Css ext-conc
####################
load("~/dev/GeoTox/data/css_by_county_20220228.RData")
css.by.county <- lapply(
  css.by.county[fips_idx], function(mat) mat[1:MC_iter, ]
)
names(css.by.county) <- fips[fips_idx]
# Any NA to impute?
# TODO need to impute? Some missing with higher MC_iter values
table(sapply(css.by.county, \(x) any(is.na(x))))
# Use median values
Css[["ext-conc"]] <- lapply(
  css.by.county,
  function(x) rep(median(x, na.rm = TRUE), length.out = MC_iter)
)

################################################################################
# Create data file
################################################################################

geo_tox_data <- list(
  ice = ice,
  MC_iter = MC_iter,
  cyp1a1 = county_cyp1a1_up,
  age = age,
  Css = Css
)

usethis::use_data(geo_tox_data, overwrite = TRUE)
