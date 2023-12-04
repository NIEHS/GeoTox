library(devtools)
library(tidyverse)

################################################################################
################################################################################
# 00-Sensitivity.R
################################################################################
################################################################################

# TODO

################################################################################
################################################################################
# [01-05]-Sensitivity.R
################################################################################
################################################################################

rm(list = ls())
load_all()

set.seed(2345)

MC_iter <- 10 # Max 1000 when using pre-generated C_ss values

step <- 1 # vary age
# step <- 2 # vary obesity
# step <- 3 # vary httk
# step <- 4 # vary dose-response params
# step <- 5 # vary external concentration

##########
# Data
load("~/dev/GeoTox/data/county_cyp1a1_up_20220201.RData")
age.data <- read.csv("~/dev/GeoTox/data/cc-est2019-alldata.csv")
if (step == 1) {
  load("~/dev/GeoTox/data/css_by_county_sensitivity_age_20220228.RData")
  css.sensitivity.age <- lapply(css.sensitivity.age, function(mat) mat[1:MC_iter, ])
  C_ss <- css.sensitivity.age
} else if (step == 2) {
  load("~/dev/GeoTox/data/css_by_county_sensitivity_obesity_20220228.RData")
  css.sensitivity.obesity <- lapply(css.sensitivity.obesity, function(mat) mat[1:MC_iter, ])
  C_ss <- css.sensitivity.obesity
} else if (step == 3) {
  load("~/dev/GeoTox/data/css_by_county_sensitivity_httk_20220228.RData")
  css.sensitivity.httk <- lapply(css.sensitivity.httk, function(mat) mat[1:MC_iter, ])
  C_ss <- css.sensitivity.httk
} else if (step == 4 | step == 5) {
  # TODO 04 uses 20220201, 05 uses 20220228
  if (step == 4) {
    load("~/dev/GeoTox/data/css_by_county_20220201.RData")
  } else {
    load("~/dev/GeoTox/data/css_by_county_20220228.RData")
  }
  # Replace missing values with mean
  for (i in 1:length(css.by.county)) {
    for (j in 1:ncol(css.by.county[[i]])) {
      idx <- is.na(css.by.county[[i]][, j])
      if (any(idx)) {
        css.by.county[[i]][idx, j] <- mean(css.by.county[[i]][!idx, j])
      }
    }
  }
  # Use median values
  # TODO why mean-impute then use median?
  #      why not just use median of non-NA values?
  C_ss <- lapply(
    css.by.county,
    function(x) rep(median(x), length.out = MC_iter)
  )
}

# Filter age to desired year
age <- age.data %>%
  filter(YEAR == 7) %>% # 7/1/2014 Census population
  mutate(FIPS = as.numeric(sprintf("%d%03d", STATE, COUNTY))) %>%
  select(FIPS, AGEGRP, TOT_POP) %>%
  # Update FIPS: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
  mutate(FIPS = if_else(FIPS == 46102, 46113, FIPS)) %>%
  filter(FIPS %in% unique(.env$county_cyp1a1_up$FIPS))

# Span of 6 FIPS in census.age.overlap ("age" in this script) are out of order
plot(unique(age$FIPS)[2378:2385])
# These are later multiplied by css.sensitivity.age
# order of css.sensitivity.age?
# load("~/dev/GeoTox/data/css_by_county_sensitivity_age_20220228.RData")
# same as order of age.by.county?
# load("~/dev/GeoTox/data/age_by_county_20220228.RData")
# unsure where age.by.county comes from, but maybe same FIPS order as census.age.overlap?

########################################
# cyp1a1
cyp1a1 <- split(county_cyp1a1_up, ~FIPS)

########################################
# age
age_split <- split(age, ~FIPS)

# Adjust order due to changing FIPS 46102 to 46113
# TODO does this adjustment line up with other input data, e.g. C_ss?
idx <- 1:length(age_split)
idx[2379:2384] <- c(2384, 2379:2383)
age_split <- age_split[idx]

# Simulate ages
simulated_age <- lapply(age_split, simulate_age, n = MC_iter)
if (step != 1) {
  simulated_age <- lapply(
    simulated_age,
    function(x) rep(median(x), length.out = MC_iter)
  )
}

########################################
# inhalation rate
simulated_IR <- lapply(simulated_age, simulate_inhalation_rate)

########################################
# exposure
if (step != 5) {
  simulated_exposure <- lapply(
    lapply(cyp1a1, "[[", "concentration_mean"),
    simulate_exposure,
    n = MC_iter
  )
} else {
  simulated_exposure <- mapply(
    simulate_exposure,
    mean = lapply(cyp1a1, "[[", "concentration_mean"),
    sd = lapply(cyp1a1, "[[", "concentration_sd"),
    n = MC_iter,
    SIMPLIFY = FALSE
  )
}

########################################
# internal dose
internal_dose <- mapply(
  calc_internal_dose,
  C_ext = lapply(simulated_exposure, function(x) x / 1000),
  IR = simulated_IR,
  SIMPLIFY = FALSE
)

########################################
# in vitro concentration
invitro_concentration <- mapply(
  calc_invitro_concentration,
  D_int = internal_dose,
  C_ss = C_ss,
  SIMPLIFY = FALSE
)

########################################
# concentration response
concentration_response <- mapply(
  calc_concentration_response,
  resp = cyp1a1,
  concentration = invitro_concentration,
  tp_b_mult = ifelse(step == 4, 1.2, 1.5),
  fixed = step != 4,
  SIMPLIFY = FALSE
)

# saveRDS(
#   concentration_response,
#   paste0("~/dev/GeoTox/outputs/conc_resp_", step, ".rds")
# )

# TODO correction of age carried to simulated_IR, but others are original order
idx <- 2379:2384
as.data.frame(cbind(
  "cyp1a1" = names(cyp1a1)[idx],
  "sim_age" = names(simulated_age)[idx],
  "sim_IR" = names(simulated_IR)[idx],
  "sim_exposure" = names(simulated_exposure)[idx],
  "internal_dose" = names(internal_dose)[idx],
  "invitro_conc" = names(invitro_concentration)[idx],
  "conc_resp" = names(concentration_response)[idx]
))

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

library(truncnorm)
source("~/github/GeoToxMIE/helper_functions/census-age-sim.R")
source("~/github/GeoToxMIE/helper_functions/sim-IR-BW.R")
source("~/github/GeoToxMIE/helper_functions/GCA-obj.R")
source("~/github/GeoToxMIE/helper_functions/tcplHillConc_v2.R")
source("~/github/GeoToxMIE/helper_functions/IA-Pred.R")
source("~/github/GeoToxMIE/helper_functions/tcplHillVal_v2.R")
source("~/github/GeoToxMIE/helper_functions/ECmix-obj.R")

set.seed(2345)

MC.iter <- MC_iter

########################################
# cyp1a1

# Run lines
# 01: 51
# 02: 45
# 03: 43
# 04: 44
# 05: 42

all.equal(cyp1a1_up.by.county, cyp1a1)

########################################
# age

# Run lines
# 01: 59-75
# 02: 53-72
# 03: 48-67
# 04: 49-68
# 05: 47-66

if (step != 1) {
  age.by.county <- age.by.county.median
}

all.equal(census.age.overlap, age)
all.equal(age.by.county, simulated_age)

########################################
# inhalation rate

# Run lines
# 01: 82
# 02: 76
# 03: 71
# 04: 72
# 05: 70

all.equal(IR.by.county, simulated_IR)

# Note for steps with fixed age
# only index 2384 (FIPS 46113) has median ages in different IR age groups
if (step != 1) {
  cbind(
    sapply(age.by.county.median[2379:2384], "[", 1),
    sapply(simulated_age[2379:2384], "[", 1)
  )
}

########################################
# exposure

# Run lines
# 01: 88-128
# 02: 80-119
# 03: 76-115
# 04: 77-116
# 05: 75-114

all.equal(external.dose.by.county, simulated_exposure)

########################################
# internal dose

# Run lines
# 01: 130-135
# 02: 121-126
# 03: 118-123
# 04: 119-124
# 05: 117-122

all.equal(inhalation.dose.by.county, internal_dose)

########################################
# in vitro concentration

if (step == 4 | step == 5) {
  css.by.county.median <- C_ss
}

# Run lines
# 01: 145-151
# 02: 136-142
# 03: 134-140
# 04: 152-158
# 05: 150-156

all.equal(invitro.conc.by.county, invitro_concentration)

########################################
# concentration response

# Run lines
# 01: 154-246
# 02: 148-239
# 03: 145-236
# 04: 163-257
# 05: 161-255

all.equal(final.response.by.county, concentration_response)

################################################################################
################################################################################
# 06-Sensitivity.R
################################################################################
################################################################################

# TODO
