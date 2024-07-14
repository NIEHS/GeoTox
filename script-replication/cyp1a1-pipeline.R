library(devtools)
library(tidyverse)

################################################################################
################################################################################
# 01-CYP1A1-Pipeline.R
# Aggregate chemical data at county level
################################################################################
################################################################################

rm(list = ls())

# Generated from script-replication/conc-resp.R
fit_params <- readRDS("~/dev/GeoTox/outputs/fit_params.rds")

nata_df <- read.csv("~/dev/GeoTox/data/2014_NATA_CONCS.csv")
nata_chemicals <- read.csv("~/dev/GeoTox/data/NATA_pollutant_names_casrn.csv")

# TODO this removes first \(.*\) preceded by a space, what if there are 2?
# See row 90 for an example
nata_chems <- nata_chemicals %>%
  mutate(
    web_name = str_to_title(web_name),
    web_name = str_replace(web_name, "\\s+\\([^\\)]+\\)", "")
  )
nata_chems[90, ]

# Gather county level chemical data
county_cyp1a1_data <- inner_join(
  # Compute county mean
  nata_df %>%
    summarise(across(ACETALD:XYLENES, mean), .by = STCOFIPS) %>%
    pivot_longer(-STCOFIPS, values_to = "concentration_mean"),
  # Compute county sd
  nata_df %>%
    summarise(across(ACETALD:XYLENES, sd), .by = STCOFIPS) %>%
    pivot_longer(-STCOFIPS, values_to = "concentration_sd"),
  by = join_by(STCOFIPS, name)) %>%
  # Rename columns
  rename(FIPS = STCOFIPS, chemical = name) %>%
  # Add chemical info
  left_join(nata_chems, join_by(chemical == smoke_name)) %>%
  # Add STATE code
  mutate(STATE = sprintf("%02d", FIPS %/% 1000)) %>%
  # Limit to continental USA
  filter(!(STATE %in% c("02", "15", "60", "66", "69", "72", "78"))) %>%
  # Add 2-parameter hill fit data (and drop those without fit data)
  right_join(fit_params, join_by(casrn == casn))

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

library(reshape2)

load("~/dev/GeoTox/data/Hill_2param_model_fit.RData") # 02-Conc-Response-Fit.R
hill2.fit <- df.params; rm(df.params)

# Run lines 56-97

all.equal(
  county_cyp1a1_up   %>% arrange(FIPS, chemical),
  county_cyp1a1_data %>% arrange(FIPS, chemical) %>%
    select(-ends_with("imputed"), -chnm),
  check.attributes = FALSE
)

################################################################################
################################################################################
# 02-CYP1A1-Pipeline.R
# Simulate obesity data
################################################################################
################################################################################

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# TODO Error on line 107, index is not passed to rnorm
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Example
temp <- data.frame(
  OBESITY_CrudePrev = 1:5,
  OBESITY_SD = rep(0, 5)
)
# Original code
lapply(
  1:length(temp$OBESITY_CrudePrev),
  function(x) rnorm(3, temp$OBESITY_CrudePrev, temp$OBESITY_SD)
)
# Corrected code
lapply(
  1:length(temp$OBESITY_CrudePrev),
  function(x) rnorm(3, temp$OBESITY_CrudePrev[x], temp$OBESITY_SD[x])
)

########################################

rm(list = ls())
load_all()

set.seed(2345)

MC_iter <- 10 # Max 1000 when using pre-generated C_ss values

##########
# Data
load("~/dev/GeoTox/data/county_cyp1a1_up_20220201.RData")
age.data <- read.csv("~/dev/GeoTox/data/cc-est2019-alldata.csv")
places <- read.csv(paste0(
  "~/dev/GeoTox/data/",
  "PLACES__County_Data__GIS_Friendly_Format___2020_release.csv"
))

# Filter age to desired year
age <- age.data %>%
  filter(YEAR == 7) %>% # 7/1/2014 Census population
  mutate(FIPS = as.numeric(sprintf("%d%03d", STATE, COUNTY))) %>%
  select(FIPS, AGEGRP, TOT_POP) %>%
  # Update FIPS: https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
  mutate(FIPS = if_else(FIPS == 46102, 46113, FIPS)) %>%
  filter(FIPS %in% unique(.env$county_cyp1a1_up$FIPS))

########################################
# age
age_split <- split(age, ~FIPS)

# Adjust order due to changing FIPS 46102 to 46113
idx <- 1:length(age_split)
idx[2379:2384] <- c(2384, 2379:2383)
age_split <- age_split[idx]

# Simulate ages
simulated_age <- lapply(age_split, simulate_age, n = MC_iter)

# Compute median for each county (saved as "age_by_county.csv")
age_summary <- data.frame(
  L1 = seq_len(length(simulated_age)),
  age_median = sapply(simulated_age, median)
)

########################################
# inhalation rate
simulated_IR <- lapply(simulated_age, simulate_inhalation_rate)

# Compute median for each county (saved as "IR_by_county.csv")
IR_summary <- data.frame(
  L1 = seq_len(length(simulated_IR)),
  IR_median = sapply(simulated_IR, median, na.rm = TRUE)
)

########################################
# obesity
grep("obesity", names(places), ignore.case = TRUE, value = TRUE)

# Get obesity sd for each county

extract_CI <- function(x) {
  range <- as.numeric(unlist(str_extract_all(x, "\\d+\\.\\d+")))
  diff(range) / 3.92
}

obesity <- places %>%
  select(CountyFIPS, OBESITY_CrudePrev, OBESITY_Crude95CI) %>%
  rowwise() %>%
  mutate(OBESITY_SD = extract_CI(OBESITY_Crude95CI)) %>%
  ungroup() %>%
  select(-OBESITY_Crude95CI) %>%
  mutate(CountyFIPS = if_else(CountyFIPS == 46102, 46113, CountyFIPS)) %>%
  filter(CountyFIPS %in% unique(.env$county_cyp1a1_up$FIPS))

# Simulate data by county
simulated_obesity <- obesity %>%
  rowwise() %>%
  mutate(
    p = list(rnorm(.env$MC_iter, OBESITY_CrudePrev, OBESITY_SD)),
    value = list(rbinom(.env$MC_iter, 1, unlist(p) / 100))
  )

obesity_by_county <- simulated_obesity$value

# Summarize obesity by county
obesity_summary <- data.frame(
  L1 = seq_len(length(obesity_by_county)),
  IR_median = sapply(obesity_by_county, median, na.rm = TRUE)
)

# Simulate data by county
# TODO check for age and obesity index differences
simulated_obesity_label <- mapply(
  function(obesity, age) {
    tibble(obesity, age) %>%
      mutate(
        label = case_when(
          age < 18 ~ "Normal",
          obesity == 0 ~ "Normal",
          obesity == 1 ~ "Obese",
          .default = "Blank"
        )
      ) %>% pull(label)
  },
  obesity_by_county,
  simulated_age,
  SIMPLIFY = FALSE
)

########################################
# cyp1a1
cyp1a1 <- split(county_cyp1a1_up, ~FIPS)

########################################
# exposure
simulated_exposure <- mapply(
  simulate_exposure,
  mean = lapply(cyp1a1, "[[", "concentration_mean"),
  sd = lapply(cyp1a1, "[[", "concentration_sd"),
  n = MC_iter,
  SIMPLIFY = FALSE
)

########################################
# internal dose
internal_dose <- mapply(
  calc_internal_dose,
  C_ext = lapply(simulated_exposure, function(x) x / 1000),
  IR = simulated_IR,
  SIMPLIFY = FALSE
)

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

library(reshape2)
library(truncnorm)
source("~/github/GeoToxMIE/helper_functions/census-age-sim.R")
source("~/github/GeoToxMIE/helper_functions/sim-IR-BW.R")
source("~/github/GeoToxMIE/helper_functions/tcplHillConc_v2.R")

set.seed(2345)

MC.iter <- MC_iter

########################################
# age

# Run lines 48-69

all.equal(census.age.overlap, age)
all.equal(age.by.county, simulated_age)
all.equal(age.summary, age_summary, check.attributes = FALSE)

########################################
# inhalation rate

# Run lines 73-77

all.equal(IR.by.county, simulated_IR)
all.equal(IR.summary, IR_summary, check.attributes = FALSE)

########################################
# obesity

# Run lines 86-104

# Fix for line 107
obesity.binom.p <- lapply(
  1:length(places$OBESITY_CrudePrev),
  function(x) rnorm(MC.iter, places$OBESITY_CrudePrev[x], places$OBESITY_SD[x])
)

# Run lines 111-120

hist(unlist(simulated_obesity$value))
all.equal(obesity.binom.p, simulated_obesity$p)
all.equal(obesity.by.county, obesity_by_county)
all.equal(obesity.summary, obesity_summary, check.attributes = FALSE)

# Run line 125

all.equal(obesity.by.county, simulated_obesity_label)

########################################
# cyp1a1

# Run line 139

all.equal(cyp1a1_up.by.county, cyp1a1)

########################################
# exposure

# Run line 143-177

all.equal(external.dose.by.county, simulated_exposure)

########################################
# internal dose

# Run lines 193-199

all.equal(inhalation.dose.by.county, internal_dose)

################################################################################
################################################################################
# 03-CYP1A1-Pipeline.R
# Simulate Css data
################################################################################
################################################################################

library(httk)

rm(list = ls())

set.seed(2345)

MC_iter <- 2
n_chem <- 2

##########
# Data
load("~/dev/GeoTox/data/county_cyp1a1_up_20220201.RData")

########################################

in_chems <- unique(county_cyp1a1_up$casrn)[1:n_chem]

age_weight <- cross_join(
  tibble(
    age_group = list(
      c(0, 2), c(3, 5), c(6, 10), c(11, 15), c(16, 20), c(21, 30),
      c(31, 40), c(41, 50), c(51, 60), c(61, 70), c(71, 79)
    )
  ),
  tibble(
    weight = c("Normal", "Obese")
  )
)

css_list <- vector("list", length(in_chems))
val <- matrix(NA, nrow = MC_iter, ncol = nrow(age_weight))
for (i in 1:length(in_chems)) {
  for (j in 1:nrow(age_weight)) {

    print(c(i, j))

    mcs <- create_mc_samples(
      chem.cas = in_chems[i],
      samples = MC_iter,
      httkpop.generate.arg.list = list(
        method = "vi",
        gendernum = NULL,
        agelim_years = age_weight$age_group[[j]],
        agelim_months = NULL,
        weight_category = age_weight$weight[j],
        reths = c(
          "Mexican American",
          "Other Hispanic",
          "Non-Hispanic White",
          "Non-Hispanic Black",
          "Other"
        )
      )
    )

    css <- calc_analytic_css(
      chem.cas = in_chems[i],
      parameters = mcs,
      model = "3compartmentss",
      suppress.messages = TRUE
    )

    val[,j] <- css
  }
  css_list[[i]] <- val
}

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

set.seed(2345)

MC.iter <- MC_iter

in.chems <- c(
  "98-86-2", "92-87-5", "92-52-4", "117-81-7", "133-06-2", "532-27-4",
  "133-90-4", "57-74-9", "510-15-6", "94-75-7", "64-67-5", "132-64-9",
  "106-46-7", "111-44-4", "79-44-7", "131-11-3", "77-78-1", "119-90-4",
  "121-14-2", "534-52-1", "51-28-5", "121-69-7", "107-21-1", "51-79-6",
  "76-44-8", "822-06-0", "77-47-4", "123-31-9", "72-43-5", "101-77-9",
  "56-38-2", "82-68-8", "87-86-5", "1120-71-4", "114-26-1", "91-22-5",
  "96-09-3", "95-80-7", "584-84-9", "95-95-4", "1582-09-8"
)[1:n_chem]

# Run lines 34-49, 53-106

all.equal(css.list, css_list)

################################################################################
################################################################################
# 04-CYP1A1-Pipeline.R
# Sample Css data
################################################################################
################################################################################

rm(list = ls())

set.seed(2345)

MC_iter <- 10

##########
# Data
load("~/dev/GeoTox/data/age_by_county_20220228.RData")
load("~/dev/GeoTox/data/obesity_by_county_20220228.RData")
load("~/dev/GeoTox/data/httk_css_pre_simulate_20220201.RData")

# age.by.county <- lapply(age.by.county[1:2], \(x) x[1:MC_iter])
# obesity.by.county <- lapply(obesity.by.county[1:2], \(x) x[1:MC_iter])
age.by.county <- lapply(age.by.county, \(x) x[1:MC_iter])
obesity.by.county <- lapply(obesity.by.county, \(x) x[1:MC_iter])

########################################

age_weight <- cross_join(
  tibble(
    age_group = list(
      c(0, 2), c(3, 5), c(6, 10), c(11, 15), c(16, 20), c(21, 30),
      c(31, 40), c(41, 50), c(51, 60), c(61, 70), c(71, 79)
    )
  ),
  tibble(
    weight = c("Normal", "Obese")
  )
) %>%
  rowwise() %>%
  mutate(age_min = age_group[1]) %>%
  ungroup()

# Get column index values corresponding the age and weight
get_column_idx <- function(age, weight) {
  idx <- tail(which(age_weight$age_min <= age & age_weight$weight == weight), 1)
  ifelse(length(idx) == 1, idx, NA)
}

# Sample httk data using matched column_idx
css_by_county <- mapply(
  function(age, weight) {
    column_idx <- mapply(get_column_idx, round(age), weight)
    out <- matrix(NA, nrow = length(age), ncol = length(css.list))
    for (i in 1:length(css.list)) {
      for (j in 1:nrow(age_weight)) {
        idx <- which(column_idx == j)
        n <- length(idx)
        if (n > 0) {
          out[idx, i] <- sample(css.list[[i]][, j], n, replace = TRUE)
        }
      }
    }
    out
  },
  age.by.county,
  obesity.by.county,
  SIMPLIFY = FALSE
)

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

set.seed(2345)

MC.iter <- MC_iter

# Run lines 45-141

all.equal(css.by.county, css_by_county)

################################################################################
################################################################################
# 05-CYP1A1-Pipeline.R
# Compute hazard quotient and efficacy under CA and IA assumptions
################################################################################
################################################################################

rm(list = ls())
load_all()

set.seed(2345)

MC_iter <- 10

##########
# Data
load("~/dev/GeoTox/data/css_by_county_20220228.RData")
load("~/dev/GeoTox/data/CYP1A1_by_county_20220901.RData")
load("~/dev/GeoTox/data/inhalation_dose_by_county_20220901.RData")

css.by.county <- lapply(css.by.county, \(x) x[1:MC_iter, ])
inhalation.dose.by.county <- lapply(inhalation.dose.by.county, \(x) x[1:MC_iter, ])

# Replace missing values with mean
for (i in 1:length(css.by.county)) {
  for (j in 1:ncol(css.by.county[[i]])) {
    idx <- is.na(css.by.county[[i]][, j])
    if (any(idx)) {
      css.by.county[[i]][idx, j] <- mean(css.by.county[[i]][!idx, j])
    }
  }
}

########################################
# in vitro concentration
invitro_concentration <- mapply(
  calc_invitro_concentration,
  D_int = inhalation.dose.by.county,
  C_ss = css.by.county,
  SIMPLIFY = FALSE
)

# Summarize by county
invitro_summary <- enframe(
  lapply(invitro_concentration, function(x) {
    data.frame(
      Var2 = 1:ncol(x),
      internal_median = apply(x, 2, median, na.rm = TRUE)
    )
  }),
  name = "L1"
) %>% unnest(value)

########################################
# concentration response
concentration_response <- calc_concentration_response(
  C_invitro = invitro_concentration,
  hill_params = cyp1a1_up.by.county[[1]],
  tp_b_mult = 1.2,
  fixed = FALSE
)

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

library(reshape2)
library(truncnorm)
source("~/github/GeoToxMIE/helper_functions/GCA-obj.R")
source("~/github/GeoToxMIE/helper_functions/tcplHillConc_v2.R")
source("~/github/GeoToxMIE/helper_functions/IA-Pred.R")
source("~/github/GeoToxMIE/helper_functions/tcplHillVal_v2.R")
source("~/github/GeoToxMIE/helper_functions/ECmix-obj.R")

set.seed(2345)

MC.iter <- MC_iter

# Run lines 52-62

all.equal(invitro.conc.by.county, invitro_concentration)
all.equal(internal.summary %>% ungroup(), invitro_summary)

# Run lines 70-167

all.equal(
  final.response.by.county,
  lapply(concentration_response, \(x) {
    setNames(x[, c(1, 2, 4)], c("GCA", "IA", "HQ.10"))
  })
)

################################################################################
################################################################################
# 06-CYP1A1-Pipeline.R
################################################################################
################################################################################

library(sf)
library(ggpubr)

rm(list = ls())

##########
# Data
load("~/dev/GeoTox/data/final_response_by_county_20220901.RData")
load("~/dev/GeoTox/data/FIPS_by_county.RData") # FIPS shouldn't be needed
county_2014 <- st_read(
  "~/dev/GeoTox/data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp",
  quiet = TRUE
)

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

county <- county_2014 %>%
  filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))) %>%
  mutate(FIPS = str_c(STATEFP, COUNTYFP), .before = 1)

########################################
# response_by_county

# TODO list should already have names() == FIPS
# Make sure leading 0s are there for joining with county
names(final.response.by.county) <- sprintf("%05d", FIPS)

response_by_county <- tibble(
  FIPS = names(final.response.by.county),
  data = final.response.by.county
) %>%
  unnest(cols = data) %>%
  pivot_longer(-FIPS, names_to = "health_measure") %>%
  mutate(
    health_measure = factor(
      health_measure,
      levels = c("GCA", "IA", "HQ.10")
    )
  )

########################################
# IVIVE summary

# Summarize within counties
ivive_summary_df <- response_by_county %>%
  summarize(
    median = median(value , na.rm = TRUE),
    x95_quantile = quantile(value, 0.95, na.rm = TRUE, names = FALSE),
    x5_quantile = quantile(value, 0.05, na.rm = TRUE, names = FALSE),
    .by = c(FIPS, health_measure)
  )

ivive_summary_df_stack <- ivive_summary_df %>%
  pivot_longer(
    cols = c(median, x95_quantile, x5_quantile),
    names_to = "variable"
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c("x5_quantile", "median", "x95_quantile")
    )
  )

# Summarize across counties
ivive_summary_stats <- ivive_summary_df_stack %>%
  summarize(
    min = min(value),
    median = median(value),
    max = max(value),
    .by = c(variable, health_measure))

########################################
# Health measure histogram

hist_HM <- ggplot(ivive_summary_df_stack, aes(x = log10(value))) +
  geom_histogram(bins = 500) +
  facet_grid(
    health_measure ~ variable,
    labeller = labeller(
      health_measure = c(
        "GCA"   = "CA Response",
        "IA"    = "IA Response",
        "HQ.10" = "RQ"
      ),
      variable = c(
        "x5_quantile"  = "5th Percentile",
        "median"       = "Median",
        "x95_quantile" = "95th Percentile"
      )
    )
  ) +
  theme_minimal() +
  ylab("Count") +
  xlab("Log10 Risk Metric Value")

########################################
# County heatmaps

ivive_county_sf <- ivive_summary_df_stack %>%
  left_join(county, by = join_by(FIPS), keep = FALSE) %>%
  st_as_sf() %>%
  st_zm() # Was having errors until adding this

make_county_heatmap <- function(df, legend_name) {
  ggplot(df, aes(fill = value)) +
    # Plot fill, hide county borders
    # geom_sf(lwd = 0) + # This still showed up in .pdf images
    geom_sf(color = NA) +
    # Add state borders
    geom_sf(data = states, fill = NA, size = 0.15) +
    # Create separate plots for each variable
    facet_wrap(
      ~variable,
      ncol = 3,
      labeller = labeller(
        variable = c(
          "x5_quantile" = "5th Percentile",
          "median" = "Median",
          "x95_quantile" = "95th Percentile"
        )
      )
    ) +
    # Add fill scale
    scale_fill_viridis_c(
      name = legend_name,
      direction = -1,
      option = "A",
      trans = "sqrt"
    ) +
    # Theme
    theme_bw() +
    theme(
      text = element_text(size = 12),
      legend.text = element_text(size = 8)
    )
}

GCA_Eff_plot <- make_county_heatmap(
  ivive_county_sf %>% filter(health_measure == "GCA"),
  paste("Predicted Response", "Log2 Fold Change", "mRNA Expression",sep = "\n")
)

IA_Eff_plot <- make_county_heatmap(
  ivive_county_sf %>% filter(health_measure == "IA"),
  paste("Predicted Response", "Log2 Fold Change", "mRNA Expression",sep = "\n")
)

HQ_10_plot <- make_county_heatmap(
  ivive_county_sf %>% filter(health_measure == "HQ.10"),
  "Risk Quotient"
)

all_plots <- ggarrange(
  GCA_Eff_plot , IA_Eff_plot, HQ_10_plot,
  labels = c( "A", "B", "C"),
  vjust = 1,
  align = "v",
  nrow = 3,
  font.label = list(size = 20, color = "black", face = "bold"),
  common.legend = FALSE
)

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

library(reshape2)

load("~/dev/GeoTox/data/final_response_by_county_20220901.RData")

# Run 25-27, 35-80, 85-97

all.equal(
  response.by.county %>%
    select(health_measure, value),
  response_by_county %>%
    arrange(as.numeric(FIPS), health_measure) %>%
    select(health_measure, value),
  check.attributes = FALSE
)

all.equal(
  summary_stats,
  ivive_summary_stats %>%
    arrange(variable, health_measure) %>%
    select(variable, health_measure, median, min, max),
  check.attributes = FALSE
)

pdf("~/dev/GeoTox/outputs/cyp1a1_hist_orig.pdf")
histogram_HM
invisible(dev.off())

pdf("~/dev/GeoTox/outputs/cyp1a1_hist_new.pdf")
hist_HM
invisible(dev.off())

# Run lines 107-108

all.equal(
  ivive_county_cyp1a1_up_sf %>%
    arrange(STATEFP, COUNTYFP, health_measure, variable) %>%
    pull(value),
  ivive_county_sf %>%
    arrange(STATEFP, COUNTYFP, health_measure, variable) %>%
    pull(value)
)

# Needed to add st_zm(), otherwise plot would have error
ivive_county_cyp1a1_up_sf <- ivive_county_cyp1a1_up_sf %>% st_zm()

# Run lines 110-112, 130, 157, 170

ggsave(
  "~/dev/GeoTox/outputs/cyp1a1_heatmap_orig.tiff",
  composite_plot, width = 40, height = 25, units = "cm", dpi = 300
)

ggsave(
  "~/dev/GeoTox/outputs/cyp1a1_heatmap_new.tiff",
  all_plots, width = 40, height = 25, units = "cm", dpi = 300
)

pdf("~/dev/GeoTox/outputs/cyp1a1_heatmap_new.pdf", width = 15.75, height = 10)
all_plots
invisible(dev.off())
