library(devtools)
library(tidyverse)

################################################################################
################################################################################
# 01-Conc-Response-Fit.R
#   GeoToxMIE 3-param vs TCPL 3-param
#   GeoToxMIE 3-param vs GeoToxMIE 2-param
################################################################################
################################################################################

rm(list = ls())
load_all()

# Load data
load("~/dev/GeoTox/data/LTEA_HepaRG_CYP1A1_up 41 chems for Kyle 220131.RData")
ice_data <- cdat; rm(cdat)

# Split by chemical
ice_conc_resp <- split(as.data.frame(ice_data), ~casn)

# Fit first chemical
fit_3param <- fit_hill(
  ice_conc_resp[[1]]$logc,
  ice_conc_resp[[1]]$resp,
  fixed_slope = FALSE
)
fit_2param <- fit_hill(
  ice_conc_resp[[1]]$logc,
  ice_conc_resp[[1]]$resp
)

# Make plot
log10_x <- seq(-3, 3, length.out = 100)

par <- fit_3param$par
y_3param <- par["tp"] / (1 + 10^((par["logAC50"] - log10_x) * par["slope"]))

par <- fit_2param$par
y_2param <- par["tp"] / (1 + 10^(par["logAC50"] - log10_x))

df <- rbind(
  tibble(x = 10^log10_x, y = y_3param, fixed_slope = FALSE),
  tibble(x = 10^log10_x, y = y_2param, fixed_slope = TRUE)
)

ggplot(df, aes(x, y, color = fixed_slope)) +
  geom_line(show.legend = FALSE) +
  scale_x_log10(labels = scales::label_math(10^.x, format = log10))

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

source("~/github/GeoToxMIE/helper_functions/tcpl_my_3hill_fit.R")
source("~/github/GeoToxMIE/helper_functions/tcpl_3hill_obj.R")
source("~/github/GeoToxMIE/helper_functions/tcpl_my_2hill_fit.R")
source("~/github/GeoToxMIE/helper_functions/tcpl_2hill_obj.R")

# county_cyp1a1_up
# Note: archived 20211109 has tcpl fit params, 20220201 has GeoToxMIE fit params
load("~/dev/GeoTox/data/archived_outputs/county_cyp1a1_up_20211109.RData")

tcpl <- county_cyp1a1_up %>%
  distinct(casrn, .keep_all = TRUE) %>%
  filter(casrn %in% names(ice_conc_resp))

# Run lines 10, 18, 28

all.equal(ice.chems.data %>% arrange(chemical), tcpl, check.attributes = FALSE)
all.equal(ice.data.by.chem, ice_conc_resp, check.attributes = FALSE)

# Run line 37

rbind(
  "tcpl" = tcpl %>%
    filter(casrn == names(ice_conc_resp)[1]) %>%
    select(hill_tp, hill_ga, hill_gw) %>%
    mutate_if(is.character, as.numeric),
  "GeoToxMIE" = my.3hill.mdl$par[1:3],
  "new" = fit_3param$par[1:3]
)

# Run line 48

all.equal(my.2hill.mdl, fit_2param)

rbind(
  "3-param" = my.3hill.mdl$par,
  "2-param" = c(my.2hill.mdl$par[1:2], 1, my.2hill.mdl$par[3]),
  "new"     = c(fit_2param$par[1:2], 1, fit_2param$par[3])
)

# Run lines 52-58

ggplot(df.plot) + geom_line(aes(X,threeHill),color = "red") +
  geom_line(aes(X,twoHill),color = "blue") +
  scale_x_log10(labels = scales::label_math(10^.x, format = log10))

################################################################################
################################################################################
# 02-Conc-Response-Fit.R
#   Compute 2-param fits
#   Some plotting
################################################################################
################################################################################

rm(list = ls())
load_all()

# Load data
load("~/dev/GeoTox/data/LTEA_HepaRG_CYP1A1_up 41 chems for Kyle 220131.RData")
ice_data <- cdat; rm(cdat)

# Split by chemical
ice_conc_resp <- split(as.data.frame(ice_data), ~casn)

# Fit 2-parameter Hill model
fits <- lapply(ice_conc_resp, function(df) {fit_hill(df$logc, df$resp)})

fit_params <- extract_hill_params(fits) %>%
  mutate(
    casn = unname(sapply(ice_conc_resp, function(x) x$casn[1])),
    chnm = unname(sapply(ice_conc_resp, function(x) x$chnm[1]))
  )

# saveRDS(fit_params, "~/dev/GeoTox/outputs/fit_params.rds")

# TODO is replacing NA sd with mean a good choice?
xylim <- range(with(fit_params, c(tp, tp.sd)), na.rm = T)
ggplot(fit_params, aes(tp, tp.sd)) +
  geom_abline(linetype = 3) +
  geom_point(aes(color = tp.sd.imputed), show.legend = FALSE) +
  coord_cartesian(xlim = xylim, ylim = xylim)

xylim <- range(with(fit_params, c(logAC50, logAC50.sd)), na.rm = T)
ggplot(fit_params, aes(logAC50, logAC50.sd)) +
  geom_abline(linetype = 3) +
  geom_point(aes(color = logAC50.sd.imputed), show.legend = FALSE) +
  coord_cartesian(xlim = xylim, ylim = xylim)

# Make plot
log10_x <- seq(-3, 3, length.out = 100)

y <- apply(fit_params, 1, function(par) {
  tp      <- as.numeric(par["tp"])
  logAC50 <- as.numeric(par["logAC50"])
  tp / (1 + 10^(logAC50 - log10_x))
})
colnames(y) <- names(ice_conc_resp)

df <- as_tibble(y) %>% mutate(x = 10^log10_x, .before = 1)

ggplot(df %>% pivot_longer(!x), aes(x, value, color = name)) +
  geom_line(show.legend = FALSE) +
  scale_x_log10(labels = scales::label_math(10^.x, format = log10))

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

source("~/github/GeoToxMIE/helper_functions/tcpl_my_2hill_fit.R")
source("~/github/GeoToxMIE/helper_functions/tcpl_2hill_obj.R")

# Run lines 9-12, 19-49

all.equal(
  df.params,
  fit_params %>% select(-ends_with("imputed"), -chnm),
  check.attributes = FALSE,
  tolerance = 1e-7
)

# Run lines 54-67

ggplot(df.plot,aes(X,value,color = variable)) +
  geom_line(show.legend = FALSE) +
  scale_x_log10(labels = scales::label_math(10^.x, format = log10))

# Compare individual chemical plots

i <- 1

as_tibble(y) %>%
  mutate(x = 10^log10_x, .before = 1) %>%
  select(x, y = !!fit_params$casn[i]) %>%
  ggplot(aes(x, y)) +
  geom_line(show.legend = FALSE) +
  geom_point(data = ice_conc_resp[[i]], aes(10^logc, resp)) +
  scale_x_log10(labels = scales::label_math(10^.x, format = log10)) +
  ggtitle(fit_params$chnm[i])

df <- data.frame("X" = X,"Y" = val.2hill[,i])
ggplot()+ geom_line(data = df,aes(X,Y))+
  geom_point(data = ice.data.by.chem[[i]],aes(10^logc,resp),size = 2,color ="red")+
  scale_x_log10(labels = scales::label_math(10^.x, format = log10))+
  ggtitle(ice.data.by.chem[[i]]$chnm[1])
