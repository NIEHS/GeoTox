library(devtools)
library(tidyverse)

################################################################################
################################################################################
# 01-CYP1A1-figures.R
################################################################################
################################################################################

library(sf)
library(ggpubr)

rm(list = ls())

##########
# Data

# TODO should the 112 be kept (i.e. "Benzidine 112")? It is in the MIE script
# fit_params is from step 2 of conc-resp.R
fit_params <- readRDS("~/dev/GeoTox/outputs/fit_params.rds") %>%
  mutate(
    chnm = case_when(
      chnm == "C.I. Disperse Black 6" ~ "3,3'-Dimethoxybenzidine",
      chnm == "C.I. Azoic Diazo Component 112" ~ "Benzidine",
      .default = chnm
    )
  )

nata_df <- read.csv("~/dev/GeoTox/data/2014_NATA_CONCS.csv")
nata_chemicals <- read.csv("~/dev/GeoTox/data/NATA_pollutant_names_casrn.csv")

# TODO different ice data source files?
# The "ice_data" in [01-02]-Conc-Response-Fit.R is from:
# load("~/dev/GeoTox/data/LTEA_HepaRG_CYP1A1_up 41 chems for Kyle 220131.RData")
load("~/dev/GeoTox/data/210105_ICE_cHTS_invitrodbv33.Rdata")
ice_data <- subset(ice_data, new_hitc == 1)

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

county_2014 <- st_read(
  "~/dev/GeoTox/data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp",
  quiet = TRUE
)

county <- county_2014 %>%
  filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))) %>%
  mutate(FIPS = str_c(STATEFP, COUNTYFP), .before = 1) %>%
  st_zm() %>%
  # TODO why simplify here but not in cyp1a1-pipeline?
  # "preserveTopology = FALSE" was dropped
  st_simplify(dTolerance = 1000)

nata_chemicals_2 <- nata_chemicals %>%
  mutate(
    web_name = str_to_title(web_name),
    web_name = str_replace(web_name, "\\s+\\([^\\)]+\\)", "")
  )

########################################
# Optional, the objects aren't used later on

# TODO same casrn but different names
x <- nata_chemicals_2 %>% distinct(casrn, web_name) %>% filter(!is.na(casrn))
(x <- x$casrn[duplicated(x$casrn)])
nata_chemicals_2 %>% filter(casrn %in% x)

nata_tox21_2 <- inner_join(
  ice_data,
  nata_chemicals_2,
  join_by(casn == casrn),
  relationship = "many-to-many" # due to duplicate casrn
)

nata_tox21_count_2 <- nata_tox21_2 %>%
  summarize(chemical_count = n(), .by = aenm) %>%
  rename("assay name" = aenm)

########################################
# Aggregate NATA data

norm_fn = function(x) {(x - min(x)) / (max(x) - min(x))}

nata_county_stack_2 <- nata_df %>%
  # Aggregate by county
  summarize(across(ACETALD:XYLENES, mean), .by = STCOFIPS) %>%
  pivot_longer(-STCOFIPS) %>%
  rename(FIPS = STCOFIPS, chemical = name, concentration = value) %>%
  # Add casrn and web_name
  left_join(nata_chemicals_2, by = join_by(chemical == smoke_name)) %>%
  # Limit to chemicals with fit data (MIE line 106)
  filter(casrn %in% fit_params$casn) %>%
  # Remove any missing data
  na.omit() %>%
  # Normalize air concentrations
  # This was very convoluted in the MIE script (MIE lines 89-95)
  group_by(chemical) %>%
  mutate(concentration_norm = norm_fn(concentration)) %>%
  ungroup()

nata_county_cyp1a1_up_2 <- nata_county_stack_2 %>%
  left_join(
    ice_data %>% filter(aenm == "LTEA_HepaRG_CYP1A1_up"),
    by = join_by(casrn == casn)
  ) %>%
  mutate(conc_gt_0 = as.integer(concentration > 0))

########################################
# heatmap_cyp1a1_up_plot

heatmap_df_2 <- nata_county_cyp1a1_up_2 %>%
  left_join(fit_params, by = join_by(casrn == casn, chnm)) %>%
  summarize(mean = mean(10^logAC50), .by = c(aenm, chnm))

heatmap_cyp1a1_up_plot <- ggplot(
  heatmap_df_2,
  aes(x = fct_reorder(chnm, mean), y = aenm,  fill = mean)
) +
  geom_tile() +
  theme_bw() +
  scale_fill_viridis_c(direction = 1, option = "A") +
  labs(y = "Assay", x = "Chemical", fill = "EC50 (μM)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title=element_text(size = 14),
    text = element_text(size = 18),
    aspect.ratio = 1.5 / 10,
    plot.margin = margin(10, 10, 10, 100)
  )

########################################
# cyp1a1_up_all_plot

cyp1a1_up_nata_county_sf_2 <- nata_county_cyp1a1_up_2 %>%
  left_join(fit_params, by = join_by(casrn == casn, chnm)) %>%
  right_join(county %>% mutate(FIPS = as.numeric(FIPS)), by = join_by(FIPS)) %>%
  st_as_sf()

cyp1a1_up_all_plot <- ggplot(
  cyp1a1_up_nata_county_sf_2,
  aes(fill = concentration_norm)
) +
  geom_sf(color = NA) +
  facet_wrap(vars(fct_reorder(chnm, logAC50)), ncol = 7) +
  # Recolor subset as light grey
  geom_sf(
    data = cyp1a1_up_nata_county_sf_2 %>% filter(concentration == 0),
    aes(fill = concentration),
    fill = "light grey",
    color = "light grey",
    lwd = 0.01
  ) +
  # State borders
  geom_sf(data = states, fill = NA, lwd = 0.15) +
  scale_fill_viridis_c(
    name = "Normalized Concentration",
    direction = -1,
    option = "A"
  ) +
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(angle = 45),
    legend.position = "bottom",
    strip.text = element_text(size = 10),
    text = element_text(size = 18)
  )

########################################
# cyp1a1_up_all_plot with relative concentration scales

cyp1a1_up_all_relative_plot <- lapply(
  unique(cyp1a1_up_nata_county_sf_2$chnm),
  function(x) cyp1a1_up_nata_county_sf_2 %>%
    filter(web_name == x) %>%
    ggplot(aes(fill = concentration)) +
    geom_sf(color = NA) +
    theme_bw() +
    geom_sf(data = states, fill = NA, size = 0.15) +
    scale_fill_viridis_c(
      name = "Conc. (ug/m3)",
      direction = -1,
      option = "A"
    ) +
    theme_bw() +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    ggtitle(x)
)

plot2_2 <- cowplot::plot_grid(
  plotlist = cyp1a1_up_all_relative_plot, label_size = 12, ncol = 8
)

pdf("~/dev/GeoTox/outputs/cyp1a1_figures_1_facet.pdf", width = 24, height = 20)
plot2_2
invisible(dev.off())

########################################
# count_cyp1a1_up_map

count_county_cyp1a1_up_sf_2 <- nata_county_cyp1a1_up_2 %>%
  summarize(count = sum(conc_gt_0), .by = FIPS) %>%
  right_join(county %>% mutate(FIPS = as.numeric(FIPS)), by = join_by(FIPS)) %>%
  st_as_sf()

count_cyp1a1_up_map_2 <- ggplot(
  count_county_cyp1a1_up_sf_2,
  aes(fill = count)
) +
  geom_sf(color = NA) +
  theme_bw() +
  geom_sf(data = states, fill = NA, size=0.15) +
  scale_fill_viridis_c(name = "# Chemicals", direction = -1, option = "A") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    text = element_text(size = 18),
    aspect.ratio = 5 / 10
  )

########################################
# combo

cyp1a1_combo <- ggarrange(
  cyp1a1_up_all_plot, count_cyp1a1_up_map_2, heatmap_cyp1a1_up_plot,
  labels = c("A", "B", "C"),
  widths = c(1, 0.65, 0.5),
  ncol = 1, nrow = 3,
  heights = c(1, 0.65, 0.5),
  align = "v",
  font.label = list(size = 20, color = "black", face = "bold"),
  common.legend = FALSE
)

pdf("~/dev/GeoTox/outputs/cyp1a1_figures_1.pdf", width = 17.7, height = 21.7)
cyp1a1_combo
invisible(dev.off())

ggsave(
  "~/dev/GeoTox/outputs/cyp1a1_figures_1.tiff",
  cyp1a1_combo, width = 45, height = 55, units = "cm", dpi = 300
)

# TODO failes to create plot
sjPlot::save_plot(
  "~/dev/GeoTox/outputs/cyp1a1_figures_1_sjPlot.tif",
  cyp1a1_combo, width = 45, height = 55, dpi = 300
)

#===============================================================================
# Compare to GeoToxMIE
#===============================================================================

library(reshape2)

load("~/dev/GeoTox/data/Hill_2param_model_fit.RData")
hill2.fit <- df.params; rm(df.params)

# Run lines 24-27

hill2.fit$chnm <- str_replace_all(hill2.fit$chnm, "Benzidine 112", "Benzidine")

all.equal(
  hill2.fit,
  fit_params %>% select(!ends_with("imputed")),
  check.attributes = FALSE
)

# Run lines 49, 56-57

# Optional run line 59-71, these objects are not used later

all.equal(
  nata_tox21   %>% arrange(casn, aeid, smoke_name) %>% select(-presence),
  nata_tox21_2 %>% arrange(casn, aeid, smoke_name),
  check.attributes = FALSE
)

all.equal(
  nata_tox21_count   %>% arrange(`assay name`),
  nata_tox21_count_2 %>% arrange(`assay name`)
)

# Run lines 76-106

all.equal(
  nata_county_stack   %>% arrange(FIPS, casrn),
  nata_county_stack_2 %>% arrange(FIPS, casrn),
  check.attributes = FALSE
)

# Run lines 109-125

all.equal(
  summary(heatmap_df$mean),
  summary(heatmap_df_2$mean)
)

# Skip remaining lines

# Compare
#
# "~/dev/GeoTox/outputs/cyp1a1_figures_1.pdf"
# vs
# "Figure3_v20220420.tif"

################################################################################
################################################################################
# 02-CYP1A1-figures.R
################################################################################
################################################################################

# TODO
