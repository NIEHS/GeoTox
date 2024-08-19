library(GeoTox)
library(tidyverse)
library(ggridges)
library(tigris)
library(sf)
# Load the geoTox object
geoTox <- readRDS("multi-run-0817.rds")


# Source calc_multi_response() and plot_multi() functions from the GeoTox package 

# Multi-Assay plots for manuscript
df10 <- calc_multi_response(geoTox, metric = "GCA.HQ.10", quant_total = 0.1, quant_assay = 0.5)
#df90 <- calc_multi_response(geoTox, metric = "GCA.HQ.10", quant_total = 0.9, quant_assay = 0.5)
g10 <- plot_multi(df10, title_label = "10th percentile of assay median responses")


ggsave("plots/multi-counties.pdf", g10, width = 6, height = 8, units = "in", device = cairo_pdf)



# Ashe County, NC (Lowest 10th percentile of assay median responses)
FIPS.Ashe <- "37009"
# Brunswick County, NC (Highest 10th percentile of assay median responses)
FIPS.Brunswick <- "37019"
# Wake County, NC (Median 10th percentile of assay median responses)
FIPS.Wake <- "37183"

FIPS.comb <- c(FIPS.Ashe, FIPS.Brunswick, FIPS.Wake)


# GeoTox() functionality that might be needed
# updating the GeoTox object 
# Filtering the entire GeoTox object by a specific region or other parameters defined within the object 



# Add the age, obesity data to the geoTox$resp 

# Filter the age and obesity data
ages <- geoTox$age[names(geoTox$age) %in% FIPS.comb]
obesity <- geoTox$obesity[names(geoTox$obesity) %in% FIPS.comb]

# convert ages list to a data frame and have the list names as a field called "FIPS"

# Convert the list to a data frame
df.age <- map_dfr(ages, ~ data.frame(value = .x, sample =  seq_along(.x)), .id = "FIPS")
df.obesity <- map_dfr(obesity, ~ data.frame(value = .x, sample =  seq_along(.x)), .id = "FIPS")

geoTox.resp <- geoTox$resp[names(geoTox$resp) %in% FIPS.comb] |> 
  map_dfr(~ data.frame(value = .x), .id = "FIPS") |> 
  rename_with(~ gsub("^value\\.", "", .x)) |>
  left_join(df.age, by = c("FIPS", "sample")) |> 
  left_join(df.obesity, by = c("FIPS", "sample")) |>
  rename(age = value.x, obesity = value.y) |>
  as_tibble()

fips_order <- geoTox.resp %>%
  distinct(FIPS, FIPS.comb) %>%
  arrange(factor(FIPS.comb, levels = c("low", "medium", "high"))) %>%
  pull(FIPS)


# Individual responses for single assay
g1 <- geoTox.resp |>
  filter(assay == "TOX21_H2AX_HTRF_CHO_Agonist_ratio") |>
#filter(assay == "ATG_PPARd_TRANS_up") |>
  mutate(FIPS = factor(FIPS, levels = fips_order)) |>
  ggplot(aes(x = GCA.HQ.10, y = FIPS, fill = FIPS, color = FIPS)) +
 # scale_color_viridis_d(option = "D", direction = -1) +
 geom_density_ridges(
  aes(point_shape = FIPS, point_fill = FIPS, point_color = FIPS),
  jittered_points = TRUE, 
  alpha = .2, point_alpha = 1, scale = 0.99
  ) + 
  facet_wrap(~obesity) +
  scale_x_log10() +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))+
    theme_minimal()


ggsave("plots/g1-TOX21_H2AX_HTRF_CHO_Agonist_ratio.pdf", g1, width = 6, height = 6, units = "in", device = cairo_pdf)

# Multi-individual responses for multiple assays

g2 <- geoTox.resp |>
  filter(FIPS %in% idx) |>
  dplyr::summarise(value = stats::quantile(.data$GCA.HQ.10, 0.5, na.rm = TRUE),
                   .by = c("FIPS", "sample", "obesity", "age")) |>
  # Reorder FIPS based on the order we created
  mutate(FIPS = factor(FIPS, levels = fips_order)) |>
  ggplot(aes(x = value, y = FIPS, fill = FIPS, color = FIPS)) +
  geom_density_ridges(
    aes(point_shape = FIPS, point_fill = FIPS, point_color = FIPS),
    jittered_points = TRUE,
    alpha = .2, 
    point_alpha = 1,
    scale = 0.99
  ) +
  facet_wrap(~obesity) +
  scale_x_log10() +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23)) +
  labs(x = "GCA.HQ.10", y = "FIPS") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(vjust = 0)
  ) +
  theme_minimal()


ggsave("plots/g2-multi-dev.pdf", g2, width = 6, height = 6, units = "in", device = cairo_pdf)


# County plots for figure

# Download North Carolina county shapefile
nc_counties <- counties(state = "NC", cb = TRUE, year = 2021)

# Filter for only the counties in FIPS.comb
selected_counties <- nc_counties %>%
  filter(GEOID %in% FIPS.comb) %>%
    mutate(GEOID = factor(GEOID, levels = fips_order))


# Create the plot
map_plot <- ggplot() +
  geom_sf(data = nc_counties, fill = "white", color = "gray") +
  geom_sf(data = selected_counties, aes(fill = GEOID), color = "black", alpha = 0.2) +
  scale_fill_manual(values = c("red", "green", "blue")) +  # Adjust colors as needed
  theme_minimal() +
  labs(title = "Selected Counties in North Carolina",
       fill = "FIPS Code") +
  theme(legend.position = "bottom")
ggsave("plots/nc_counties_map.pdf", map_plot, width = 10, height = 8, units = "in", device = cairo_pdf)


# Let's make a plot that shows the ridge density plots by assay and by KCC

CancerMOA <- read_xlsx("data-raw/CancerMOA.xlsx")


CancerMOA_unique <- CancerMOA %>%
  select(AssayEndpointName, ModeofAction) %>%
  mutate(ModeofAction = str_extract(ModeofAction, "KCC\\d+")) %>%
  distinct(AssayEndpointName, .keep_all = TRUE) %>%
  arrange(AssayEndpointName)

# join the CancerMOA data with the geoTox data

geoTox.MOA <- left_join(geoTox.resp, CancerMOA_unique, by = c("assay" = "AssayEndpointName"), relationship = "many-to-many") 


g3 <- geoTox.MOA |>
  filter(FIPS %in% idx) |>
  # Reorder FIPS based on the order we created
  mutate(FIPS = factor(FIPS, levels = fips_order)) |>
  # Reorder ModeofAction
  mutate(ModeofAction = fct_reorder(ModeofAction, 
                                    as.numeric(gsub("KCC", "", ModeofAction)), 
                                    .desc = FALSE)) |>
  ggplot(aes(x = GCA.HQ.10, y = FIPS, color = assay)) +
  geom_density_ridges(
    alpha = .2,
    rel_min_height = 0.01, scale = 1.0
  ) +
  facet_wrap(~ModeofAction) +
  scale_x_log10() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(vjust = 0)
  )

ggsave("plots/density-MOA.pdf", g3, width = 10, height = 8, units = "in", device = cairo_pdf)


