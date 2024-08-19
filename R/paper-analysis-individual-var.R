library(GeoTox)
library(tidyverse)
library(ggridges)
library(gghalves)
# Load the geoTox object
geoTox <- readRDS("multi-run-0817.rds")


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


g1 <- geoTox.resp |>
  filter(assay == "TOX21_H2AX_HTRF_CHO_Agonist_ratio") |>
  ggplot(aes(x = GCA.HQ.10, y = FIPS, fill = FIPS, color = FIPS)) +
 # scale_color_viridis_d(option = "D", direction = -1) +
 geom_density_ridges(
  aes(point_shape = FIPS, point_fill = FIPS, point_color = FIPS),
  jittered_points = TRUE, 
  alpha = .2, point_alpha = 1, scale = 1.4
  ) +
  facet_wrap(~obesity) +
  scale_x_log10() +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

ggsave("plots/g1.png", g1, width = 10, height = 10, units = "in", dpi = 300)

# Multi-individual responses
idx <- names(geoTox$resp)[names(geoTox$resp) %in% FIPS.comb]
g2 <- geoTox.resp |> 
filter(FIPS %in% idx) |>
    dplyr::summarise(value = stats::quantile(.data$GCA.HQ.10, 0.5, na.rm = TRUE),
                    .by = c("FIPS","sample","obesity","age")) |>
 ggplot(aes(x = value, y = FIPS,  fill = FIPS, color = FIPS)) +
 # scale_color_viridis_d(option = "D", direction = -1) +
  geom_density_ridges(
  aes(point_shape = FIPS, point_fill = FIPS, point_color = FIPS),
  jittered_points = TRUE, 
  alpha = .2, point_alpha = 1, scale = 1.4
  ) +
  facet_wrap(~obesity) +
  scale_x_log10() +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

ggsave("plots/g2.png", g2, width = 10, height = 10, units = "in", dpi = 300)

# Multi-individual responses half-violin version
idx <- names(geoTox$resp)[names(geoTox$resp) %in% FIPS.comb]
geoTox.resp |> 
filter(FIPS %in% idx) |>
    dplyr::summarise(value = stats::quantile(.data$GCA.HQ.10, 0.5, na.rm = TRUE),
                    .by = c("FIPS","sample","obesity","age")) |>
  ggplot(aes(x = FIPS, y = value, color = obesity)) +
 # scale_color_viridis_d(option = "D", direction = -1) +
 geom_half_violin(
    side = "r",
  ) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~obesity, labeller = function(variable, value) {
    return(value)
  }) +
  theme_minimal() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "Obesity", y = "GCA.HQ.10") + 
  coord_flip()


  # Individual plots by age groups
age_group <- tibble(
  lower = c(0, 3, 6, 11, 16, 21, 31, 41, 51, 61, 71),
  upper = c(2, 5, 10, 15, 20, 30, 40, 50, 60, 70, 100)
)


geoTox.resp |> 
filter(FIPS %in% idx) |>
    dplyr::summarise(value = stats::quantile(.data$GCA.HQ.10, 0.5, na.rm = TRUE),
                    .by = c("FIPS","sample","obesity","age")) |>
  ggplot(aes(x = FIPS, y = value)) +
 # scale_color_viridis_d(option = "D", direction = -1) +
 geom_density_ridges(
    side = "r",
  ) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~obesity, labeller = function(variable, value) {
    return(value)
  }) +
  theme_minimal() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "Obesity", y = "GCA.HQ.10") + 
  coord_flip()
