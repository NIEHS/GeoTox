library(GeoTox)
library(tidyverse)
library(ggridges)
# Load the geoTox object
geoTox <- readRDS("vignettes/multi-run-0817.rds")


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


# Analysis Outline

# 3.a [calc] stratify by assay, age, obesity
# 3.b [calc] combined multi-assay response by age, obesity
# 4. [plot] response by county and facet by stratification

# Add the age, obesity data to the geoTox$resp 

# Lists of length 3
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


geoTox.resp |>
  filter(assay == "TOX21_H2AX_HTRF_CHO_Agonist_ratio") |>
  ggplot(aes(x = FIPS, y = GCA.HQ.10, color = FIPS)) +
  scale_color_viridis_d(option = "D", direction = -1) +
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


# Multi-individual responses
geoTox.ind <- geoTox[names(geoTox$resp) %in% FIPS.comb] |> 
  calc_multi_response(metric = "GCA.HQ.10", quant_assay = 0.5, quant_total = "individual")
