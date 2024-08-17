library(GeoTox)
library(tidyverse)
library(ggridges)
# Load the geoTox object
geoTox1 <- readRDS("vignettes/multi-run-0814.rds")


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

geoTox.resp$GCA.Eff[is.na(geoTox.resp$GCA.Eff)] <- 0

ggplot(geoTox.resp, aes(GCA.Eff, color = assay)) +
  geom_density() +
  facet_wrap(~FIPS) +
  theme_minimal() + 
  theme(legend.position = "none")
