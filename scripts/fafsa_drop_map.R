# fafsa_drop_map.R
# 2024-04-09

# load -------
library(tidyverse)
library(sf)
library(tigris)
library(viridis)
library(scales)

options(scipen = 999)

# image of the table in this post was converted to a csv using claude 3
# https://onedtech.philhillaa.com/p/visualizing-fafsa-fiasco-by-geography
fafsa_claude_raw <- read_csv("data/fafsa_drop_claude.csv")

# load state shapefiles
state_geo <- states(cb = TRUE) |> 
  # move ak, hi, and pr 
  shift_geometry() |> 
  # filter out other territories
  filter(!GEOID %in% c("60", "66", "69", "78"))

# clean --------

# clean fafsa data
fafsa_clean <- fafsa_claude_raw |>
  rename(state = State,
         apps_23 = `2023 Applications`,
         comp_23 = `2023 Completions`,
         apps_24 = `2024 Applications`,
         comp_24 = `2024 Completions`,
         comp_drop = `YoY Completion Drop`) |> 
  # convert completion drop column to numeric
  mutate(comp_drop = as.numeric(str_remove_all(comp_drop, "%"))/100,
         # calculate completion drop 
         comp_drop_calc = (comp_24 - comp_23) / comp_23,
         # check for differences in calculated vs original amount
         # as a double-check on claude's conversion of the table
         # all should be close to 0 (might be some around .4% b/c of rounding)
         comp_diff = abs(comp_drop_calc) - comp_drop) 

# join fafsa data to state shapes
fafsa_geo <- state_geo |> 
  left_join(fafsa_clean, by = c("STUSPS" = "state"))

# plot -----------

ggplot(fafsa_geo) +
  geom_sf(aes(fill = comp_drop_calc)) +
  scale_fill_viridis(option = "viridis",
                     direction = -1,
                     labels = label_percent()) +
  labs(fill = "YoY Change in\nFAFSA Completion",
       title = "FAFSA completion droppped 39% from March 2023 to March 2024",
       caption = "Reproduction of a chart orignially produced in the 'On Ed Tech' newsletter.\nhttps://onedtech.philhillaa.com/p/visualizing-fafsa-fiasco-by-geography") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0))

ggsave("figures/fafsa_drop.png", units = "in", width = 8, height = 6)
