# Tara's project: color variation in C. pulchella

# Load libraries
library(tidyverse)
library(sf) # for mapping
library(scatterpie) # for mapping
library(rnaturalearth) # for mapping
library(lme4) # for statistical models
library(lmerTest) # for statistical models
library(ggeffects) # for visualising model fits
library(cowplot) # for multipanel plots
library(vegan) # for shannon diversity

# Load data ----

# All data prep is combined in script 00_data_prep.R
flowers <- read_csv("combined_data.csv") %>% 
  # Make some binary columns for binomial models
  mutate(is_pollen_cream = if_else(pollen_color_subj == "cream", 1, 0),
         is_anther_cream = if_else(anther_color_subj == "cream", 1, 0),
         is_petal_lavender = if_else(overall_petal_color_subj == "lavender", 1, 0))
summary(flowers)

climate <- read_csv("seasonal_climate.csv")


# Reshape data ----

table(flowers$anther_color_subj)

# make a proportion

pollen_props <- flowers %>%
  # filter out rows in the pollen color with NA
  filter(!is.na(pollen_color_subj)) %>%
  #group by population and color, then calculate the count of unique colors for each group
  group_by(pop_name, pollen_color_subj) %>%
  summarise(count = n()) %>%
  #group by population, then calculate total count for each population
  group_by(pop_name) %>%
  mutate(total_count = sum(count),
         #calculate proportion of each color within each population
         proportion = count / total_count) %>% 
  select(-total_count) %>% 
  pivot_longer(names_to = "metric", cols = c(count, proportion), values_to = "value") %>% 
  unite(type, metric, pollen_color_subj) %>% 
  pivot_wider(names_from = type, names_prefix = "pollen_", values_from = value, values_fill = 0)  

pollen_prop_tall = pollen_props %>% 
  select(-contains("proportion")) %>% 
  pivot_longer(cols = contains("pollen_count"), 
               names_to = c(NA, NA, "color"), names_sep = "_", values_to = "count") %>% 
  group_by(pop_name) %>% 
  mutate(total_count = sum(count),
         proportion = count/total_count) %>% 
  left_join(., climate)

anther_props <- flowers %>%
  # filter out rows in the pollen color with NA
  filter(!is.na(anther_color_subj)) %>%
  #group by population and color, then calculate the count of unique colors for each group
  group_by(pop_name, anther_color_subj) %>%
  summarise(count = n()) %>%
  #group by population, then calculate total count for each population
  group_by(pop_name) %>%
  mutate(total_count = sum(count),
         #calculate proportion of each color within each population
         proportion = count / total_count) %>% 
  select(-total_count) %>% 
  pivot_longer(names_to = "metric", cols = c(count, proportion), values_to = "value") %>% 
  unite(type, metric, anther_color_subj) %>% 
  pivot_wider(names_from = type, names_prefix = "anther_", values_from = value, values_fill = 0)  

anther_prop_tall = anther_props %>% 
  select(-contains("proportion")) %>% 
  pivot_longer(cols = contains("anther_count"), 
               names_to = c(NA, NA, "color"), names_sep = "_", values_to = "count") %>% 
  group_by(pop_name) %>% 
  mutate(total_count = sum(count),
         proportion = count/total_count) %>% 
  left_join(., climate)

petal_props <- flowers %>%
  # filter out rows in the pollen color with NA
  filter(!is.na(overall_petal_color_subj)) %>%
  #group by population and color, then calculate the count of unique colors for each group
  group_by(pop_name, overall_petal_color_subj) %>%
  summarise(count = n()) %>%
  #group by population, then calculate total count for each population
  group_by(pop_name) %>%
  mutate(total_count = sum(count),
         #calculate proportion of each color within each population
         proportion = count / total_count) %>% 
  pivot_longer(names_to = "metric", cols = c(count, proportion), values_to = "value") %>% 
  unite(type, metric, overall_petal_color_subj) %>% 
  pivot_wider(names_from = type, names_prefix = "petal_", values_from = value, values_fill = 0)  

petal_prop_tall = petal_props %>% 
  select(-contains("proportion")) %>% 
  pivot_longer(cols = contains("petal_count"), 
               names_to = c(NA, NA, "color"), names_sep = "_", values_to = "count") %>% 
  group_by(pop_name) %>% 
  mutate(total_count = sum(count),
         proportion = count/total_count) %>% 
  left_join(., climate)

# Calculate Shannon diversity
diversity = data.frame(pop_name = petal_props$pop_name,
           shannon_petals =  diversity(x = petal_props[,c("petal_count_lavender", "petal_count_pink", "petal_count_purple")], 
                                index = 'shannon'),
           shannon_pollen =  diversity(x = pollen_props[,c("pollen_count_cream", "pollen_count_lavender", "pollen_count_pink", "pollen_count_purple")], 
                                       index = 'shannon'),
           shannon_anthers =  diversity(x = anther_props[,c("anther_count_cream", "anther_count_lavender", "anther_count_pink", "anther_count_purple")], 
                                       index = 'shannon'))



all_props = left_join(anther_props, pollen_props) %>% 
  left_join(., petal_props) %>% 
  left_join(., climate) %>% 
  left_join(., diversity)

summary(all_props)

rm(petal_props, anther_props, pollen_props)
  
# There are now six dataframes that should be able to do everything
# One is just climate and spatial data (climate)
# One is all the floral trait data, from both surveys and imagej (flowers)
# One is a wide dataframe with one row per population and proportions and counts of each color (all_props)
# Three are "tall" dataframes of color proportions for each floral part (x_prop_tall)


# 1. How is color variation distributed within vs. between populations? ----
# Colors might be fixed within populations, or may be polymorphic within populations. 
# This can just be presented visually

## Plot pollen variation ----
custom_pollen_colors <- c("#ffe4c4", "#c8a2c8", "#C71585", "#800080")
ggplot(pollen_prop_tall, aes(x = reorder(pop_name, latitude), y = proportion, fill = color)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Pollen Colors in Each Population",
       x = "Population (ordered by latitude)",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_pollen_colors) +
  theme(
    plot.background = element_rect(fill = "#242526"),
    text = element_text(color = "white"),  # Text color
    axis.text = element_text(color = "white"),  # Axis text color
    axis.title = element_text(color = "white", face = "bold"),  # Axis title color and bold
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Plot title color and styling
    panel.grid.major = element_blank(),  # Make major grid lines invisible
    panel.grid.minor = element_blank(),   # Make minor grid lines invisible
    legend.position = "bottom"  # Move legend to the bottom
  )

# Conclusions: cream is the most frequent pollen color, and color is generally polymorphic within populaitons

## Plot anther variation ----
ggplot(anther_prop_tall, aes(x = reorder(pop_name, latitude), y = proportion, fill = color)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Anther Colors in Each Population",
       x = "Population (ordered by latitude)",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_pollen_colors) +
  theme(
    plot.background = element_rect(fill = "#242526"),
    text = element_text(color = "white"),  # Text color
    axis.text = element_text(color = "white"),  # Axis text color
    axis.title = element_text(color = "white", face = "bold"),  # Axis title color and bold
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Plot title color and styling
    panel.grid.major = element_blank(),  # Make major grid lines invisible
    panel.grid.minor = element_blank(),   # Make minor grid lines invisible
    legend.position = "bottom"  # Move legend to the bottom
  )

# Conclusions: Again cream is very frequent, and color is variable within all populations


## Plot petal variation ----
custom_petal_colors <- c("#c8a2c8", "#C71585", "#800080")
ggplot(petal_prop_tall, aes(x = reorder(pop_name, latitude), y = proportion, fill = color)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Petal Colors in Each Population",
       x = "Population (ordered by latitude)",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_petal_colors) +
  theme(
    plot.background = element_rect(fill = "#242526"),
    text = element_text(color = "white"),  # Text color
    axis.text = element_text(color = "white"),  # Axis text color
    axis.title = element_text(color = "white", face = "bold"),  # Axis title color and bold
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Plot title color and styling
    panel.grid.major = element_blank(),  # Make major grid lines invisible
    panel.grid.minor = element_blank(),   # Make minor grid lines invisible
    legend.position = "bottom"  # Move legend to the bottom
  )

# Conclusions: Color is variable within populations, all three colors are fairly common, and there appears to be a latitudinal trend (can dig into that statistically later)


# 2. Do floral color traits vary across the range? ----

# It's interesting to look at how traits vary in space.
# Traits may vary spatially because of historic processes (e.g., patterns of range expansion and demographic process that affect genetic variation, and in turn, phenotypic variation)
# Traits may also vary spatially because of selective processes (e.g. if climate or other environmental variables vary in space and exert selection on flower color). We will explore this more later with climate data.

# How do we test whether traits vary across the range?
# Make maps
# Run regressions with latitude (our major range axis)


## Map color proportions ----

# Goals:
# - get state and province outlines
# - plot pies on top of map
# Would be nice if:
# - map projection was well suited to area
# - pies could have little lines pointing to the sites so they don't overlap
# - pies weren't distorted by map projection
# but these steps might require writing custom functions etc. 


### Make a single base map that you can use for each flower part ----
# To do this, I'm putting the map into an object called map_base (then I put map_base at the end of the map code so it'll print)

# option 1: use map_data, which is housed in ggplot
# pros: easy, has a aspect ratio based projection that works well with scatterpie
# cons: only has maps from the fairly limited maps package, so no province outlines

world <- map_data("world")
states <- map_data("state")

map_base <-
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "black", fill = "white") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = "white") +
  coord_sf(xlim = c(-125, -114), ylim = c(41, 53)); map_base

# option 2: source map outlines from the r natural earth database
# pros: has province lines, projection is easy because it's an sf object
# cons: lose the very simple aspect ratio projection that comes with option 1 and I can't figure out how to specify it. so when piecharts are overlaid, they're stretched

state_prov <- ne_states(c("united states of america", "canada"))

# this data comes in as an sf object so it can be plotted with a slightly different syntax
map_base <-
  ggplot() +
  geom_sf(data = state_prov, fill = "white", color = "black") +
  coord_sf(xlim = c(-125, -114), ylim = c(41, 51)); map_base

# option 2b:
# can reproject to a nice projection for that part of the world; there might be a better one than this

state_prov_proj <- st_transform(state_prov, 32610)

map_base <-
  ggplot() +
  geom_sf(data = state_prov_proj, fill = "white", color = "black") +
  coord_sf(xlim = c(-125, -114), ylim = c(41, 51), lims_method = "orthogonal", default_crs = 4326); map_base

# pick which base map you like best and then add pies

### Map pollen colors ----

pollen_map <- map_base + geom_scatterpie(data = all_props, 
                                         aes(x = longitude, y = latitude, group = pop_name),
                                         cols = c("pollen_proportion_cream", 
                                                  "pollen_proportion_lavender", 
                                                  "pollen_proportion_pink", 
                                                  "pollen_proportion_purple"), 
                                         pie_scale = 8, alpha = 0.8) +
  theme_bw() +
  ggtitle("Map of Pollen Colors") +
  scale_fill_manual(values = c("#ffe4c4", "#c8a2c8", "#C71585", "#800080"), 
                    name = "Color",
                    labels = c("cream", "lavender", "pink", "purple")); pollen_map

### Map anther colors ----

anther_map <- map_base + geom_scatterpie(data = all_props, 
                                         aes(x = longitude, y = latitude, group = pop_name),
                                         cols = c("anther_proportion_cream", 
                                                  "anther_proportion_lavender", 
                                                  "anther_proportion_pink", 
                                                  "anther_proportion_purple"), 
                                         pie_scale = 8, alpha = 0.8) +
  theme_bw() +
  ggtitle("Map of Anther Colors") +
  scale_fill_manual(values = c("#ffe4c4", "#c8a2c8", "#C71585", "#800080"), 
                    name = "Color",
                    labels = c("cream", "lavender", "pink", "purple")); anther_map

### Map petal colors ----
petal_map <- map_base + geom_scatterpie(data = all_props, 
                                         aes(x = longitude, y = latitude, group = pop_name),
                                         cols = c("petal_proportion_lavender", 
                                                  "petal_proportion_pink", 
                                                  "petal_proportion_purple"), 
                                         pie_scale = 8, alpha = 0.8) +
  theme_bw() +
  ggtitle("Map of Petal Colors") +
  scale_fill_manual(values = c("#c8a2c8", "#C71585", "#800080"), 
                    name = "Color",
                    labels = c("lavender", "pink", "purple")); petal_map

# Now, also plot trends in color over space
# And analyse trend in dominant color

## Spatial trends in pollen color ----

m2 <- glmer(is_pollen_cream ~ poly(latitude, 2) + (1|pop_name), data = flowers, family = binomial)
# added a random effect of population because I think we need this. 
# uncertainty is larger but quadratic effect of latitude is still significant

# m2 <- glm(cbind(pollen_count_cream, total_count - pollen_count_cream) ~ poly(latitude, 2), data = all_props, family = binomial)

summary(m2)

p2 <- (ggpredict(m2, terms = "latitude", type = "fixed")); plot(p2)

# Version with binomial fits for all colors
# ggplot() +
#   stat_smooth(data = pollen_prop_tall, aes(x = latitude, y = proportion, color = color),
#               method = "glm", method.args = list(family = binomial), formula = y ~ x + I(x^2), se)

pollen_lat_plot = ggplot() +
  geom_smooth(data = pollen_prop_tall, aes(x = latitude, y = proportion, color = color), method = "loess", span = 1, se = FALSE) +
  geom_line(data = p2, aes(x = x, y = predicted), linetype = "dashed", alpha = 0.6) +
  geom_ribbon(data = p2, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_color_manual(values = custom_pollen_colors) +
  labs(caption = "Solid lines are loess fits, dashed line \nand shaded area are binomial glm fit \nand CI for proportion of pollen that is \ncream.",
       x = "Latitude",
       y = "Proportion") +
  theme(plot.caption = element_text(hjust = 0)) +
  ylim(0, 1); pollen_lat_plot
  
# May want to adjust aesthetics for this to make colors stand out more

plot_grid(pollen_map, pollen_lat_plot)
  
## Spatial trends in anther color ----

m3 <- glmer(is_anther_cream ~ poly(latitude, 2) + (1|pop_name), data = flowers, family = binomial)
# added a random effect of population because I think we need this. 
# uncertainty is larger but quadratic effect of latitude is still significant

summary(m3)

p3 <- (ggpredict(m3, terms = "latitude[all]", type = "fixed")); plot(p3)

anther_lat_plot = ggplot() +
  geom_smooth(data = anther_prop_tall, aes(x = latitude, y = proportion, color = color), method = "loess", span = 1, se = FALSE) +
  geom_line(data = p3, aes(x = x, y = predicted), linetype = "dashed", alpha = 0.6) +
  geom_ribbon(data = p3, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_color_manual(values = custom_pollen_colors) +
  labs(caption = "Solid lines are loess fits, dashed line \nand shaded area are binomial glm fit \nand CI for proportion of anthers that are \ncream.",
       x = "Latitude",
       y = "Proportion") +
  theme(plot.caption = element_text(hjust = 0)) +
  ylim(-0.01, 1); anther_lat_plot

# May want to adjust aesthetics for this to make colors stand out more

plot_grid(anther_map, anther_lat_plot)


## Spatial trends in petal color ----

# m4 <- glmer(is_petal_lavender ~ poly(latitude, 2) + (1|pop_name), data = flowers, family = binomial)
# added a random effect of population because I think we need this. 
# uncertainty is larger but quadratic effect of latitude is still significant
# not converging for petals so gong back to old method

m4 <- glm(cbind(petal_count_lavender, total_count - petal_count_lavender) ~ poly(latitude, 2), data = all_props, family = binomial)

summary(m4)

p4 <- (ggpredict(m4, terms = "latitude[all]", type = "fixed")); plot(p4)


petal_lat_plot = ggplot() +
  geom_smooth(data = petal_prop_tall, aes(x = latitude, y = proportion, color = color), method = "loess", span = 1, se = FALSE) +
  geom_line(data = p4, aes(x = x, y = predicted), linetype = "dashed", alpha = 0.6) +
  geom_ribbon(data = p4, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_color_manual(values = custom_petal_colors) +
  labs(caption = "Solid lines are loess fits, dashed line \nand shaded area are binomial glm fit \nand CI for proportion of petals that are \nlavender.",
       x = "Latitude",
       y = "Proportion") +
  theme(plot.caption = element_text(hjust = 0)) +
  ylim(-0.01, 1); petal_lat_plot

# May want to adjust aesthetics for this to make colors stand out more

plot_grid(petal_map, petal_lat_plot)

## Spatial trends in color diversity ----

# Higher number = more color diversity
# These might pair nicely with the first set of stacked bar graphs showing color variation in each population
# Or with the maps and multi-line plots just above

m5 = lm(shannon_petals ~ poly(latitude, 2), data = all_props)
summary(m5)
plot(ggpredict(m5, terms = c("latitude")))

m6 = lm(shannon_anthers ~ poly(latitude, 2), data = all_props)
summary(m6)
plot(ggpredict(m6, terms = c("latitude")))

m7 = lm(shannon_pollen ~ poly(latitude, 2), data = all_props)
summary(m7)
plot(ggpredict(m7, terms = c("latitude")))



# STATISTICAL TESTS ----
#####Quadratic models ----


######pollen ----
#pollen_prop_cream <- pollen_color_prop %>%
#filter(pollen_color_subj == "cream")

pollen_color_prop_count_wide <- pollen_color_prop %>%
  pivot_wider(names_from = pollen_color_subj, values_from = count, values_fill = 0, id_cols = c(pop_name, total_count))

# combine pollen proportion with climate and geographic information for each population
pollen_prop_clim_geo_count <- pollen_color_prop_count_wide %>%
  left_join(ave_clim_seasonal, by = c("pop_name" = "pop_name"))

#plot


ggplot(pollen_prop_clim_geo) + 
  geom_point(aes(y = cream, x = latitude), color = "black") +
  geom_line(data = p2, aes(y = predicted, x = x), color = "black", size = 0.6) + # Adjust size as needed
  geom_ribbon(data = p2, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.2, fill = "#D7BBA0") +
  theme_bw() +  # Applying black and white theme
  labs(title = "Pollen Proportion vs. Latitude", x = "Latitude", y = "Pollen Proportion") +  # Adding axis titles
  theme(plot.title = element_text(face = "bold", hjust = 0.5),  # Making title bold and centered
        axis.title = element_text(face = "bold"))  # Making axis titles bold

######petals----
#petal quadratic model 
petal_color_prop_count_wide <- petal_color_prop %>%
  pivot_wider(names_from = overall_petal_color_subj, values_from = count, values_fill = 0, id_cols = c(pop_name, total_count))

# combine pollen proportion with climate and geographic information for each population
petal_prop_clim_geo_count <- petal_color_prop_count_wide %>%
  left_join(ave_clim_seasonal, by = c("pop_name" = "pop_name"))

#model
m3 <- glm(cbind(lavender, total_count - lavender) ~ poly(latitude, 2), data = petal_prop_clim_geo, family = binomial)
summary(m3)
p3 <- (ggpredict(m3, terms = "latitude"))
plot(ggpredict(m3, terms = "latitude"))

ggplot(petal_prop_clim_geo) + 
  geom_point(aes(y = lavender, x = latitude), color = "#301934") +
  geom_line(data = p3, aes(y = predicted, x = x), color = "#301934", size = 0.6) + # Adjust size as needed
  geom_ribbon(data = p3, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.2, fill = "#C3B1E1") +
  theme_bw() +  # Applying black and white theme
  labs(title = "Petal Proportion vs. Latitude", x = "Latitude", y = "Petal Proportion") +  # Adding axis titles
  theme(plot.title = element_text(face = "bold", hjust = 0.5),  # Making title bold and centered
        axis.title = element_text(face = "bold"))  # Making axis titles bold

#run this for climate predictors and tissue types (copy and paste this to your ImageJ_Data_Analysis.R project)
m4 <- glm(cbind(lavender, total_count - lavender) ~ Tave_sm, data = petal_prop_clim_geo_count, family = binomial)
summary(m4)
p4 <- (ggpredict(m4, terms = "latitude"))
plot(ggpredict(m4, terms = "Tave_sm"))

ggplot(pollen_prop_clim_geo) + 
  geom_point(aes(y = cream, x = latitude)) +
  geom_line(data = p2, aes(y = predicted, x = x)) +
  geom_ribbon(data = p2, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.2)





#multiple line plot 
#petals
petal_prop_long <- petal_prop_clim_geo %>%
  pivot_longer(cols = c (lavender, pink, purple), names_to = "color")

ggplot(petal_prop_long) + geom_smooth(aes(x = Tave_sm, y = value, color = color, fill = color), method = "glm", method.args = list(family = binomial), se = FALSE)




# 3. Do floral traits correlate with climate variables -----

# First, a quick look at how latitude correlates with climate variables, this could be useful later
plot(climate$latitude, climate$CMD_sp)
plot(climate$latitude, climate$CMD_sm)
plot(climate$latitude, climate$CMD_gs)
plot(climate$latitude, climate$Tave_gs)
plot(climate$latitude, climate$bFFP)

# 4. Are there floral "syndromes"? -----
# Do shape and size characteristics correlate with color or vary across populations? -----
# Also look at associations of color on different organs
# PCA
# Contingency tests
# Correlation of 





















#### MOSAIC PLOTS:----




#### WITHIN POP. VARIATION ----
# question: what is the within population variation in color depending on time

#flowersurvey_DOY
# overall_petal_color_subj: variable of interest, response variable
# pop_name: group stuff by
# third_flwr_doy: time

custom_colors <- c("#c8a2c8", "#C71585", "#800080")
ggplot(flowersurvey_DOY, aes(x = third_flwr_doy, y = reorder(pop_name, third_flwr_doy), color = overall_petal_color_subj)) +
  # getting rid of the grey behind
  theme_bw() +
  scale_color_manual(values = custom_colors) +
  geom_jitter()


library(ggplot2)
ggplot(data = ImageJ_Data_clim, aes(x = reorder(pop_name, CMD_sm), y = area, group = pop_name)) +
  geom_boxplot(fill = "#4682b4") + 
  ylab("Area") +
  xlab("Population") +
  ggtitle("Area by Population x CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

##perimeter comparisons----
ggplot(data = ImageJ_Data_clim, aes(x = reorder(pop_name, CMD_sm), y = perimeter, group = pop_name, fill = pop_name)) + 
  geom_boxplot(fill = "#4682b4") +
  ylab("Perimeter") +
  xlab("Population") +
  ggtitle("Perimeter X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
##circularity comparisons---- 
ggplot(data = ImageJ_Data_clim, aes(x = reorder(pop_name, latitude), y = circularity, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Circularity") +
  xlab("Population") +
  ggtitle("Circularity by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
##aspect ratio comparisons----
ggplot(data = ImageJ_Data_clim, aes(x = reorder (pop_name, CMD_sm), y = aspect_ratio, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Aspect Ratio") +
  xlab("Population") +
  ggtitle("Aspect Ratio X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
##roundness comparisons----
ggplot(data = ImageJ_Data_clim, aes(x = reorder (pop_name, CMD_sm), y = roundness, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Roundness") +
  xlab("Population") +
  ggtitle("Roundness X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
##solidity comparisons----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = solidity, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Solidity") +
  xlab("Population") +
  ggtitle("Solidity by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
##inner lobe comparisons----
ggplot(data = ImageJ_Data_clim, aes(x = reorder (pop_name, CMD_sm), y = IL_width, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Inner Lobe width") +
  xlab("Population") +
  ggtitle("Inner Lobe Width X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
##outer lobe width comparisons----
###left----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = OL_width_left, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Left Outer Lobe Width") +
  xlab("Population") +
  ggtitle("Left Outer Lobe Width by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
###right----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = OL_width_right, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Right Outer Lobe Width") +
  xlab("Population") +
  ggtitle("Right Outer Lobe Width by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()


#view plots side by side 
# Plotting left trough lengths
OL_left_plot <- ggplot(data = ImageJ_Data_clim, aes(x = reorder(pop_name, CMD_sm), y = OL_width_left, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Left Outer Lobe Width") +
  xlab("Population") +
  ggtitle("Left Outer Lobe Width X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(angle = 45, hjust = 1)) +  # Tilting x-axis title
  theme_bw()

# Plotting right trough lengths
OL_right_plot <- ggplot(data = ImageJ_Data_clim, aes(x = reorder(pop_name, CMD_sm), y = OL_width_right, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Right Outer Lobe Width") +
  xlab("Population") +
  ggtitle("Right Outer Lobe Width X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(angle = 45, hjust = 1)) +  # Tilting x-axis title
  theme_bw()

# Combine plots
combined_OL_plot <- grid.arrange(OL_left_plot, OL_right_plot, ncol = 2)







##trough length comparisons---- 
###left----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = trough_length_left, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Left Trough Length") +
  xlab("Population") +
  ggtitle("Left Trough Length by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
###right----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = trough_length_right, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Right Trough Length") +
  xlab("Population") +
  ggtitle("Right Trough Length by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()


#view plots side by side
library(ggplot2)
library(gridExtra)

# Plotting left trough lengths
left_trough_plot <- ggplot(data = ImageJ_Data_clim, aes(x = reorder(pop_name, CMD_sm), y = trough_length_left, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Left Trough Length") +
  xlab("Population") +
  ggtitle("Left Trough Length X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(angle = 45, hjust = 1)) +  # Tilting x-axis title
  theme_bw()

# Plotting right trough lengths
right_trough_plot <- ggplot(data = ImageJ_Data_clim, aes(x = reorder(pop_name, CMD_sm), y = trough_length_right, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Right Trough Length") +
  xlab("Population") +
  ggtitle("Right Trough Length X CMD_sm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(angle = 45, hjust = 1)) +  # Tilting x-axis title
  theme_bw()

# Combine plots
combined_trough_plot <- grid.arrange(left_trough_plot, right_trough_plot, ncol = 2)



# STATISTICAL TESTS ----






