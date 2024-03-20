# if you want and have the time, githappy is the step by step guide for starting
# github account


# Tasks for Tara:
# - read through code and make sure stuff makes sense
#  - when you look through the plots, add annotations/comments of what you are seeing

# - check for outliers, for any doy, should be above 300
    # for checking, do View(flowersurvey_DOY) in console
    # check for any doy <300 and any time diff (negative)


# - check the subjective color enteries for other spellings,
#       color/color or color + color
# - check for cell enteries in date columns that have "?"
# - then redownload google sheet as csv
    # delete the csv that is in the folder
    # replace the csv with the newly downloaded, rename it the same
# - rerun all the code and check to see if anything is funky


# - for the color proportion, check with chatgpt or ggplot2,
#     assign certain colors to each unique color in the stacked barplot

# - is there a way to check if anther and pollen color differ? binary with "if" statement?
#     - is there a way to quantify how they differ?

# - are there any statistical analyses that could be used?

# - is DOY the best method to use? can also explore the data for time between bud and first flower


#The following code is to start looking through Tara's data
#Information on the data:
# Includes information regarding population, reproductive timing, size @ flowering,
#   and subjective color of petal, anther, and pollen (overall pattern too)


# Clean and explore data from daily flower surveys
# don't know how much we will gain from this BUT a good way to get comfortable
# using R and solving certain data puzzles

# Install and load necessary packages
# my top packages are always dplyr, tidyr, and ggplot2 if I'm looking at data

# manipulate and analyze data frames
# library(dplyr)  

# reshape and tidy data for analysis
# library(tidyr)

# handling date and time data
# library(lubridate)

# visualizing data
# library(ggplot2)

# working with string operations, good for text extraction
# library(stringr)

# MB: can replace all of the packages above with library(tidyverse) which is a megapackage that contains all of these, assuming it installs fine for you
library(tidyverse)
library(sf)
library(scatterpie)
library(rnaturalearth)

#### CLEAN AND CREATE NEW DATA FRAME ####

# Now that you have finished the huge task of entering your data...
# Time to load the data using the relative path/directory on the computer

# Edited to use path relative to the R project (this way the same path should work on all computers)

flowersurvey <- read.csv("TaraClarkiaFloralData/flowersurvey.csv")

# first let's look at the data
summary(flowersurvey)
# there are some cells that include empty spaces instead of NAs
# replace all empty values in your flowersurvey data frame with NA. 
# also dates are read as characters right now, not dates

# Identify character and numeric columns (data contains both)
#for a character column
# returns a logical vector of TRUE for character column and FALSE for anything else
char_cols <- sapply(flowersurvey, is.character)
# same for numeric columns
num_cols <- sapply(flowersurvey, is.numeric)
# this will allow us to selectively applying functions or 
# operations to specific column types.

# Replace empty values in character columns with NA using mutate_if()
# With the dplyr package, use %>% (pipe) operator to manipulate the data 
# in a bunch of ways at the "same time"

flowersurvey <- flowersurvey %>%
  mutate_if(char_cols, ~if_else(. == "", NA_character_, .)) %>%
  mutate_if(num_cols, ~if_else(is.na(.), NA_real_, .))
#>>>>>>> d23ee8de28bf3e043ec6ed90d6135a8119afdc61

# You used "m/d/y" format, but we need to tell the program to read it as that
# instead of simply a random character variable

# mdy(): function to convert any date column to a Date object

flowersurvey$bud_emerg_date <- mdy(flowersurvey$bud_emerg_date)
flowersurvey$flwr_date <- mdy(flowersurvey$flwr_date)
flowersurvey$scnd_flwr_date <- mdy(flowersurvey$scnd_flwr_date)
flowersurvey$third_flwr_date <- mdy(flowersurvey$third_flwr_date)
flowersurvey$fourth_flwr_date <- mdy(flowersurvey$fourth_flwr_date)

# Now we should make a new data frame that has DOY instead of the date
# DOY is January 01 = 0
# Create a new data frame 'flowersurvey_DOY' based on 'flowersurvey', saying DOY
  # to signify that this new data frame includes dates as DOY
# Huge assumption here is that we are stating all the plants germinated at the
  # same time, we know they all germinated in two (?) week period.


# With the dplyr package, use %>% (pipe) operator to manipulate the data 
  # in a bunch of ways at the "same time"
#Problem with using DOY is it restarts each year so we want to distinguish btwn
  #2023 and 2024, when starting in 2024, DOY starts at 365 instead of 0
# To convert DOY to a continuous DOY variable,
  # create a column that includes the year and then "if" statements"


flowersurvey_DOY <- flowersurvey %>%
  # first make sure there are no random spaces before or after each string in a cell
  mutate(across(where(is.character), str_trim)) %>%

  # Create new columns 'year_bud' and 'bud_doy'
    # mutate(newcolumn = ANY FUNCTIONS WITH (old column)) function: create new columns based on existing columns
    # can derive new variables this way.
  # the year() function extracts the year from the date column.
  mutate(year_bud = year(bud_emerg_date),
         # The yday() function converts date to DOY
         bud_doy = yday(bud_emerg_date)
         # ifelse() function to add 365 to DOY if the year is 2024
         + ifelse(year_bud == 2024, 365, 0)) %>%
  
  # Create new columns 'year_flwr' and 'flwr_doy'
  mutate(year_flwr = year(flwr_date),
         flwr_doy = yday(flwr_date) + ifelse(year_flwr == 2024, 365, 0)) %>%
  
  # Create new columns 'year_scnd_flwr' and 'scnd_flwr_doy'
  mutate(year_scnd_flwr = year(scnd_flwr_date),
         scnd_flwr_doy = yday(scnd_flwr_date) + ifelse(year_scnd_flwr == 2024, 365, 0)) %>%
  
#commented this out for now, can bring it in later
  #mutate(year_third_flwr = year(third_flwr_date),
         #third_flwr_doy = yday(third_flwr_date) + ifelse(year_third_flwr == 2024, 365, 0)) %>%

  # rename tag_name as pop_fam so we know it is telling us the population and family
  rename(pop_fam = tag_name) %>%
  
  # mutate() to modify existing column and add a new one with just population
    #extracting words from a string, taking the first word, separated by "_"
  mutate(pop_name = word(pop_fam, 1, sep = "_")) %>%
 
#censoring out any plants that didn't germinate
  #also do not want plants that didn't have a first date of flowering
  filter(exclude == 0, !is.na(flwr_date)) %>% 

# select() function chooses specific columns to keep in final data frame,
  # add a "-" to get rid of certain columns
   select(-c(location,
             exclude,
             exclude.1,
            notes,
            X,
            key),

# getting rid of columns that contain the phrase "date"         
   -contains("date"),
  # getting rid of columns that contain "year"
    -contains("year")) %>%

# create two new columns for the time between bud and first, and first and second flower 
  mutate(
    time_fst_flwr = flwr_doy - bud_doy,
    time_fst_scnd_flwr = scnd_flwr_doy - flwr_doy)

# START VISUALIZING DATA ----
## Flowering Time ----
# Create a histogram of flowering time for each population
ggplot(data = flowersurvey_DOY) + 
  geom_histogram(aes(x = flwr_doy)) +
  facet_wrap(.~pop_name) +
  ylab("Frequency") +
  xlab("Days to flower")

summary(flowersurvey_DOY)


# plot day of flower by population

ggplot(flowersurvey_DOY, aes(x = flwr_doy, y = pop_name)) +
  # getting rid of the grey behind
  theme_bw() +
  geom_boxplot()

# plot height of first flower by population
ggplot(data = flowersurvey_DOY, aes(x = pop_name, y = ttl_stem_lngth_flwr)) +
  geom_point() +
  theme_bw()
# great news!! This is very similar to Louisa's data, lovely to see

# plot day to first flower from bud
ggplot(flowersurvey_DOY, aes(x = time_fst_flwr, y = pop_name)) +
  # getting rid of the grey behind
  theme_bw() +
  geom_boxplot()
# interesting data, most of the time it is 15-20 days after budding that the first flower will appear
# keep in mind that weekly census was done for budding

# COLOR PROPORTION DATA AND GRAPHING ----
# what about looking at color stuff, possibly look at the proportion of diff. colors
# in each population?

##### Pollen color ----
# make a proportion
pollen_color_prop <- flowersurvey_DOY %>%
  # filter out rows in the pollen color with NA or empty strings
  filter(!is.na(pollen_color_subj) & pollen_color_subj != "") %>%

    #let's get rid of any weird spacing and capitalization
  mutate(
    pollen_color_subj = str_trim(tolower(pollen_color_subj))
  ) %>%
  
  #group by population and color, then calculate the count of unique colors for each group
  group_by(pop_name, pollen_color_subj) %>%
  summarise(count = n()) %>%
  
  #group by population, then calculate total count for each population
  group_by(pop_name) %>%
  mutate(total_count = sum(count),
         #calculate proportion of each color within each population
         proportion = count / total_count)

#let's plot

# Assuming you have a vector of custom colors for each variable
custom_colors <- c("#ffe4c4", "#c8a2c8", "#C71585", "#800080", "#FFFFFF")
ggplot(pollen_color_prop, aes(x = pop_name, y = proportion, fill = pollen_color_subj)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Pollen Colors in Each Population",
       x = "Population",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
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




###### Mosaic map plot: pollen color ----
    #start with proportion data frame 
    # use the map pies function 

#vizualize how the pollen data is distributed to make sure there are no issues
# table(flowersurvey_DOY$pollen_color_subj)
# PetalvPollen <- xtabs(~pollen_color_subj+ overall_petal_color_subj, data = flowersurvey_DOY)
# mosaic_plot <- mosaic(PetalvPollen, gp = shading_max,
#                       split_vertical = TRUE,
#                       main = "PetalvPollen [pollen_color_subj] [overall_petal_color_subj]",
#                       margin = c(2, 2, 2, 2))
# problem: since I am using flowersurvey_DOY, R is not considering proportions of each variable across the populations which is what I want it to do. 
  #I need to find a way to use the previously made proportion dataframes instead (petal_color_prop and pollen_color_prop)
  # I think you need to make a new data frame that includes the proportion frames (together???) and then substitute that above to compare PROPORTIONS
  # also consider that you may want to make a mosaic that considers proportions per population, idk, think about

# map
## fix proportion data frames to be usable for a mosaic plot  
pollen_color_prop_wide <- pollen_color_prop %>%
  pivot_wider(names_from = pollen_color_subj, values_from = proportion, values_fill = 0, id_cols = pop_name)

#clean the lat long data info (select populations, columns to keep)

Lat_Long_Elev_Data <- read.csv("TaraClarkiaFloralData/Clarkia_seed_inventory - LatLongElev.csv")

#select the columns to keep 
Selected_Lat_Long_Elev_Data <- select(Lat_Long_Elev_Data, "abbr_site", "lat", "long", "elev_m")
#select populations to keep
Final_Lat_Long_Elev_Data <- Selected_Lat_Long_Elev_Data[Selected_Lat_Long_Elev_Data$abbr_site %in% c("CRE", "FUR", "GPN", "HES", "JCP", "KAB", "LIC", "LRR", "MEB", "OGP", "PMS", "STM", "SUC", "TIC"), ]
  #change the name of abbr_site to pop_name 
Final_Lat_Long_Elev_Data_PopName <- Final_Lat_Long_Elev_Data %>%
  rename(pop_name = abbr_site)
#merge LatLong data onto proportion 
merged_PollenProp_LatLongElev <- merge(Final_Lat_Long_Elev_Data_PopName, pollen_color_prop_wide, by = "pop_name")

# Map color proportions ----

# Goals:
# - get state and province outlines
# - plot pies on top of map
# Would be nice if:
# - map projection was well suited to area
# - pies could have little lines pointing to the sites so they don't overlap
# - pies weren't distorted by map projection
# but these steps might require writing custom functions etc. 



# Make a single base map that you can use for each flower part
# To do this, I'm putting the map into an object called base_map (then I put base_map at the end of the map code so it'll print)

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

# can reproject to a nice projection for that part of the world; there might be a better one than this

state_prov_proj <- st_transform(state_prov, 32610)

map_base <-
  ggplot() +
  geom_sf(data = state_prov_proj, fill = "white", color = "black") +
  coord_sf(xlim = c(-125, -114), ylim = c(41, 51), lims_method = "orthogonal", default_crs = 4326); map_base

# pick which base map you like best and then add pies

map_base + geom_scatterpie(data = merged_PollenProp_LatLongElev, 
                    aes(x = long, y = lat, group = pop_name),
                    cols = c("cream", "lavender", "pink", "purple"), 
                    # These are the names of the columns that will make up the pies, not the names of colors R should use 
                    pie_scale = 8, alpha = 0.8) +
  scale_fill_manual(values = c("#ffe4c4", "#c8a2c8", "#C71585", "#800080"))
# these are the colors to use, in the same order as the columns




##### Petal color ----
petal_color_prop <- flowersurvey_DOY %>%
  #filter out rows in the pollen color with NA or empty strings
  filter(!is.na(overall_petal_color_subj) & overall_petal_color_subj != "") %>%
  
  #let's get rid of any weird spacing and capitalization
  mutate(
    overall_petal_color_subj = str_trim(tolower(overall_petal_color_subj))
  ) %>%
  
  #group by population and color, then calculate the count of unique colors for each group
  group_by(pop_name, overall_petal_color_subj) %>%
  summarise(count = n()) %>%
  
  #group by population, then calculate total count for each population
  group_by(pop_name) %>%
  mutate(total_count = sum(count),
         #calculate proportion of each color within each population
         proportion = count / total_count)

#let's plot?
custom_colors <- c("#c8a2c8", "#C71585", "#800080")

ggplot(petal_color_prop, aes(x = pop_name, y = proportion, fill = overall_petal_color_subj)) +
  geom_bar(stat = "identity", position = "stack") + #creating a stacked bar plot
  labs(title = "Proportion of Petal Colors in Each Population",
       x = "Population",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(
    plot.background = element_rect(fill = "#242526"),
    text = element_text(color = "white"),  # Text color
    axis.text = element_text(color = "white"),  # Axis text color
    axis.title = element_text(color = "white", face = "bold"),  # Axis title color and bold
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Centered plot title
    panel.grid.major = element_blank(),  # Make major grid lines invisible
    panel.grid.minor = element_blank(),   # Make minor grid lines invisible
    legend.position = "bottom"  # Move legend to the bottom
  )

##### Anther color ----
anther_color_prop <- flowersurvey_DOY %>%
  #filter out rows in the pollen color with NA or empty strings
  filter(!is.na(anther_color_subj) & anther_color_subj != "") %>%
  
  #let's get rid of any weird spacing and capitalization
  mutate(
    anther_color_subj = str_trim(tolower(anther_color_subj))
  ) %>%
  
  #group by population and color, then calculate the count of unique colors for each group
  group_by(pop_name, anther_color_subj) %>%
  summarise(count = n()) %>%
  
  #group by population, then calculate total count for each population
  group_by(pop_name) %>%
  mutate(total_count = sum(count),
         #calculate proportion of each color within each population
         proportion = count / total_count)

#plot anther color distribution
custom_colors <- c("#ffe4c4", "#c8a2c8", "#C71585", "#800080")
ggplot(anther_color_prop, aes(x = pop_name, y = proportion, fill = anther_color_subj)) +
  geom_bar(stat = "identity", position = "stack") + #creating a stacked bar plot
  labs(title = "Proportion of Anther Colors in Each Population",
       x = "Population",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(
    plot.background = element_rect(fill = "#242526"),
    text = element_text(color = "white"),  # Text color
    axis.text = element_text(color = "white"),  # Axis text color
    axis.title = element_text(color = "white", face = "bold"),  # Axis title color and bold
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Plot title color, bold, and centered
    panel.grid.major = element_blank(),  # Make major grid lines invisible
    panel.grid.minor = element_blank(),   # Make minor grid lines invisible
    legend.position = "bottom"  # Move legend to the bottom
  )

#### VIEWING PLOTS ----
# I want to view the anthers and pollen color plots side by side
library(ggplot2)
library(gridExtra)

# Define custom colors
custom_colors <- c("#ffe4c4", "#c8a2c8", "#C71585", "#800080", "#FFFFFF")
# Pollen Proportion Plot 
Pollen_Proportion_Plot <- ggplot(pollen_color_prop, aes(x = pop_name, y = proportion, fill = pollen_color_subj)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Pollen Colors in Each Population",
       x = "Population",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(
    plot.background = element_rect(fill = "#242526"),
    text = element_text(color = "white"),  # Text color
    axis.text = element_text(color = "white"),  # Axis text color
    axis.title = element_text(color = "white", face = "bold"),  # Axis title color and bold
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Plot title color and styling
    panel.grid.major = element_blank(),  # Make major grid lines invisible
    panel.grid.minor = element_blank(),   # Make minor grid lines invisible
    legend.position = "bottom",  # Position legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(color = "white")  # Legend text color
  )

# Anther Proportion Plot
Anther_Proportion_Plot <- ggplot(anther_color_prop, aes(x = pop_name, y = proportion, fill = anther_color_subj)) +
  geom_bar(stat = "identity", position = "stack") + #creating a stacked bar plot
  labs(title = "Proportion of Anther Colors in Each Population",
       x = "Population",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(
    plot.background = element_rect(fill = "#242526"),
    text = element_text(color = "white"),  # Text color
    axis.text = element_text(color = "white"),  # Axis text color
    axis.title = element_text(color = "white", face = "bold"),  # Axis title color and bold
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Plot title color and styling
    panel.grid.major = element_blank(),  # Make major grid lines invisible
    panel.grid.minor = element_blank(),   # Make minor grid lines invisible
    legend.position = "bottom",  # Position legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(color = "white")  # Legend text color
  )

# Arrange plots side by side
grid.arrange(Pollen_Proportion_Plot, Anther_Proportion_Plot, ncol = 2)




#### CHECK FOR OUTLIERS ####
# see any outliers of weirdness, should do this for each variable
# anything below DOY 300 is wild because we started the experiment after that

#first let's check the day of first flower
selected_id <- flowersurvey_DOY$location[which(flowersurvey_DOY$flwr_doy < 300)]

# Print the result, this will print the location of weird outliers to recheck
print(selected_id)

#### ADDING CLIMATIC DATA ####

# CHECK FOR THE NEW FILE THAT LOUISA ADDED

# Reading in CSV file with climate data
climate_data_1981_2022_ALL <- read.csv("TaraClarkiaFloralData/ClimateNA_pop_information_1981-2022MSY.csv")

#Reformat CSV file to pull out combined Tave, PPT, CMD, and DD1040 across all months
ave_climate_data_1981_2022 <- climate_data_1981_2022_ALL %>%
  #select necessary columns
  select(c("Year", "ID2", "Latitude", "Longitude", "Elevation", "MAT", "MAP",
           "DD1040", "CMD")) %>%
  rename(pop_name = ID2) %>%
  group_by(pop_name) %>%
  summarize(
    ttl_years = n_distinct(Year),
    MAT_ave = mean(MAT),
    MAT_sd = sd(MAT),
    MAT_se = MAT_sd / sqrt(ttl_years - 1),
    MAP_ave = mean(MAP),
    MAP_sd = sd(MAP),
    MAP_se = MAP_sd / sqrt(ttl_years - 1),
    CMD_ave = mean(CMD),
    CMD_sd = sd(CMD),
    CMD_se = CMD_sd / sqrt(ttl_years - 1),
    DD1040_ave = mean(DD1040),
    DD1040_sd = sd(DD1040),
    DD1040_se = DD1040_sd / sqrt(ttl_years - 1),
    latitude = first(Latitude),
    longitude = first(Longitude),
    elevation = first(Elevation)
  ) %>%
  filter(!(pop_name %in% c("BBL", "GPS"))) %>%
  select(-ttl_years)



# combine climatic data frame to flower survey data frame
flowersurvey_clim <- flowersurvey_DOY %>%
  #left join by the two columns that include the pick abbreviation in the two data frames
  left_join(ave_climate_data_1981_2022, by = c("pop_name" = "pop_name"))

