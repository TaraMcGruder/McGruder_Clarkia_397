# Data prep
# Separating data prep steps from analyses

library(tidyverse)

# Read in floral color data
# flowersurvey <- read_csv("C:/Users/taram/Documents/Lab Analysis/McGruder_Clarkia_397/TaraClarkiaFloralData/flowersurvey.csv")
flowersurvey <- read_csv("TaraClarkiaFloralData/flowersurvey.csv")
# Should be possible to just use relative path here

summary(flowersurvey)

# Convert dates to date format 
flowersurvey$bud_emerg_date <- mdy(flowersurvey$bud_emerg_date)
flowersurvey$flwr_date <- mdy(flowersurvey$flwr_date)
flowersurvey$scnd_flwr_date <- mdy(flowersurvey$scnd_flwr_date)
flowersurvey$third_flwr_date <- mdy(flowersurvey$third_flwr_date)
flowersurvey$fourth_flwr_date <- mdy(flowersurvey$fourth_flwr_date)

flowersurvey$pollen_color_subj = as.factor(flowersurvey$pollen_color_subj)
flowersurvey$anther_color_subj = as.factor(flowersurvey$anther_color_subj)
flowersurvey$overall_petal_color_subj = as.factor(flowersurvey$overall_petal_color_subj)

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
  mutate(year_third_flwr = year(third_flwr_date),
         third_flwr_doy = yday(third_flwr_date) + ifelse(year_third_flwr == 2024, 365, 0)) %>%
  
  # rename tag_name as pop_fam so we know it is telling us the population and family
  rename(pop_fam = tag_name) %>%
  
  # mutate() to modify existing column and add a new one with just population
  # extracting words from a string, taking the first word, separated by "_"
  mutate(pop_name = word(pop_fam, 1, sep = "_")) %>%
  
  #removing  any plants that didn't germinate
  #also do not want plants that didn't have a first date of flowering
  filter(exclude == 0, !is.na(flwr_date)) %>% 
  
  # select() function chooses specific columns to keep in final data frame,
  # add a "-" to get rid of certain columns
  select(-c(
    # location,
            exclude,
            exclude2,
            notes,
            notes2,
            key),
         
         # getting rid of columns that contain the phrase "date"         
         -contains("date"),
         # getting rid of columns that contain "year"
         -contains("year")) %>%
  
  # create two new columns for the time between bud and first, and first and second flower 
  mutate(
    time_fst_flwr = flwr_doy - bud_doy,
    time_fst_scnd_flwr = scnd_flwr_doy - flwr_doy)

summary(flowersurvey_DOY)

  

# Climate data extraction with Louisa

# From my own data exploration, I think I will be looking at CMD, Temp, and bFFP
# CMD and temp are both average/cumulative across months so we will look at the time
# that is important for Clarkia, the spring (March to May) and summer (June and July)

# Reading in CSV file with climate data
# climate_data_1981_2022_ALL <- read.csv("C:/Users/taram/Documents/Lab Analysis/McGruder_Clarkia_397/TaraClarkiaFloralData/ClimateNA_pop_information_1981-2022MSY.csv")

climate_data_1981_2022_ALL <- read.csv("TaraClarkiaFloralData/ClimateNA_pop_information_1981-2022MSY.csv")


# first I am going to extract out all the by month data

# Reformat CSV file to pull out temperature across all months
ave_clim_monthly <- climate_data_1981_2022_ALL %>%
  #select necessary columns
  select(c("Year", "ID2", "Latitude", "Longitude", "Elevation", 
           Tave03:Tave07, PPT03:PPT07, CMD03:CMD07, bFFP)) %>%
  rename(pop_name = ID2) %>%
  group_by(pop_name) %>%
  # use across to run the same calculations for multiple variables
  summarise(across(Tave03:Tave07, 
                   list("ttl" = mean),
                   .names = "{.col}_{.fn}"),
            across(PPT03:PPT07, 
                   list("ttl" = mean),
                   .names = "{.col}_{.fn}"),
            across(CMD03:CMD07, 
                   list("ttl" = mean),
                   .names = "{.col}_{.fn}"),
            latitude = first(Latitude),
            longitude = first(Longitude),
            elevation = first(Elevation),
            bFFP = first(bFFP)
  ) %>%
  filter(!(pop_name %in% c("BBL", "GPS", "ttl_years"))) %>%
  pivot_longer(cols = Tave03_ttl:CMD07_ttl,
               names_to = "month",
               values_to = "climatic_var") %>%
  #adding a column for the part of the range the population is in
  mutate(range_position = ifelse(latitude < 44.34, "Southern",
                                 ifelse(latitude > 48.19, "Northern", "Central")))


# now going to derive the different climate variables

ave_clim_seasonal <- ave_clim_monthly %>%
  group_by(pop_name) %>%
  summarize(
    CMD_gs = sum(climatic_var[month %in% c("CMD03_ttl", "CMD04_ttl", "CMD05_ttl", "CMD06_ttl", "CMD07_ttl")]),
    CMD_sp = sum(climatic_var[month %in% c("CMD03_ttl", "CMD04_ttl", "CMD05_ttl")]),
    CMD_sm = sum(climatic_var[month %in% c("CMD06_ttl", "CMD07_ttl")]),
    Tave_gs = mean(climatic_var[month %in% c("Tave03_ttl", "Tave04_ttl", "Tave05_ttl", "Tave06_ttl", "Tave07_ttl")]),
    Tave_sp = mean(climatic_var[month %in% c("Tave03_ttl", "Tave04_ttl", "Tave05_ttl")]),
    Tave_sm = mean(climatic_var[month %in% c("Tave06_ttl", "Tave07_ttl")]),
    latitude = first(latitude),
    elevation = first(elevation),
    longitude = first(longitude),
    range_position = first(range_position),
    bFFP = first(bFFP)
  )

# to join with your data

#dataframe <- datafram %>%
#left join by the two columns that include the pick abbreviation in the two data frames
# this is if both dataframes have pop_name
#left_join(ave_clim_seasonal, by = c("pop_name" = "pop_name"))


#### ADDING CLIMATE AND GEOGRAPHIC DATA ####
## run code in the "climate_data.R" project first 
# merging dataframe with climate and geographic information
flowersurvey_DOY_clim <- flowersurvey_DOY %>%
  #left join by the two columns that include the pick abbreviation in the two data frames
  left_join(ave_clim_seasonal, by = c("pop_name" = "pop_name"))



# add on image J data ----
#load in the data

# ImageJ_Data <- read_csv("C:/Users/taram/Documents/Lab Analysis/McGruder_Clarkia_397/ImageJ_Data/ImageJ_Data.csv")

ImageJ_Data <- read_csv("ImageJ_Data/ImageJ_Data.csv")

#exclude notes column and measurements_done column and any other columns that measure basically the same thing
#new_df <- subset(df, char_var=="m" , select=c(keepvar1, keepvar2)
ImageJ_Data_Cleaned <- subset(ImageJ_Data, select = c("location", "tag_name", "flwr_number", "area", "perimeter", "circularity", "feret", "feret_angle", "min_feret", "aspect_ratio", "roundness", "solidity", "OL_width_left", "OL_width_right", "IL_width", "trough_length_left", "trough_length_right"))
summary(ImageJ_Data_Cleaned)
#remove all "NA" entries
ImageJ_Data_Cleaned_NAomit <- na.omit(ImageJ_Data_Cleaned)
#add column with just population (to make it easier to make graphs?)
ImageJ_Data_Cleaned_NAomit_Rename <- rename(ImageJ_Data_Cleaned_NAomit, pop_fam = tag_name)

#MERGE CLIMATIC DATA----
# Skipping this here and merging onto flower survey and climate data
#run climate_data.R dataframe before running these next lines 
# combine climatic data frame to ImageJ data frame
# ImageJ_Data_clim <- ImageJ_Data_Cleaned_NAomit_Rename %>%
  #left join by the two columns that include the pick abbreviation in the two data frames
  # left_join(ave_clim_seasonal, by = c("pop_name" = "pop_name"))

#VIZUALIZE THE DATA----
##area comparisons----
#make a separate column with only the population
#mutate() to modify existing column and add a new one with just population
#extracting words from a string, taking the first word, separated by "_"
# ImageJ_Data_Cleaned_NAomit_Rename <- ImageJ_Data_Cleaned_NAomit_Rename %>%
#   mutate(pop_name = stringr::word(pop_fam, 1, sep = "_"))

all_data = left_join(flowersurvey_DOY_clim, ImageJ_Data_Cleaned_NAomit_Rename)

summary(all_data)

write_csv(all_data, "combined_data.csv")

# Write out climte data alone
write_csv(ave_clim_seasonal, "seasonal_climate.csv")
