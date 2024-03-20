
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

