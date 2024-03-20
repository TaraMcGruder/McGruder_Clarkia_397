# Climate data extraction with Louisa

# From my own data exploration, I think I will be looking at CMD, Temp, and bFFP
# CMD and temp are both average/cumulative across months so we will look at the time
# that is important for Clarkia, the spring (March to May) and summer (June and July)

# Reading in CSV file with climate data
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

