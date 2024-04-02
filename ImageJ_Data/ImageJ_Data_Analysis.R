#CLEAN DATASET AND CHOOSE VARIABLES----
#load in the data
ImageJ_Data <- read.csv("C:/Users/taram/Documents/Lab Analysis/McGruder_Clarkia_397/ImageJ_Data/ImageJ_Data.csv")
#exclude notes column and measurements_done column and any other columns that measure basically the same thing
#new_df <- subset(df, char_var=="m" , select=c(keepvar1, keepvar2)
ImageJ_Data_Cleaned <- subset(ImageJ_Data, select = c("location", "tag_name", "flwr_number", "area", "perimeter", "circularity", "feret", "feret_angle", "min_feret", "aspect_ratio", "roundness", "solidity", "OL_width_left", "OL_width_right", "IL_width", "trough_length_left", "trough_length_right"))
View(ImageJ_Data_Cleaned)
#remove all "NA" entries
ImageJ_Data_Cleaned_NAomit <- na.omit(ImageJ_Data_Cleaned)
#add column with just population (to make it easier to make graphs?)
install.packages("dplyr")
library(dplyr)
ImageJ_Data_Cleaned_NAomit_Rename <- rename(ImageJ_Data_Cleaned_NAomit, pop_fam = tag_name)

#MERGE CLIMATIC DATA----
#run climate_data.R dataframe before running these next lines 
# combine climatic data frame to ImageJ data frame
ImageJ_Data_clim <- ImageJ_Data_Cleaned_NAomit_Rename %>%
  #left join by the two columns that include the pick abbreviation in the two data frames
  left_join(ave_clim_seasonal, by = c("pop_name" = "pop_name"))
#VIZUALIZE THE DATA----
##area comparisons----
    #make a separate column with only the population
    #mutate() to modify existing column and add a new one with just population
    #extracting words from a string, taking the first word, separated by "_"
library(stringr)
ImageJ_Data_Cleaned_NAomit_Rename <- ImageJ_Data_Cleaned_NAomit_Rename %>%
  mutate(pop_name = stringr::word(pop_fam, 1, sep = "_"))

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





