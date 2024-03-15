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
#VIZUALIZE THE DATA----
##area comparisons----
    #make a separate column with only the population
    #mutate() to modify existing column and add a new one with just population
    #extracting words from a string, taking the first word, separated by "_"
library(stringr)
ImageJ_Data_Cleaned_NAomit_Rename <- ImageJ_Data_Cleaned_NAomit_Rename %>%
  mutate(pop_name = stringr::word(pop_fam, 1, sep = "_"))

library(ggplot2)
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = area, group = pop_name)) +
  geom_boxplot(fill = "#4682b4") + 
  ylab("Area") +
  xlab("Population") +
  ggtitle("Area by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##perimeter comparisons----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = perimeter, group = pop_name, fill = pop_name)) + 
  geom_boxplot(fill = "#4682b4") +
  ylab("Perimeter") +
  xlab("Population") +
  ggtitle("Perimeter by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##circularity comparisons---- 
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = circularity, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Circularity") +
  xlab("Population") +
  ggtitle("Circularity by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##aspect ratio comparisons----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = aspect_ratio, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Aspect Ratio") +
  xlab("Population") +
  ggtitle("Aspect Ratio by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##roundness comparisons----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = roundness, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Roundness") +
  xlab("Population") +
  ggtitle("Roundness by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##solidity comparisons----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = solidity, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Solidity") +
  xlab("Population") +
  ggtitle("Solidity by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##inner lobe comparisons----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = IL_width, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Inner Lobe width") +
  xlab("Population") +
  ggtitle("Inner Lobe Width by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##outer lobe width comparisons----
###left----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = OL_width_left, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Left Outer Lobe Width") +
  xlab("Population") +
  ggtitle("Left Outer Lobe Width by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###right----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = OL_width_right, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Right Outer Lobe Width") +
  xlab("Population") +
  ggtitle("Right Outer Lobe Width by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##trough length comparisons---- 
###left----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = trough_length_left, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Left Trough Length") +
  xlab("Population") +
  ggtitle("Left Trough Length by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###right----
ggplot(data = ImageJ_Data_Cleaned_NAomit_Rename, aes(x = pop_name, y = trough_length_right, group = pop_name, fill = pop_name)) +
  geom_boxplot(fill = "#4682b4") +
  ylab("Right Trough Length") +
  xlab("Population") +
  ggtitle("Right Trough Length by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#MERGE CLIMATIC DATA----