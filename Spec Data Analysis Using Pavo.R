#Loading the Pavo package 
install.packages("pavo")

#Getting the spec data in R 
library(pavo)
set.seed(1612217)
getspec("C:/Users/taram/OneDrive/Documents/Step One/Clarkia_Spec_File_Upload_2/McGruder397_Clarkia_SpecFile_Upload", ext = "txt", lim = c (250, 1000), decimal = ".", sep = ",", subdir = FALSE, subdir.names = FALSE, ignore.case = TRUE)

#Okay, now I want to form columns for all the data to make everything easy to see and relevant variables clear
library(tidyverse) # stringr and other useful packages are included in the tidyverse superpackage

list_of_files <- list.files(path = "C:/Users/taram/OneDrive/Documents/Step One/Clarkia_Spec_File_Upload_2/McGruder397_Clarkia_SpecFile_Upload", 
                            # path to the folder where spec images live
                            # recursive = TRUE, # optional, if your files are in 
                            # subfolders this will also look in those subfolders
                            pattern = "\\.txt$", 
                            # this says list all the .txt files, so it assumes you don’t have 
                            # .txt files that are not spec data
                            full.names = TRUE) # may want to make this false? 
                            # If true will keep the entire path in the name i think

df <- read_delim(list_of_files, id = "file_name", skip = 14, col_names = c("wavelength", "intensity")) %>% 
  mutate(file_name = str_remove(file_name, "C:/Users/taram/OneDrive/Documents/Step One/Clarkia_Spec_File_Upload_2/McGruder397_Clarkia_SpecFile_Upload")) %>% # may need to change
  separate(file_name, sep = ", ", into = c("id1", "id2")) %>% 
  separate(id1, sep = "_", into = c("flower", "tissue", "position")) %>% 
  separate(id2, sep = 6, into = c("id")) # here is where you can play with regular expressions to get the info you want out. You can also use other str* commands, like str_split to split it into multiple columns, str_remove to get rid of specific text (i.e., “.txt”).

#Use `spec()` to retrieve the full column specification for this data.
spec()

summary(df)
table(df$flower)
sum(is.na(df$flower))

df_trimmed = df %>% 
  filter(wavelength > 230)

ggplot(data = df_trimmed) + 
  geom_line(aes(x = wavelength, y = intensity, color = id, linetype = tissue))
