# # # # # # # # # # # # # # # # # # # #
#                                     #
# create forest_functions_helper.txt  #
#                                     #
# # # # # # # # # # # # # # # # # # # #
#
# Creation : file created 15.08.2023 by Marc Beringer inspired by the "create_27087_helper_from_additional_metadata.R" created by Noëlle Schenk
# Last edit : 25.02.2025 by Marc Beringer
# Aim : create the helper dataset "Forest_Functions_helper.csv" which simply is a subset 
#       of the metadata file (i.e. synthesis_forest_function_metadata_HIVE_local_copy.xlsx) 
#       to the upcoming forest functions synthesis dataset (not yet uploaded on BExIS).

### === working directory === ### 
# setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Synthesis projects/Synthesis_dataset_functions_forest_09-04-24-main")
setwd(...) # adjust to your own case
### ===== ###

### === libraries === ###
library("tidyverse")
library("xlsx")
### ===== ###

### === read metadata table === ###
# the input file is the metadata table of the upcoming forest functions synthesis dataset (not yet uploaded on BExIS).
# Please download the forest functions synthesis dataset metadata into your R working directory
forest_fun_metadata <- read.xlsx("synthesis_forest_function_metadata_homogen_FB_26.11.24.xlsx", 1)
# str(forest_fun_metadata)
# names(forest_fun_metadata)
### ===== ###

### === select the necessary columns and save output to working directory === ###
helper_dat <- forest_fun_metadata %>% subset(select = c("Function_Name_Year","Function_Name","codedYear","dataID","Dataset_Version"))
#str(helper_dat); names(helper_dat)

write.table(helper_dat, file = "forest_functions_helper_16.01.25.txt", quote = F, sep = "\t", row.names = F)
### ===== ###


