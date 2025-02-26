
### === Forest functions synthesis dataset assembly === ###

# Author(s): Fabio Benedetti & Marc Beringer, Paul Armando Gilmour Rivas Luginbühl, Noëlle Schenk, Bruno Ximenes Pinho, Caterina Penone
# Date of last update: 26/02/2025
# Purpose:
#   - Assemble synthesis dataset for forest ecosystem functions 
#       - First, generate an empty dataframe with identifier columns (Plot ID etc.)
#       - Second, fill this dataframe with the content of the associated values from the BE datasets (wide format) and using "AggregatedColumnName"
#       - Third, calculate new variables from base variables where necessary (e.g., averages across years)
#       - Fourth, convert this wide version of the data frame to the long version using the "transform_to_long_format.R" script

### =========================================================================================================

# Libraries needed
library("tidyverse") 
library("data.table")

### === working directory === ### 
#setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Synthesis projects/Synthesis_dataset_functions_forest_09-04-24-main") 
# Adjust to own dir
setwd(...)
### ===== ###

### === save a snapshot of the R environment === ###
#includes information about versions and packages used
#can be updated if new package versions are installed and required for the script to work
#see rstuido.github.io/renv/ for more details
#renv::init()
#renv::snapshot()
### ===== ###

### =========================================================================================================
  
### === dependencies === ###
### === Calculate mini-multifunctionality compound variables from individual ecosystem functions (i.e. variables)
#' multidiv() function 
#' @author Eric Allan
#' @param x Input data frame or matrix. Rows are different diversities/functions for multifun/div calculation. 
#' Columns are replicates, e.g. plots.

source("multidiversity.R")

### ===== ###

### === useful functions === ###
### === Change Exploratories plot names without zeros to plot names with zeros
#' https://github.com/biodiversity-exploratories-synthesis/Synthesis_useful_functions/blob/main/BEplotZeros%20.R
#' @author Caterina Penone
#' @examples
#' #create a dataset with a plot name column
#' dat <- data.frame(Plot_name = c("AEG1", "AEG2", "HEW4", "SEG8", "SEW10"), Values=1:5)
#' dat <- BEplotZeros(dat, "Plot_name", plotnam = "Sorted_plot_name")
#' 
#' @export
BEplotZeros <- function(dat, column, plotnam="PlotSTD"){
      dat <- as.data.frame(dat)
      funz <- function(x) ifelse((nchar(as.character(x))==4), gsub("(.)$", "0\\1", x), as.character(x)) # eo funz
      dat[,plotnam] <- sapply(dat[,column],funz)
      return(dat)
} # eo FUN


### === path to data === ###
# Path to raw data on the server of the Institute of Plant Sciences at the University of Bern (Switzerland)
# pathtodata <- "/Volumes/planteco/PROJECTS/Exploratories Synthesis/Data/Forest_functions/dataset_creation/raw_data/"
pathtodata <- ...
### ===== ###

### === data assembly === ###
#create an empty dataframe with identifier columns (Plot, Plot_bexis, exploratory and habitat), which will be filled with relevant columns later
#Plot is the two-digit plot identifier (e.g. AEW01) that will be used for merging, while Plot_bexis is the classic identifier (e.g. AEW1)
BE_synthesis_identifier_dat  <- data.frame(Plot = c(paste("AEW", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                    paste("AEG", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                    paste("SEW", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                    paste("SEG", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                    paste("HEW", formatC(1:51, width = 2, flag = "0"), sep = ""), #HEW51 was established in 2017, after HEW02 was destroyed in 2016.
                                    paste("HEG", formatC(1:50, width = 2, flag = "0"), sep = "")),
                            Plot_bexis = c(paste("AEW", formatC(1:50), sep = ""),
                                    paste("AEG", formatC(1:50), sep = ""),
                                    paste("SEW", formatC(1:50), sep = ""),
                                    paste("SEG", formatC(1:50), sep = ""),
                                    paste("HEW", formatC(1:51), sep = ""), #HEW51 was established in 2017, after HEW02 was destroyed in 2016.
                                    paste("HEG", formatC(1:50), sep = "")),
                            exploratory = c(rep("ALB", 100), rep("SCH", 100), rep("HAI", 101))) %>% 
                    mutate( habitat = ifelse(grepl(pattern = "W", Plot), "forest", "grassland") ) # eo mutate

#filter for forest habitat
BE_synthesis_identifier_dat <- BE_synthesis_identifier_dat %>% subset(habitat == "forest")
### ===== ###


### =========================================================================================================

##### === Forest Functions Variables Assembly === #####

### === root_decomposition === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Schrumpf
#Dataset(s):                 16666_2_Dataset
#Relevant columns (unit):    Mass_loss_October_2012 (%)

# Read data
dat <- read.table(paste0(pathtodata, "Functions/16666_2_Dataset/16666_2_data.txt"), header = T, sep = ";")
# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_identifier_dat, dat[,c("Mass_loss_October_2012","Plot")], by = "Plot", all.x = T)
# Special treatment for the added columns
# Rename them
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Mass_loss_October_2012"] <- "Root_decomposition"
# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat$Root_decomposition))) # 16 NAs
### ===== ###



### === dung_removal === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Blüthgen
#Dataset(s):                 21206_3_Dataset
#Relevant columns (unit):    removal_g (g)

# Read data
dat <- read.table(paste0(pathtodata, "Functions/21206_3_Dataset/21206_3_data.txt"), header = T, sep = ";")
# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP", plotnam = "Plot")

# Special treatment of the removal_g column of the 21206_3_Dataset
# as in the synthesis dataset functions grassland, here we only use samples gathered in summer 2014 (i.e. June, July and August)
# since samples from May 2014 were not collected at all Plots and samples from April and July 2015 were only collected at 36 out of 300 Plots
# then we apply the same calculation as in the grassland dataset by first scaling each dungtype before averaging them per Plot
# so that each dungtype (e.g. Cow, Sheep etc.) across the 150 Plots has a mean of 0 and a standard deviation of 1

# Add the habitat column to enable selection of forest Plots only
dat.1 <- merge(dat, BE_synthesis_identifier_dat, by = "Plot", all.x = T)

dat.2 <- dat.1 %>% 
  #select summer 2014 samples in forest Plots
  subset(habitat == "forest" & (month == "June_2014" | month == "July_2014" | month == "August_2014")) %>% 
  #scale removal_g across the range of each dungtype individually, to make dungtypes comparable to each other (i.e. Cow comparable with Sheep)
  group_by(dungtype) %>% 
  mutate(scaled_removal_g = scale(removal_g, center = F, scale = T)) %>% 
  #then calculate the mean of the scaled_removal_g for each Plot
  group_by( Plot) %>% 
  summarise(Dung_removal = mean(scaled_removal_g))

# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.2[,c("Plot","Dung_removal")], by = "Plot", all.x = T)

# Count NAs in the added columns
#length(which(is.na(BE_synthesis_forest_dat$Dung_removal))) # 2 NAs


### === seed_depletion === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Blüthgen
#Dataset(s):                 24966_3_Dataset
#Relevant columns (unit):    seed_depletion

# Read data
dat <- read.table(paste0(pathtodata, "Functions/24966_3_Dataset/24966_3_data.txt"), header = T, sep = ";")
# Add two-digit plot names for merging with the BE_synthesis_forest_dat
# names(dat)
dat <- BEplotZeros(dat, "Plot", plotnam = "Plot")

# Special treatment for the added columns of the 24966_3_Dataset
# Average the subplot measurements of seed_depletion
dat.1 <- dat %>% 
  group_by(Plot) %>% 
  summarise(Seed_depletion = mean(seed_depletion, na.rm = T) )

# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("Seed_depletion","Plot")], by = "Plot", all.x = T)
# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat$Seed_depletion))) # 2 NAs



### === browsing_percentage === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Ammer
#Dataset(s):                 20347_2_Dataset
#Relevant columns (unit):    Bper (%)

# Read data
dat <- read.table(paste0(pathtodata, "Functions/20347_2_Dataset/20347_2_data.txt"), header = T, sep = ";")
# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")

# Special treatment for the added columns of the 20347_2_Dataset
# before merging dat to BE_synthesis_forest_dat, overall browsing_percentage has to be formatted
# Where total number of saplings (juvenile tree, like a 'teenage tree') was equal to zero, the browsing percentage is given as NA, as there were no saplings to measure the browsing percentage. 
# However, there might be browsing percentage = 0, which is different from NA, as the saplings of the subplot were not subject to herbivory.
# Only consider heightclass == "-1" and tsg == "all" 

# Therefore, subset the relevant rows, pivot_wider and rename columns.
dat.1 <- dat %>% 
  subset( (tsg == "all" & heightclass == -1)) %>% 
  pivot_wider( id_cols = c("Plot"), names_from = c(tsg), values_from = Bper) 

# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1, by = "Plot", all.x = T)
# Rename added columns for more context
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "all"] <- "Browsing_percentage"

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Browsing_percentage))) # 47 NAs



### === herbivore_leaf_damage === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Goßner
#Dataset(s):                 24806_2_Dataset
#Relevant columns (unit):    a_hole + a_edge (chewing damage; mm^2)
#                            a_scraping + a_sucking + a_scraping_sucking (scraping and sucking damage; mm^2)
#                            a_mines (mining damage; mm^2)
#                            a_galls + a_gallm_bottom + a_gallm_top + a_gallm_rolled (gall damage; mm^2)
#                            a_thrips (thrips damage; mm^2)

# Read herbivory and species abundance data for community weighted means
herbivory0 <- read.table(paste0(pathtodata, "Functions/24806_2_Dataset/24806_2_data.txt"), header = T, sep = ";")
for_plants_up <- fread("/Volumes/planteco/PROJECTS/Exploratories Synthesis/Data/Forest_diversity/raw_data/31405_5_Dataset/31405_5_data.csv")

# Special treatment for the columns of the 24806_2_Dataset:
# This is a large dataset with over 160'000 rows: 
# each row represents a single leaf, on which the area (mm^2) damaged by different classes of herbivores was measured
# up to 200 leaves per plant species and up to ten plant species per Plot
# to arrive at an overall measure for herbivory weighted by the species abundance the data is severely compressed

herbivory0 <- herbivory0 %>% 
  filter(system == "forest") %>%
  select(plotid_withzero,collection_year,pl_species,pl_ind_id,pl_lb_id,lf_id,lf_area,lf_area_corr,a_hole:a_thrips)

# Get total and percentage of leaf damage by all types of herbivory0
herbivory0$a_herb_total <- herbivory0$a_hole+herbivory0$a_edge+herbivory0$a_scraping+herbivory0$a_sucking+
                        herbivory0$a_mines+herbivory0$a_galls+herbivory0$a_gallm_bottom+
                        herbivory0$a_gallm_top+herbivory0$a_gallm_rolled+herbivory0$a_thrips

herbivory0$perc_herb <- (herbivory0$a_herb_total/herbivory0$lf_area_corr)*100

# Check if all total herbivory0 values are lower than the area of leaves
# which(herbivory0$lf_area < herbivory0$a_herb_total) # many
# which(herbivory0$lf_area_corr < herbivory0$a_herb_total) # only three leaves, when considering corrected leaf areas
# which(herbivory0$perc_herb >= 100) # error due to measurement. % cannot be > 100%

# Check those three leaves with leaf damage higher than leaf area
herbivory1 <- herbivory0 %>% filter(perc_herb < 100) # Remove those leaves above?
# Note: additionally over 10,000 rows excluded due to NAs

# Calculate average herbivory by "leaf bundle" and then by species/plot
sp_plot_avg_herb <- herbivory1 %>% 
  group_by(plotid_withzero, pl_species, pl_lb_id) %>%
  summarise(lb_avg_herb = mean(perc_herb)) %>%
  ungroup() %>%
  group_by(plotid_withzero, pl_species) %>%
  summarise(sp_avg_herb = mean(lb_avg_herb))
  
# Check
# summary(sp_plot_avg_herb)
rm(herbivory0, herbivory1); gc()


## Plant inventory ----

# Get sp cover per plot (shrubs and trees only)
cov_sp_plot_ST <- for_plants_up %>%
    filter(Cover > 0, Layer != "H", Useful_EPPlotID != "HEW02") %>%
    group_by(Useful_EPPlotID) %>%
    mutate(total_plot_cov = sum(Cover)) %>%
    ungroup() %>%
    group_by(Useful_EPPlotID, Species) %>%
    summarise(total_sp_cov = sum(Cover)) 
# Check  
# summary(cov_sp_plot_ST)

# Get sp cover per plot (herbs only)
cov_sp_plot_H <- for_plants_up %>%
    filter(Cover > 0, Layer == "H", Useful_EPPlotID != "HEW02") %>%
    group_by(Useful_EPPlotID) %>%
    mutate(total_plot_cov = sum(Cover)) %>%
    ungroup() %>%
    group_by(Useful_EPPlotID, Species) %>%
    summarise(total_sp_cov = sum(Cover)) 

# Get sp cover per plot (overall)
cov_sp_plot_all <- for_plants_up %>%
    filter(Cover > 0, Useful_EPPlotID != "HEW02") %>%
    group_by(Useful_EPPlotID) %>%
    mutate(total_plot_cov = sum(Cover)) %>%
    ungroup() %>%
    group_by(Useful_EPPlotID, Species) %>%
    summarise(total_sp_cov = sum(Cover)) 


## Analyses

# Check if all species in the plant inventory are in the herbivory dataset
names(sp_plot_avg_herb) <- c("plot", "species", "sp_avg_herb")
names(cov_sp_plot_ST) <- c("plot", "species", "cover")
cov_sp_plot_ST$species <- gsub("_", " ", cov_sp_plot_ST$species)
names(cov_sp_plot_H) <- c("plot", "species", "cover")
cov_sp_plot_H$species <- gsub("_", " ", cov_sp_plot_H$species)
names(cov_sp_plot_all) <- c("plot", "species", "cover")
cov_sp_plot_all$species <- gsub("_", " ", cov_sp_plot_all$species)

# Join plant cover data with herbivory data
data_ST <- cov_sp_plot_ST %>% left_join(sp_plot_avg_herb, by = c("plot","species"))
summary(data_ST) # 1459 out of 1921 species-plot combinations without herbivory information (= NA)
data_H <- cov_sp_plot_H %>% left_join(sp_plot_avg_herb, by = c("plot","species"))
summary(data_H) # 6062 out of 6675 species-plot combinations without herbivory information
data_all <- cov_sp_plot_all %>% left_join(sp_plot_avg_herb, by = c("plot","species"))
summary(data_all) # 7516 out of 8590 species-plot combinations without herbivory information


# Check coverage of plot total abundance by herbivory data----
coverage_ST <- data_ST %>%
  group_by(plot) %>%
  mutate(total_cov = sum(cover)) %>%
  ungroup() %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  summarise(coverage_ST = sum(cover)/total_cov) %>%
  unique()
# Check
# summary(coverage_ST)
# Good coverage overall - minimum 34.34% but 1st quantile = 91.31%!

coverage_H <- data_H %>%
  group_by(plot) %>%
  mutate(total_cov = sum(cover)) %>%
  ungroup() %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  summarise(coverage_H = sum(cover)/total_cov) %>%
  unique()
# Check
# summary(coverage_H)
# Relatively low coverage (1st qt = 25.72%)

coverage_all <- data_all %>%
  group_by(plot) %>%
  mutate(total_cov = sum(cover)) %>%
  ungroup() %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  summarise(coverage_all = sum(cover)/total_cov) %>%
  unique()


## Calculate CWMs

CWMs_ST <- data_ST %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  mutate(total_plot_cov = sum(cover)) %>%
  ungroup() %>%
  group_by(plot, species) %>%
  mutate(cover_herb = cover*sp_avg_herb) %>%
  ungroup() %>%
  group_by(plot) %>%
  summarise(CWM_herb_ST = sum(cover_herb)/total_plot_cov) %>%
  unique()

CWMs_H <- data_H %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  mutate(total_plot_cov = sum(cover)) %>%
  ungroup() %>%
  group_by(plot, species) %>%
  mutate(cover_herb = cover*sp_avg_herb) %>%
  ungroup() %>%
  group_by(plot) %>%
  summarise(CWM_herb_H = sum(cover_herb)/total_plot_cov) %>%
  unique()

CWMs_all <- data_all %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  mutate(total_plot_cov = sum(cover)) %>%
  ungroup() %>%
  group_by(plot, species) %>%
  mutate(cover_herb = cover*sp_avg_herb) %>%
  ungroup() %>%
  group_by(plot) %>%
  summarise(CWM_herb_all = sum(cover_herb)/total_plot_cov) %>%
  unique()

## Join information on coverage
CWMs_all <- left_join(CWMs_all, coverage_all)
CWMs_ST <- left_join(CWMs_ST, coverage_ST)
CWMs_H <- left_join(CWMs_H, coverage_H)

## Filter out communities with low coverage
CWMs_all <- CWMs_all %>% filter(coverage_all >= 0.7)
CWMs_ST <- CWMs_ST %>% filter(coverage_ST >= 0.7)
CWMs_H <- CWMs_H %>% filter(coverage_H >= 0.7)

## Final output
CWM_final <- CWMs_all[,1:2] %>% full_join(CWMs_ST[,1:2]) %>% full_join(CWMs_H[,1:2])
# summary(CWM_final) # no NAs in 'CWM_herb_ST'

# Merge relevant columns with the BE_synthesis_forest_dat
# Need to rename plot column for the merge
names(CWM_final)[names(CWM_final) == "plot"] <- "Plot"
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, CWM_final, by = "Plot", all.x = T)

# Rename added columns for more context (same names as in metadata HIVE table)
# names(BE_synthesis_forest_dat)
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CWM_herb_all"] <- "Insect_herbivory_total_damaged_area"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CWM_herb_ST"] <- "Insect_herbivory_total_damaged_ST"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CWM_herb_H"] <- "Insect_herbivory_total_damaged_area_H"

# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat$Insect_herbivory_total_damaged_area))) # 39 NAs
# length(which(is.na(BE_synthesis_forest_dat$Insect_herbivory_total_damaged_ST))) # 6 NAs
# length(which(is.na(BE_synthesis_forest_dat$Insect_herbivory_total_damaged_area_H))) # 91 NAs



### === caterpillars_predation === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Blüthgen
#Dataset(s):                 25807_3_Dataset
#Relevant columns (unit):    Predation_prop (proportion)

#read data
dat <- read.table(paste0(pathtodata, "Functions/25807_3_Dataset/25807_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "PlotID", plotnam = "Plot")

#special treatment, averaging the five measurements per Plot
dat.1 <- dat %>% 
  group_by( Plot) %>% 
  summarise( caterpillars_predation = mean(Predation_prop, na.rm = T))
  
#merge caterpillars_predation with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("caterpillars_predation","Plot")], by = "Plot", all.x = T)
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$caterpillars_predation))) # 2 NAs
### ===== ###



### === barkbeetle_antagonist_ratio === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Weisser
#Dataset(s):                 20035_2_Dataset
#Relevant columns (unit):    BB_Antagonist_ratio

# Read data
dat <- read.table(paste0(pathtodata, "Functions/20035_2_Dataset/20035_2_data.txt"), header = T, sep = ";")
# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("BB_Antagonist_ratio","Plot")], by = "Plot", all.x = T)

# Special treatment for the added column: to be renamed
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "BB_Antagonist_ratio"] <- "Barkbeetle_antagonist_ratio"
# Count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Barkbeetle_antagonist_ratio))) # 2 NAs


# Save main ddf here
BE_synthesis_forest_dat.save <- BE_synthesis_forest_dat


### === Carbon cycle fluxes === ###
#Category1:                  flux
#Catergory2:                 C_cycle
#Principal Investigator(s):  Trumbore
#                            Marhan
#                            Schrumpf
#Dataset(s):                 17166_3_Dataset
#                            17026_3_Dataset
#                            22686_5_Dataset
#                            26908_4_Dataset
#                            26306_7_Dataset
#Relevant columns (unit):    Glu (nmol/(g*h))
#                            N_Ac (nmol/(g*h))
#                            Xyl (nmol/(g*h))
#                            Res_14 (µg/g) (17026_3_Dataset - Soil respiration 2011)
#                            CO2_rate_mean (µg/g) (potential soil respiration rate 2017)
#                            Soil_respiration_2018 (g/(m^2*d))
#                            Soil_respiration_2019 (g/(m^2*d))
#                            PMOR ng/(g*h) - soil methane (CH4) oxidation

# Read the datasets
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/17026_3_Dataset/17026_3_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/22686_5_Dataset/22686_5_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/26908_4_Dataset/26908_4_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/26306_7_Dataset/26306_7_data.txt"), header = T, sep = ";")

# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "PlotID", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_PlotID", plotnam = "Plot")

# Special treatment for the added columns of the 17166_3_Dataset
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat[,c("Glu","N_Ac","Xyl","Plot")], by = "Plot", all.x = T)

# Rename columns
names(BE_synthesis_forest_dat.save)[names(BE_synthesis_forest_dat.save) == "Glu"] <- "Soil_beta_glucosidase"
names(BE_synthesis_forest_dat.save)[names(BE_synthesis_forest_dat.save) == "N_Ac"] <- "Soil_Nacetyl_glucosaminidase"
names(BE_synthesis_forest_dat.save)[names(BE_synthesis_forest_dat.save) == "Xyl"] <- "Soil_xylanase"

# Calculate the mini-multifunctionality variable "soil_carbon_fluxes" as in the synthesis dataset functions grassland
# Requires the variables soil_beta_glucosidase, soil_Nacetyl_glucosaminidase and soil_xylanase
# Therefore, select the relevant columns from "BE_synthesis_forest_dat"
dat.1 <- BE_synthesis_forest_dat.save %>% 
  subset(select = c("Plot","Soil_beta_glucosidase","Soil_Nacetyl_glucosaminidase","Soil_xylanase")) %>% 
  # and calculate mini-multifunctionalities based on z-scores (sd = 1, mean = 0) of input variables
  # function returns a matrix of which we only need the 1st column
  mutate(Soil_carbon_fluxes = multidiv(.[,c("Soil_beta_glucosidase","Soil_Nacetyl_glucosaminidase","Soil_xylanase")], sc = "sd", cent = T)[,1])

# Merge the soilCflxs column
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat.1[,c("Soil_carbon_fluxes","Plot")], by = "Plot", all.x = T)

# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat1[,c("Res_14","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat2[,c("CO2_rate_mean","Plot")], by = "Plot", all.x = T)

# Special treatment for the added columns of the 17026_3_Dataset and 22686_5_Dataset: rename columns
names(BE_synthesis_forest_dat.save)[names(BE_synthesis_forest_dat.save) == "Res_14"] <- "soil_respiration_2011"
names(BE_synthesis_forest_dat.save)[names(BE_synthesis_forest_dat.save) == "CO2_rate_mean"] <- "soil_respiration_2017"

# Special treatment for the added columns of the 26908_4_Dataset
# Soil respiration was measured in 2018 and 2019. Create soil_respiration_2018 and soil_respiration_2019 columns.
dat3.1 <- dat3 %>%
  subset( Year == "2018") %>% 
  pivot_wider( names_from = Year, names_glue = "soil_respiration_{Year}", values_from = Rs)

dat3.2 <- dat3 %>%
  subset( Year == "2019") %>% 
  pivot_wider( names_from = Year, names_glue = "soil_respiration_{Year}", values_from = Rs)

# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat3.1[,c("soil_respiration_2018", "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat3.2[,c("soil_respiration_2019", "Plot")], by = "Plot", all.x = T)

# Then calculate the average soil respiration across the years 2011, 2017, 2018 and 2019
dat3.3 <- BE_synthesis_forest_dat.save %>% 
  subset( select = c("Plot", "soil_respiration_2011", "soil_respiration_2017", "soil_respiration_2018", "soil_respiration_2019")) %>% 
  # scale respiration measurements within years, across all plots, since the units differ between 2011, 2017 and 2018, 2019
  mutate( scaled_res_2011 = scale(soil_respiration_2011, center = F, scale = T),
          scaled_res_2017 = scale(soil_respiration_2017, center = F, scale = T),
          scaled_res_2018 = scale(soil_respiration_2018, center = F, scale = T),
          scaled_res_2019 = scale(soil_respiration_2019, center = F, scale = T)) %>% 
  # calculate average of temporal replicates for each plot
  rowwise(Plot) %>% 
  mutate(Average_soil_respiration = mean(c_across(c("scaled_res_2011","scaled_res_2017","scaled_res_2018","scaled_res_2019")), na.rm = T))


# Merge
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat3.3[,c("Average_soil_respiration", "Plot")], by = "Plot", all.x = T)

# Special treatment for the columns of the 26306_7_Dataset
# There are 2 replicates per plot -> average them
# Therefore, subset forest plots and average replicates per Plot
dat4.1 <- dat4 %>% 
  subset(Plot %in% BE_synthesis_forest_dat.save$Plot) %>% 
  group_by(Plot) %>% 
  summarise(Methane_oxidation = mean(PMOR, na.rm = T))
  
# Merge the averaged column
BE_synthesis_forest_dat.save <- merge(BE_synthesis_forest_dat.save, dat4.1[,c("Methane_oxidation","Plot")], by = "Plot", all.x = T)

# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat.save$Soil_beta_glucosidase))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$Soil_Nacetyl_glucosaminidase))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$Soil_xylanase))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$soil_carbon_fluxes))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$soil_respiration_2011))) # 8 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$soil_respiration_2017))) # 2 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$soil_respiration_2018))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$soil_respiration_2019))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$Average_soil_respiration))) # 0 NAs
# length(which(is.na(BE_synthesis_forest_dat.save$Methane_oxidation))) # 1 NAs


### Save (or reset if something goes wrong below)
BE_synthesis_forest_dat.save2 <- BE_synthesis_forest_dat.save
# dim(BE_synthesis_forest_dat.save2) # so far so good

# Tidy up: remove useless objects and run garbage collection
rm("cov_sp_plot_all","cov_sp_plot_H","cov_sp_plot_ST","coverage_all",
    "coverage_H","coverage_ST","CWM_final","CWMs_all","CWMs_H","CWMs_ST",
    "dat","dat.1","dat.2","dat1","dat2","dat3","dat3.1","dat3.2","dat3.3",
    "dat4","dat4.1","data_all","data_H","data_ST"
); gc()



### === Carbon cycle stocks === ###
#Category1:                  stock
#Catergory2:                 C_cycle
#Principal Investigator(s):  Bonkowski
#                            Polle
#                            Schrumpf
#                            Trumbore
#Dataset(s):                 14106_2_Dataset
#                            19230_3_Dataset
#                            20127_17_Dataset
#                            17086_4_Dataset
#                            20266_3_Dataset
#                            31271_7_Dataset
#Relevant columns (unit):    Cmic (µg/g)
#                            Carbon_within_fine_roots_Soil_Sampling_May_2011 (mg/g)
#                            Total_litter_C (g/kg)
#                            OC_stock (kg/m^2) (17086_4_Dataset - organic soil carbon stock 2011)
#                            OC_stock (kg/m^2) (20266_3_Dataset - organic soil carbon stock 2014)
#                            OC_stock (kg/m^2) (31271_7_Dataset - organic soil carbon stock 2014)

##### FUNCTIONS TO ADD ON TOP OF MARC'S VERSION: 
# - 'soil_org_C_2017' (dataID: 31271)
# - average across 2011-2015-2017
### 15/01/25: ON HOLD FOR NOW DUE TO 2017 DATASET. DO NOT MERGE WITH OTHER YEARS.
### KEEP 2017 SEPARATE ('dat5' below) FOR LATER UPDATE MAYBE

# Read data
dat <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/19230_3_Dataset/19230_3_data.txt"), header = T, sep = ";")
dat1.1 <- read.table(paste0(pathtodata, "Functions/14567_5_Dataset/14567_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/20127_17_Dataset/20127_17_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/17086_4_Dataset/17086_4_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/20266_3_Dataset/20266_3_data.txt"), header = T, sep = ";")
#dat5 <- read.table(paste0(pathtodata, "Functions/31271_7_Dataset/31271_7_data.txt"), header = T, sep = ";")

# Add two-digit plot names for merging with the BE_synthesis_forest_dat
# names(dat1); names(dat5)
dat <- BEplotZeros(dat, "PlotID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat1.1 <- BEplotZeros(dat1.1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")
#dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "Plot")

# Rename columns
names(dat)[names(dat) == "Cmic"] <- "microbial_C"
names(dat1)[names(dat1) == "Carbon_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_roots_C"
names(dat3)[names(dat3) == "OC_stock"] <- "soil_org_C_2011"
names(dat4)[names(dat4) == "OC_stock"] <- "soil_org_C_2014"
# names(dat5)[names(dat5) == "C_stock"] <- "soil_org_C_2017"

# Compare range and units of 2017 data with previous years
# summary(dat3$soil_org_C_2011)
# Convert negative values to NA
dat3 <- dat3 %>% mutate(soil_org_C_2011 = replace(soil_org_C_2011, which(soil_org_C_2011 < 0), NA))
# summary(dat4$soil_org_C_2014) # is ok
# summary(dat5$soil_org_C_2017) # no negative values but much higher values relative to 2011 and 2014...Looks lie something is wrong

# WATCHOUT, dat5 has 3 horizons levels?
# summary(factor(dat5$Horizon)) # Which to take??

# Merge
BE_synthesis_forest_dat.save2 <- merge(BE_synthesis_forest_dat.save2, dat[,c("microbial_C","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save2 <- merge(BE_synthesis_forest_dat.save2, dat1[,c("Fine_roots_C","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save2 <- merge(BE_synthesis_forest_dat.save2, dat3[,c("soil_org_C_2011","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save2 <- merge(BE_synthesis_forest_dat.save2, dat4[,c("soil_org_C_2014","Plot")], by = "Plot", all.x = T)
#BE_synthesis_forest_dat.save2 <- merge(BE_synthesis_forest_dat.save2, dat5[,c("soil_org_C_2017","Plot")], by = "Plot", all.x = T)

### 15/01/25: Compute average across 2011, 2014 in a dummy data.frame before adding it to main data.frame
datX <- BE_synthesis_forest_dat.save2 %>%
   subset( select = c("Plot","soil_org_C_2011","soil_org_C_2014")) %>%
   # scale respiration measurements within years, across all plots, since the units differ between 2011, 2017 and 2018, 2019
   mutate( scaled_soil_org_C_2011 = scale(soil_org_C_2011, center = F, scale = T),
           scaled_soil_org_C_2014 = scale(soil_org_C_2014, center = F, scale = T)) %>%
   # calculate average of temporal replicates for each plot
   rowwise(Plot) %>%
   mutate(Average_soil_org_C = mean(c_across(c("scaled_soil_org_C_2011","scaled_soil_org_C_2014")), na.rm = T))

# Merge with main dataset
BE_synthesis_forest_dat.save2 <- merge(BE_synthesis_forest_dat.save2, datX[,c("Average_soil_org_C","Plot")], by = "Plot", all.x = T)


#### ============ ####
# Special treatment for the columns of the 19230_3_Dataset and 14567_5_Dataset: 
# both datasets measure fine roots carbon concentration, but 14567_5_Dataset also contains data from grassland plots
# correlate the "Carbon_within_fine_roots_Soil_Sampling_May_2011" and "Total_C" values,
# if they show strong correlation, use values from the 14567_5_Dataset

# subset forest plots for both datasets
# dat1.2 <- merge(BE_synthesis_forest_dat[,c(1:4)], dat1[,c("fine_roots_C","Plot")], by = "Plot", all.x = T)
# dat1.3 <- merge(dat1.2, dat1.1[,c("Total_C","Plot")], by = "Plot", all.x = T)
# #plot data (there is one outlier in terms of 'fine_roots_C')
# plot(dat1.3$fine_roots_C, dat1.3$Total_C)
# cor(dat1.3$fine_roots_C, dat1.3$Total_C, use = "pairwise.complete.obs", method = "pearson") # 0.361
# #remove the one outlier from 14567_5_Dataset and compute correlation coeff. again
# dat1.3[dat1.3$Plot == "AEW17", "fine_roots_C"] <- NA
# plot(dat1.3$fine_roots_C, dat1.3$Total_C)
# cor(dat1.3$fine_roots_C, dat1.3$Total_C, use = "pairwise.complete.obs", method = "pearson")
# Increases the correlation r = 0.387 (+ 0.2 only) --> NOT WORTH using 14567_5_Dataset over 19230_3_Dataset
#### ============ ####


### Save main ddf here
BE_synthesis_forest_dat.save3 <- BE_synthesis_forest_dat.save2

# Special treatment for the temporal replicates of total litter C stocks of the 20127_17_Dataset
dat2.1 <- dat2 %>% 
  subset( select = c(Plot,Year,Season,Element,Total_litter)) %>% 
  # remove the year 2019, because there are no measurements for Total_litter and remove Sulfur measurements
  subset( !(Year == "2019") & !(Element == "S") & !(Element == "N")) %>% 
  # reformat to wide for calculation of averages between seasons
  pivot_wider(names_from = c("Season","Element","Year"), values_from = "Total_litter") %>% 
  # average seasonal measurements to get a value for each year and then average the years
  rowwise(Plot) %>% 
  mutate(Total_litter_C_2015 = mean(c_across(c("S_C_2015","A_C_2015")), na.rm = T),
    Total_litter_C_2016 = mean(c_across(c("W_C_2016","S_C_2016","A_C_2016")), na.rm = T),
    Total_litter_C_2017 = mean(c_across(c("W_C_2017","S_C_2017","A_C_2017")), na.rm = T),
    Total_litter_C_2018 = mean(c_across(c("W_C_2018","S_C_2018")), na.rm = T),
    Average_Total_litter_C = mean(c_across(c("Total_litter_C_2015", "Total_litter_C_2016", 
                            "Total_litter_C_2017", "Total_litter_C_2018")), na.rm = T) )
                                                   
# Merge individual years, as well as the average between years
BE_synthesis_forest_dat.save3 <- merge(BE_synthesis_forest_dat.save3, dat2.1[,c("Total_litter_C_2015","Total_litter_C_2016",
                            "Total_litter_C_2017", "Total_litter_C_2018",
                            "Average_Total_litter_C","Plot")], by = "Plot", all.x = T)

# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat.save3$microbial_C))) # 2 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$Fine_roots_C))) # 2 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$Total_litter_C_2015))) # 28 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$Total_litter_C_2016))) # 28 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$Total_litter_C_2017))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$Total_litter_C_2018))) # 3 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$Average_Total_litter_C))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$soil_org_C_2011))) # 8 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$soil_org_C_2014))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$soil_org_C_2017))) # 0 NAs



### === Nitrogen cycle fluxes === ###
#Category1:                  flux
#Catergory2:                 N_cycle
#Principal Investigator:     Schloter
#                            Schrumpf
#Dataset(s):                 19847_3_Dataset
#                            21546_2_Dataset
#                            27266_5_Dataset
#Relevant columns (unit):    PNR (ng/(g*h)) (19847_3_Dataset - Potential Nitrification Rate 2014)
#                            pN (ng/(g*h)) (21546_2_Dataset - Potential Nitrification Rate 2016)
#                            amoA_AOA
#                            amoA_AOB
#                            nxrA_NB
#                            X16S_NS
#                            Ammonium
#                            Nitrate

# Read data
dat <- read.table(paste0(pathtodata, "Functions/19847_3_Dataset/19847_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/21546_2_Dataset/21546_2_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/27266_5_Dataset/27266_5_data.txt"), header = T, sep = ";")
# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "Plot_ID", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")

# Special treatment: before adding the columns of the 19847_3_Dataset and the 21546_2_Dataset: rename columns
names(dat)[names(dat) == "PNR"] <- "soil_potential_nitrification_2014"
names(dat1)[names(dat1) == "pN"] <- "soil_potential_nitrification_2016"


### FABIO: CHECK RANGE AND UNITS OF 'PNR' and 'pN'.
### STANDARDIZE IF NECESSARY TO COMPUTE AVERAGE, ELSE LEAVE SEPARATE 
# unique(dat$soil_potential_nitrification_2014)
# unique(dat1$soil_potential_nitrification_2016)
# Very different range of values, plus these are different units --> do not merge to average

# Special treatment for temporal replicates: 
# average years to generate the "average_soil_potential_nitrification" variables
# therefore, create an intermediate dataframe with the relevant columns
dat.1 <- merge(BE_synthesis_identifier_dat, dat[,c("soil_potential_nitrification_2014","Plot")], by = "Plot", all.x = T)
dat.1 <- merge(dat.1, dat1[,c("amoA_AOA","amoA_AOB","nxrA_NB","X16S_NS","soil_potential_nitrification_2016","Plot")], by = "Plot", all.x = T)

# Instead of values some rows contain the string "bdl" and their columns are character columns
# Replace these strings with NA
dat.1[dat.1 == "bdl"] <- NA

# Change the character columns to numeric, to enable calculation of an average and rename them
dat.1 <- dat.1 %>% 
  mutate(soil_potential_nitrification_2014 = as.numeric(soil_potential_nitrification_2014),
    soil_potential_nitrification_2016 = as.numeric(soil_potential_nitrification_2016),
    ammonia_oxidizing_archaea = amoA_AOA,
    ammonia_oxidizing_bacteria = amoA_AOB,
    nitrite_oxidizing_nitrobacter = as.numeric(nxrA_NB),
    nitrite_oxidizing_nitrospira = as.numeric(X16S_NS))

# Merge generated columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat.save3 <- merge(BE_synthesis_forest_dat.save3, dat.1[,c("soil_potential_nitrification_2014","soil_potential_nitrification_2016",
        "ammonia_oxidizing_archaea","ammonia_oxidizing_bacteria","nitrite_oxidizing_nitrobacter","nitrite_oxidizing_nitrospira","Plot")],
        by = "Plot", all.x = T)


# Calculate the mini-multifun. variable "forest_soil_nitrate_fluxes"
# a similar variable exists in the grassland functions synthesis dataset (27087)
# in the 27087 dataset, "soilNitrateflxs" is calculated with the variables nxrA_NB, 16S_NS, nifH and DEA
# we lack nifH and DEA data for forests
# therefore, "forest_soil_nitrate_fluxes" is only calculated with the variables nitrite_oxidizing_nitrobacter and nitrite_oxidizing_nitrospira (from the 21546_2_Dataset)
dat.3 <- BE_synthesis_forest_dat.save3 %>% 
  subset(select = c("Plot", "nitrite_oxidizing_nitrobacter","nitrite_oxidizing_nitrospira")) %>%
  # like in grasslands, nitrite_oxidizing_nitrobacter and nitrite_oxidizing_nitrospira are summed up to “nitrite-oxidising functional gene abundances” (nitOx_fga)
  rowwise(Plot) %>% 
  mutate(nitOx_fga = sum(c(nitrite_oxidizing_nitrobacter, nitrite_oxidizing_nitrospira), na.rm = T))

# Replace all "0" values of "nitOx_fga" by NA
dat.3[dat.3$nitOx_fga == 0,] <- NA

# Then calculate mini-multifunctionality which here is simply the z-score of nitOx_fga (sd = 1, mean = 0)
dat.3$soil_nitrite_fluxes <- multidiv(dat.3[,4], sc = "sd", cent = F)[,1]

# Merge the forest_soilNitrateflxs column
BE_synthesis_forest_dat.save3 <- merge(BE_synthesis_forest_dat.save3, dat.3[,c("soil_nitrite_fluxes","Plot")], by = "Plot", all.x = T)
dim(BE_synthesis_forest_dat.save3) # should be 151


# Calculate the mini-multifunctionality variable "forest_soil_ammonia_fluxes"
# A similar variable exists in the grassland functions synthesis dataset (27087)
# in the 27087 dataset, "soilAmmoniaflxs" is calculated with the variables amoA_AOA, amoA_AOB and Urease
# but we lack Urease data for forests.
# Therefore, "forest_soil_ammonia_fluxes" is only calculated with the variables amoA_AOA, amoA_AOB (from the 21546_2_Dataset)
dat.4 <- BE_synthesis_forest_dat.save3 %>% 
  subset(select = c("Plot","ammonia_oxidizing_archaea","ammonia_oxidizing_bacteria")) %>%
  #like in grasslands, amoA_AOA and amoA_AOB are summed up to “ammonia-oxidising functional gene abundances” (amOX_fga)
  rowwise(Plot) %>% 
  mutate(amOX_fga = sum(c(ammonia_oxidizing_archaea, ammonia_oxidizing_bacteria), na.rm = T))

# Replace all "0" values of "nitOx_fga" by NA
dat.4[dat.4$amOX_fga == 0,] <- NA
# Then calculate mini-multifunctionality which here is simply the z-score of nitOx_fga (sd = 1, mean = 0)
dat.4$soil_ammonia_fluxes <- multidiv(dat.4[,4], sc = "sd", cent = F)[,1]
# Merge the forest_soilNitrateflxs column
BE_synthesis_forest_dat.save3 <- merge(BE_synthesis_forest_dat.save3, dat.4[,c("soil_ammonia_fluxes","Plot")], by = "Plot", all.x = T)

# Special treatment for the columns of the 27266_5_Dataset
# Calculate the "N_retention" variable, by inverting the Ammonia and Nitrate variables and summing them up
dat2.1 <- dat2 %>% rowwise() %>% mutate(Soil_N_retention = 1/(sum(c(Ammonium,Nitrate), na.rm = T)))

# Merge N_retention column
BE_synthesis_forest_dat.save3 <- merge(BE_synthesis_forest_dat.save3, dat2.1[,c("Soil_N_retention","Plot")], by = "Plot", all.x = T)

# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat.save3$soil_potential_nitrification_2014))) # 11 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$soil_potential_nitrification_2016))) # 11 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$ammonia_oxidizing_archaea))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$ammonia_oxidizing_bacteria))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$nitrite_oxidizing_nitrobacter))) # 2 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$nitrite_oxidizing_nitrospira))) # 12 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$soil_nitrite_fluxes))) # 3 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$soil_ammonia_fluxes))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save3$Soil_N_retention))) # 1 NAs

### Save main ddf here
BE_synthesis_forest_dat.save4 <- BE_synthesis_forest_dat.save3


### === Nitrogen cycle stocks === ###
#Category1:                  stock
#Catergory2:                 N_cycle
#Principal Investigator:     Bonkowski
#                            Polle
#                            Schrumpf
#                            Trumbore
#Dataset(s):                 14106_2_Dataset
#                            19230_3_Dataset
#                            20127_17_Dataset
#                            17086_4_Dataset
#                            20266_3_Dataset
#                            31271_7_Dataset
#Relevant columns (unit):    Nmic (µg/g)
#                            Nitrogen_within_fine_roots_Soil_Sampling_May_2011 (mg/g)
#                            N_stock (kg/m^2)
#                            Total_litter (g/kg)


### FABIO: ADD 2017 DATA FOR N STOCK (31271_7_Dataset) AND COMPUTE AVERAGE ACROSS 2011-2014-2017
### 15/01/25: ON HOLD BECAUSE OF THE DIFFERENT SOIL HORIZONS IN THE 2017 DATA. 2017 VALUES MAY BE ADDED
### LATER FOR AN UPDATE, ONCE WE KNOW HOW TO PROPERLY MERGE THEM WITH THE PREVIOUS

# Read data
dat <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/19230_3_Dataset/19230_3_data.txt"), header = T, sep = ";")
dat1.1 <- read.table(paste0(pathtodata, "Functions/14567_5_Dataset/14567_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/20127_17_Dataset/20127_17_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/17086_4_Dataset/17086_4_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/20266_3_Dataset/20266_3_data.txt"), header = T, sep = ";")
#dat5 <- read.table(paste0(pathtodata, "Functions/31271_7_Dataset/31271_7_data.txt"), header = T, sep = ";")

# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "PlotID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat1.1 <- BEplotZeros(dat1.1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")
#dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "Plot")
### SAME ISSUE AS FOR SOIL C STCOK: 3 HORIZONS LEVELS...I DO NOT KOW WHICH ONE TO CHOOSE

# Rename
names(dat)[names(dat) == "Nmic"] <- "microbial_N"
names(dat1)[names(dat1) == "Nitrogen_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_roots_N"
names(dat3)[names(dat3) == "N_stock"] <- "Soil_N_2011"
names(dat4)[names(dat4) == "N_stock"] <- "Soil_N_2014"

# Merge
BE_synthesis_forest_dat.save4 <- merge(BE_synthesis_forest_dat.save4, dat[,c("microbial_N","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save4 <- merge(BE_synthesis_forest_dat.save4, dat3[,c("Soil_N_2011","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save4 <- merge(BE_synthesis_forest_dat.save4, dat4[,c("Soil_N_2014","Plot")], by = "Plot", all.x = T)

# Check values
# summary(BE_synthesis_forest_dat.save4$Soil_N_2011) # convert negative values to NA
# summary(BE_synthesis_forest_dat.save4$Soil_N_2014) # ok
BE_synthesis_forest_dat.save4 <- BE_synthesis_forest_dat.save4 %>% mutate(Soil_N_2011 = replace(Soil_N_2011, which(Soil_N_2011 < 0), NA))

# Add Average_Soil_N using 2011 and 2014 data only
BE_synthesis_forest_dat.save4 <- BE_synthesis_forest_dat.save4 %>% rowwise(Plot) %>% 
                    mutate(Average_Soil_N = mean(c_across(c("Soil_N_2011","Soil_N_2014")), na.rm = T))


# Special treatment for the columns of the 19230_3_Dataset and 14567_5_Dataset: 
# both datasets measure fine roots nitrogen concentration, but 14567_5_Dataset also contains data from grassland plots
# correlate the "fine_roots_N" and "Total_N" values, 
# if they correlate well, use values from the 14567_5_Dataset, subset forest plots for both datasets
# dat1.2 <- merge(BE_synthesis_forest_dat.save4[,c(1:4)], dat1[,c("Fine_roots_N","Plot")], by = "Plot", all.x = T)
# dat1.3 <- merge(dat1.2, dat1.1[,c("Total_N","Plot")], by = "Plot", all.x = T)
# # plot data (there is one outlier)
# plot(dat1.3$Fine_roots_N, dat1.3$Total_N)
# cor(dat1.3$Fine_roots_N, dat1.3$Total_N, use = "pairwise.complete.obs", method = "pearson")
# pearson r = 0.573

# Remove the one outlier from 14567_5_Dataset and correlate again
# dat1.3[dat1.3$Plot == "AEW17","fine_roots_N"] <- NA
# plot(dat1.3$Fine_roots_N, dat1.3$Total_N)
# cor(dat1.3$Fine_roots_N, dat1.3$Total_N, use = "pairwise.complete.obs", method = "pearson")
# removing the outlier decreases the correlation to pearson r = 0.51 --> NOT WORTH IT to use 14567_5_Dataset over 19230_3_Dataset

# Merge the fine_roots_N variable from the 19230_3_Dataset
BE_synthesis_forest_dat.save4 <- merge(BE_synthesis_forest_dat.save4, dat1[,c("Fine_roots_N","Plot")], by = "Plot", all.x = T)


# Special treatment for the temporal replicates of total litter N stocks of the 20127_17_Dataset
dat2.1 <- dat2 %>% 
  subset(select = c(Plot, Year, Season, Element, Total_litter)) %>% 
  # remove the year 2019, because there are no measurements for Total_litter and remove Sulfur measurements
  subset(!(Year == "2019") & !(Element == "S") & !(Element == "C")) %>% 
  # reformat to wide for calculation of averages between seasons
  pivot_wider(names_from = c("Season", "Element", "Year"), values_from = "Total_litter") %>% 
  # average seasonal measurements to get a value for each year and then average the years
  rowwise(Plot) %>% 
  mutate(Total_litter_N_2015 = mean(c_across(c("S_N_2015", "A_N_2015")), na.rm = T),
    Total_litter_N_2016 = mean(c_across(c("W_N_2016", "S_N_2016", "A_N_2016")), na.rm = T),
    Total_litter_N_2017 = mean(c_across(c("W_N_2017", "S_N_2017", "A_N_2017")), na.rm = T),
    Total_litter_N_2018 = mean(c_across(c("W_N_2018", "S_N_2018")), na.rm = T),
    Average_Total_litter_N = mean(c_across(c("Total_litter_N_2015", "Total_litter_N_2016",
                            "Total_litter_N_2017", "Total_litter_N_2018")), na.rm = T))
                                                                                                      
# Merge individual years, as well as the average between years
BE_synthesis_forest_dat.save4 <- merge(BE_synthesis_forest_dat.save4,
                            dat2.1[,c("Total_litter_N_2015", "Total_litter_N_2016",
                                "Total_litter_N_2017", "Total_litter_N_2018",
                                "Average_Total_litter_N","Plot")], by = "Plot", all.x = T)

# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat.save4$microbial_N))) # 2 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Fine_roots_N))) # 2 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Total_litter_N_2015))) # 28 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Total_litter_N_2016))) # 28 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Total_litter_N_2017))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Total_litter_N_2018))) # 3 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Average_Total_litter_N))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Soil_N_2011))) # 8 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Soil_N_2014))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save4$Average_Soil_N))) # 1 NAs

BE_synthesis_forest_dat.save5 <- BE_synthesis_forest_dat.save4


### === Phosphorus cycle fluxes === ###
#Category1:                  flux
#Catergory2:                 P_cycle
#Principal Investigator:     Trumbore
#                            Schrumpf
#                            Oelmann
#Dataset(s):                 17166_3_Dataset
#                            23906_7_Dataset
#                            19009_3_Dataset
#                            19286_3_Dataset
#                            31340_4_Dataset
#                            5241_5_Dataset
#                            27266_5_Dataset
#                            26228_4_Dataset
#                            26229_4_Dataset
#Relevant columns (unit):    Pho (nmol/(g*h))
#                            Resin_P (mg/kg)
#                            OlsenPi (mg/kg) (19286_3_Dataset - OlsenPi_2014)
#                            Olsen-P (mg/kg) (31340_4_Dataset - OlsenPi_2021)
#                            NaHCO3_Pi (mg/kg)
#                            Pmic (mg/kg)


### FABIO: - ADD 'soil_P_retention_2018' from 27266_5_Dataset (column 'Phosphorus')
###        - IF COMPARABLE VARIABLES: COMPUTE AND ADD AVERAGE soil_P_retention BASED ON 2011 AND 2018 data
###        - ADD 'soil_P_soluble' AND 'soil_org_P_soluble' FROM DATASET 26228 (column 'P_soluble') AND 26229 (column 'P_soluble')

# Read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/23906_7_Dataset/23906_7_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19009_3_Dataset/19009_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/27266_5_Dataset/27266_5_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/26228_4_Dataset/26228_4_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/26229_4_Dataset/26229_4_data.txt"), header = T, sep = ";")

# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "Plot")

# Special treatment for the columns of the columns of the 17166_3_Dataset and 23906_7_Dataset --> rename
names(dat)[names(dat) == "Pho"] <- "Soil_phosphatase_2011"
names(dat1)[names(dat1) == "Pho"] <- "Soil_phosphatase_2014"
# Calculate an average of the temporal replicates of soil phosphatase stocks and soil Olsen-P
# Therefore, create an intermediate dataframe with the relevant columns
dat.1 <- merge(BE_synthesis_identifier_dat, dat[,c("Soil_phosphatase_2011", "Plot")], by = "Plot", all.x = T)
dat.2 <- merge(dat.1, dat1[,c("Soil_phosphatase_2014","Plot")], by = "Plot", all.x = T)

# Calculate the averages of temporal replicates
dat.3 <- dat.2 %>% rowwise(Plot) %>% mutate(Average_soil_phosphatase = mean(c_across(c("Soil_phosphatase_2011","Soil_phosphatase_2014")), na.rm = T))

# Merge with main ddf
BE_synthesis_forest_dat.save5 <- merge(BE_synthesis_forest_dat.save5, dat.3[,c("Soil_phosphatase_2011","Soil_phosphatase_2014",
                    "Average_soil_phosphatase","Plot")], by = "Plot", all.x = T)

# Special treatment for the Resin_P variable of the 19009_3_Dataset
# Resin_P will be inverted and renamed to soil_P_retention
dat2.1 <- dat2 %>% 
  # subset forest plots
  subset(Plot %in% BE_synthesis_identifier_dat$Plot) %>% 
  # invert the variable
  mutate(Soil_P_retention_2011 = 1/Resin_P)

# Merge
BE_synthesis_forest_dat.save5 <- merge(BE_synthesis_forest_dat.save5, dat2.1[,c("Soil_P_retention_2011","Plot")], by = "Plot", all.x = T)

### Adding Soil_P_retention for 2018 ('dat3')
# summary(dat3$Phosphorus)
# to be compared against 'BE_synthesis_forest_dat.save5$Soil_P_retention_2011'
# summary(BE_synthesis_forest_dat.save5$Soil_P_retention_2011)
### --> don't merge into an average, just add the other year

# Rename & merge
names(dat3)[names(dat3) == "Phosphorus"] <- "Soil_P_retention_2018"
BE_synthesis_forest_dat.save5 <- merge(BE_synthesis_forest_dat.save5, dat3[,c("Soil_P_retention_2018","Plot")], by = "Plot", all.x = T)

# Adding soil_P_soluble & soil_org_P_soluble
# Rename & merge
names(dat4)[names(dat4) == "P_soluble"] <- "Soil_org_P_soluble"
names(dat5)[names(dat5) == "P_soluble"] <- "Soil_P_soluble"
BE_synthesis_forest_dat.save5 <- merge(BE_synthesis_forest_dat.save5, dat4[,c("Soil_org_P_soluble","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save5 <- merge(BE_synthesis_forest_dat.save5, dat5[,c("Soil_P_soluble","Plot")], by = "Plot", all.x = T)


# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat.save5$Soil_phosphatase_2011))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save5$Soil_phosphatase_2014))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save5$Average_soil_phosphatase))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save5$Soil_P_retention_2011))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save5$Soil_P_retention_2018))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save5$Soil_org_P_soluble))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save5$Soil_P_soluble))) # 1 NAs

# Save or reset here
BE_synthesis_forest_dat.save6 <- BE_synthesis_forest_dat.save5


### === Phosphorus cycle stocks === ###
#Category1:                  stock
#Catergory2:                 P_cycle
#Principal Investigator:     Oelmann
#                            Schrumpf
#Dataset(s):                 19286_3_Dataset
#                            31340_4_Dataset
#                            5241_5_Dataset
#                            15766_3_Dataset
#Relevant columns (unit):    OlsenPi (mg/kg) (19286_3_Dataset - OlsenPi_2014)
#                            Olsen-P (mg/kg) (31340_4_Dataset - OlsenPi_2021)
#                            NaHCO3_Pi (mg/kg) 
#                            Pmic (mg/kg) (15766_3_Dataset - microbial_P_2011)

# Read data
dat <- read.table(paste0(pathtodata, "Functions/19286_3_Dataset/19286_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/31340_4_Dataset/31340_4_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/5241_5_Dataset/5241_5_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/15766_3_Dataset/15766_3_data.txt"), header = T, sep = ";")
# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP", plotnam = "Plot")

# Rename 
names(dat)[names(dat) == "OlsenPi"] <- "Soil_OlsenPi_2014"
names(dat1)[names(dat1) == "Olsen.P"] <- "Soil_OlsenPi_2021"
names(dat2)[names(dat2) == "NaHCO3_Pi"] <- "Soil_NaHCO3_P"
names(dat3)[names(dat3) == "Pmic"] <- "microbial_P"

# Merge columns without any special treatments
BE_synthesis_forest_dat.save6 <- merge(BE_synthesis_forest_dat.save6, dat2[,c("Soil_NaHCO3_P","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save6 <- merge(BE_synthesis_forest_dat.save6, dat3[,c("microbial_P","Plot")], by = "Plot", all.x = T)

# Special treatment for the variables of the 19286_3_Dataset and 31340_4_Dataset:
# calculate an average of the temporal replicates of soil Olsen-P
# Therefore, create an intermediate dataframe with the relevant columns
dat.1 <- merge(BE_synthesis_identifier_dat, dat[,c("Soil_OlsenPi_2014","Plot")], by = "Plot", all.x = T)
dat.2 <- merge(dat.1, dat1[,c("Soil_OlsenPi_2021","Plot")], by = "Plot", all.x = T)
# calculate the averages of temporal replicates
dat.3 <- dat.2 %>% rowwise(Plot) %>% mutate(Average_Soil_OlsenPi = mean(c_across(c("Soil_OlsenPi_2014","Soil_OlsenPi_2021")), na.rm = T))

# Merge
BE_synthesis_forest_dat.save6 <- merge(BE_synthesis_forest_dat.save6, dat.3[,c("Soil_OlsenPi_2014","Soil_OlsenPi_2021",
                                "Average_Soil_OlsenPi","Plot")], by = "Plot", all.x = T)

# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat.save6$Soil_NaHCO3_P))) # 20 NAs
# length(which(is.na(BE_synthesis_forest_dat.save6$microbial_P))) # 2 NAs
# length(which(is.na(BE_synthesis_forest_dat.save6$Soil_OlsenPi_2014))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save6$Soil_OlsenPi_2021))) # 1 NAs
# length(which(is.na(BE_synthesis_forest_dat.save6$Average_Soil_OlsenPi))) # 0 NAs

# Save or reset here 
BE_synthesis_forest_dat.save7 <- BE_synthesis_forest_dat.save6


### === Plant biomass stocks === ###
#Category1:                  stock
#Catergory2:                 plant_biomass
#Principal Investigator:     Schrumpf
#Dataset(s):                 14448_3_Dataset
#                            19326_5_Dataset
#                            23886_6_Dataset
#                            20126_35
#                            22846_4_Dataset
#Relevant columns (unit):    Fine_Roots_Biomass (g/cm^3)
#                            Coarse_Roots_Biomass (g/cm^3)
#                            Weight_Roots_Field (g) --> convert to g/cm^3 !!!!
#                            iG for Productivity (m^2/ha/year; basal area growth as a proxy for productivity)   


### 03/12: FABIO:
### - ADD 'root_biomass' from 2014 (#19326, 2017 (#23886) # 2021 DATA NOT AVAILABLE YET
### - COMPUTE and add AVERAGE root_biomass ACROSS 2011, 2014, 2017 IF POSSIBLE 
### - ADD 'litter_biomass' FROM 2015 to 2022 (#20126_35, variable = 'Total_Weight'); ONE VARIABLE PER YEAR
### - COMPUTE and add 'average_litter_biomass' ACROSS 2015-2022
### - ADD 'above_ground_biomass' AS WELL AS 'above_ground_C', 'above_ground_N' and 'above_ground_P' (Bruno's data)
### - ADD 'productivity' (22846_4_Dataset, variable 'iG')

# Read data
dat <- read.table(paste0(pathtodata,"Functions/14448_3_Dataset/14448_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata,"Functions/19326_5_Dataset/19326_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata,"Functions/23886_6_Dataset/23886_6_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata,"Functions/20126_35_Dataset/20126_35_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata,"Functions/22846_4_Dataset/22846_4_data.txt"), header = T, sep = ";")

# Add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")

# Special treatment for the added columns of the 14448_3_Dataset
# before merging sum up the Fine_Roots_Biomass and Coarse_Roots_Biomass to get root_Biomass (g/cm^3)
# if Fine_Roots_Biomass, or Coarse_Roots_Biomass is NA, Root_Biomass is simply the other value
dat.1 <- dat %>% rowwise() %>% mutate(Root_biomass_2011 = sum(c(Fine_Roots_Biomass,Coarse_Roots_Biomass), na.rm = TRUE))

# Merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat.save7 <- merge(BE_synthesis_forest_dat.save7, dat.1[,c("Root_biomass_2011","Plot")], by = "Plot", all.x = T)

# Compare the 'Root_biomass_2011' valyes to 'Weight_Roots_Field' in dat1 and dat2
# summary(BE_synthesis_forest_dat.save7$Root_biomass_2011)
# summary(dat1$Weight_Roots_Field)
# summary(dat2$Weight_Roots_Field)
### 'Weight_Roots_Field' from 2014 and 2017 are much higher (one order of magnitude at least)
### --> Makes, sense these are g and not g/cm^3 --> need to divide buy volume sampled to get g/cm^3
### ON HOLD BECAUSE NO ASNWER FROM SOIL CORE TEAM, SO VOLUME REMAINS UNKNOWN.
### MAY BE CONVERTED TO CORRECT UNIT LATER AND ADDED TO THE DATASET FOR AN UPDATE


### ADD 2014 and 2017 data as separate columns, after renaming to 'Root_biomass_2014' and 'Root_biomass_2017'
names(dat1)[names(dat1) == "Weight_Roots_Field"] <- "Root_biomass_2014"
names(dat2)[names(dat2) == "Weight_Roots_Field"] <- "Root_biomass_2017"
BE_synthesis_forest_dat.save7 <- merge(BE_synthesis_forest_dat.save7, dat1[,c("Root_biomass_2014","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat.save7 <- merge(BE_synthesis_forest_dat.save7, dat2[,c("Root_biomass_2017","Plot")], by = "Plot", all.x = T)


## Add yearly total 'litter_biomass' ('Total_Weight' per year in 'dat3') and its average
# dcast to have years for different colums and compute average as an extra column
dat3.1 <- reshape2::dcast(data = dat3, Plot ~ Year, value.var = "Total_Weight", fun.aggregate = mean)
dat3.1$Average_litter_biomass <- rowMeans(x = as.matrix(dat3.1[,c(2:length(dat3.1))]), na.rm = T)
# Renale colnames
colnames(dat3.1)[c(2:9)] <- paste("litter_biomass", colnames(dat3.1)[c(2:9)], sep = "_")

# Merge with 'BE_synthesis_forest_dat.save7'
BE_synthesis_forest_dat.save7 <- merge(BE_synthesis_forest_dat.save7, dat3.1, by = "Plot", all.x = T)

# Add the variables computed by Bruno: above_ground_biomass, 'above_ground_C', 'above_ground_N' and 'above_ground_P' 
# stored in the 'AGB_stocks.csv' dataset (given directly by Bruno and prepared using the 'AGB_stocks.R' script)
dat5 <- read.csv(paste0(pathtodata,"Functions/AGB_stocks.csv"), header = T, sep = ",", dec = ".")
# Merge
colnames(dat5)[2] <- "Plot"
BE_synthesis_forest_dat.save7 <- merge(BE_synthesis_forest_dat.save7, dat5, by = "Plot", all.x = T)

# Rename
names(BE_synthesis_forest_dat.save7)[names(BE_synthesis_forest_dat.save7) == "AGB"] <- "Aboveground_biomass"
names(BE_synthesis_forest_dat.save7)[names(BE_synthesis_forest_dat.save7) == "C_stock"] <- "Aboveground_C_stock"
names(BE_synthesis_forest_dat.save7)[names(BE_synthesis_forest_dat.save7) == "N_stock"] <- "Aboveground_N_stock"
names(BE_synthesis_forest_dat.save7)[names(BE_synthesis_forest_dat.save7) == "P_stock"] <- "Aboveground_P_stock"

# Add 'Productivity' from 'dat4', rename & merge
names(dat4)[names(dat4) == "iG"] <- "Productivity"
BE_synthesis_forest_dat.save7 <- merge(BE_synthesis_forest_dat.save7, dat4[,c('Plot','Productivity')], by = "Plot", all.x = T)

# Count NAs in the added columns
# length(which(is.na(BE_synthesis_forest_dat.save7$Root_biomass_2011))) # 1
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2015))) # 28
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2016))) # 36
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2017))) # 5
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2018))) # 11
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2019))) # 17
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2020))) # 55
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2021))) # 5
# length(which(is.na(BE_synthesis_forest_dat.save7$litter_biomass_2022))) # 54
# length(which(is.na(BE_synthesis_forest_dat.save7$Average_litter_biomass))) # 1
# length(which(is.na(BE_synthesis_forest_dat.save7$Aboveground_biomass))) # 1
# length(which(is.na(BE_synthesis_forest_dat.save7$Aboveground_C_stock))) # 1
# length(which(is.na(BE_synthesis_forest_dat.save7$Aboveground_N_stock))) # 1
# length(which(is.na(BE_synthesis_forest_dat.save7$Aboveground_P_stock))) # 1
# length(which(is.na(BE_synthesis_forest_dat.save7$Productivity))) # 17

# Check
# names(BE_synthesis_forest_dat.save7)
# Remove column 'X'
BE_synthesis_forest_dat.save7 <- BE_synthesis_forest_dat.save7 %>% select(-X)

# Save or reset
BE_synthesis_forest_dat.finished <- BE_synthesis_forest_dat.save7


### === Final polishing of the dataset synthesis functions forest === ###
## Replace BExIS placeholder values "-888888.00" with NA
BE_synthesis_forest_dat.finished[BE_synthesis_forest_dat.finished == -888888.00 & !is.na(BE_synthesis_forest_dat.finished)] <- NA

### FINAL CHECKS
# dim(BE_synthesis_forest_dat.finished) # 151 x 81
# head(BE_synthesis_forest_dat.finished); tail(BE_synthesis_forest_dat.finished)
# names(BE_synthesis_forest_dat.finished) # ok
# str(BE_synthesis_forest_dat.finished); summary(BE_synthesis_forest_dat.finished) 

### Save
write.table(BE_synthesis_forest_dat.finished, file = "BE_synthesis_forest_dat_wide_Jan15_1.txt", quote = F, sep = "\t", row.names = F) 

### Next: 'forest_functions_homogenize.R' to convert to long version while making sure that the
### functions' names match those in the HIVE metedata file (used to generate the helper.txt table)


### =========================================================================================================
### =========================================================================================================
### =========================================================================================================