
library(tidyverse)
library(rlpi)
library(future)
library(future.apply)
library(data.table)
library(patchwork)

route <- '03OutData/'
exroute <- '04Plots/'

#functions 
load('01Scripts/functionsLPIT.RData')

#Verify folders
folders <- c(
  "lpi_temp/constrain/Zeropermutation/results/",
  "lpi_temp/constrain/napermutations/results/"
)

for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
}

# Set the random seed to replicate the stochastic process in the manuscript
set.seed(42)
years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

#Real LPI data
lpi_data <- read.csv('00RawData/LPD2022_public.csv')
lpi_data_filtered <- lpi_data %>% select(34:104)
## Only with NA's
lpi_data_filteredNA <- lpi_data %>% select(34:104)
#only 0's
lpi_data_filtered0 <- lpi_data %>% select(34:104)

# Assuming your data is stored in a matrix called lpi_data_filtered
lpi_data_filtered<- clean_data(lpi_data_filtered)
lpi_data_filteredNA<- clean_data(lpi_data_filteredNA)
lpi_data_filtered0<- clean_data(lpi_data_filtered0)

mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
maskNA <- is.na(lpi_data_filtered)
mask0 <- lpi_data_filtered == 0
mask0[is.na(mask0)] <- FALSE

# simulated data matrix
#########
load(file = paste0('00RawData/simulations.RData'))
# Simulate data matrix for LPI structure
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

### Permutations
######
# NAs permutations
lpi_data_filteredNA <- lpi_data_filteredNA[, !names(lpi_data_filteredNA) %in% c('Binomial', 'ID')]
lpi_data_filteredNAPer <- permutationLPI(lpi_data_filteredNA, nperm = 300, shuffle_zeros = FALSE, shuffle_NA = TRUE , years, S)

for (i in 1:length(lpi_data_filteredNAPer)) {
  # Create subdirectory path
  subdir <- sprintf("lpi_temp/constrain/napermutations/processing/It_%d", i)
  # Create the directory if it doesn't exist
  if (!dir.exists(subdir)) {
    dir.create(subdir, recursive = TRUE)
  }
  
  # Save the RDS file in the subdirectory
  saveRDS(
    lpi_data_filteredNAPer[[i]],
    file = file.path(subdir, sprintf("matrix_%03d.rds", i))
  )
}
###########
###0s permutations
#####
lpi_data_filtered02 <- lpi_data_filtered0[, !names(lpi_data_filtered0) %in% c('Binomial', 'ID')]
lpi_data_filtered0Per <- permutationLPI(lpi_data_filtered02, nperm = 300, shuffle_zeros = TRUE, shuffle_NA = FALSE, years = years, S = S)



for (i in 1:length(lpi_data_filtered0Per)) {
  # Create subdirectory path
  subdir <- sprintf("lpi_temp/constrain/Zeropermutations/processing/It_%d", i)
  # Create the directory if it doesn't exist
  if (!dir.exists(subdir)) {
    dir.create(subdir, recursive = TRUE)
  }
  
  # Save the RDS file in the subdirectory
  saveRDS(
    lpi_data_filtered0Per[[i]],
    file = file.path(subdir, sprintf("matrix_%03d.rds", i))
  )
}