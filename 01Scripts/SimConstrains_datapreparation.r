library(tidyverse)
library(rlpi)
library(data.table)

route <- '03OutData/'
exroute <- '04Plots/'

#functions 
source('01Scripts/Functions.r')

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

lpi_data_filteredNA[!maskNA] <- species_data_final[!maskNA]
identical(is.na(lpi_data_filtered), is.na(lpi_data_filteredNA))

lpi_data_filteredNAPer <- permutationLPI(lpi_data_filteredNA, nperm = 300, shuffle_zeros = FALSE, shuffle_NA = TRUE , years, S)

#### Validation of matrices NA approach
######################
sum(is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]])) ## Number of rows with NA
rowSums(is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]])) ## Number of rows with NA per row
sum(is.na(lpi_data_filtered))

#Validated NA consistency
identical(sum(is.na(lpi_data_filtered)), sum(is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]]))  )
identical(rowSums(is.na(lpi_data_filtered)), rowSums((is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]]))) )

### Validate if the position of NA are different to the original structurel
identical(is.na(lpi_data_filtered), is.na( lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]] ))

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

lpi_data_filtered0[!mask0] <- species_data_final[!mask0]

#### Zero position are identical in both matrices
identical((ifelse(is.na(lpi_data_filtered0), FALSE, lpi_data_filtered0 == 0)),
(ifelse(is.na(lpi_data_filtered), FALSE, lpi_data_filtered == 0)))

lpi_data_filtered0Per <- permutationLPI(lpi_data_filtered0, nperm = 300, shuffle_zeros = TRUE, shuffle_NA = FALSE, years = years, S = S)

#### Validation of matirces Zero approach
######################
## Same number of Zeros in general
identical((sum(lpi_data_filtered == 0, na.rm = TRUE)), 
sum((lpi_data_filtered0Per[[  sample(length(lpi_data_filtered0Per),1) ]]) == 0, na.rm = TRUE))
## Same number of Zeros per row
identical((rowSums(lpi_data_filtered == 0, na.rm = TRUE)), 
rowSums((lpi_data_filtered0Per[[  sample(length(lpi_data_filtered0Per),1) ]]) == 0, na.rm = TRUE))

### Validate if the position of Zeros are different to the original structure
identical(is.na(lpi_data_filtered), 
is.na( lpi_data_filtered0Per[[as.numeric(sample((length(lpi_data_filtered0Per)), 1))]] ))

#Validate if matrices are identical
identical((ifelse(is.na(  lpi_data_filtered0Per[[ sample(length(lpi_data_filtered0Per),1)   ]]   ), FALSE, lpi_data_filtered0Per[[  sample(length(lpi_data_filtered0Per),1)    ]] == 0))     ,
(ifelse(is.na(lpi_data_filtered), FALSE, lpi_data_filtered == 0)))


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