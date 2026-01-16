### Scirpt to prepare the data used to create the data used in the interactions
## It creates randomnly 300 matrices with NA and zero data
# Also, to generate the trends using Zeros and NA in the simulation matrix

library(tidyverse)
library(rlpi)
library(data.table)

route <- '03processedData/'
exroute <- '05Plots/'

#functions 
source('01Scripts/Functions.r')

#Folders to be used during the process
dir.create("03processedData/constrain/Zeropermutation/results/",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("03processedData/constrain/napermutations/results/",
           recursive = TRUE,
           showWarnings = FALSE)


# Set seed for reproducibility
set.seed(42)
years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

#Real LPI data
lpi_data <- read.csv('00RawData/LPD2022_public.csv')
lpi_data_filtered <- lpi_data %>% select(34:104)
##  NA's and zero
lpi_data_filteredNAZero <- lpi_data %>% select(34:104)
## Only with NA's
lpi_data_filteredNA <- lpi_data %>% select(34:104)
#only 0's
lpi_data_filtered0 <- lpi_data %>% select(34:104)

# Assuming your data is stored in a matrix called lpi_data_filtered
lpi_data_filtered <- clean_data(lpi_data_filtered)
lpi_data_filteredNAZero <- clean_data(lpi_data_filteredNAZero)
lpi_data_filteredNA<- clean_data(lpi_data_filteredNA)
lpi_data_filtered0<- clean_data(lpi_data_filtered0)

# Create a mask for NA and zeros
mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
maskNA <- is.na(lpi_data_filtered)
mask0 <- lpi_data_filtered == 0
mask0[is.na(mask0)] <- FALSE

## simulated data matrix
load(file = '03processedData/Species_simulations.RData')
# Simulate data matrix for LPI structure
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

### Permutations ###

###################
# NAs permutations
###################

# Fill simulated data matrix with NA's in the same position as the real dataset
lpi_data_filteredNA[!maskNA] <- species_data_final[!maskNA]
identical(is.na(lpi_data_filtered), is.na(lpi_data_filteredNA))
lpi_data_filteredNA_unique <- bino_id(lpi_data_filteredNA, years, S)

dir.create("03processedData/constrain/napermutation_unique/",
           recursive = TRUE,
           showWarnings = FALSE)

# Compute LPI using simulated data and adding NA based in the real dataset
lpi_resultNA <- LPIMain(
  create_infile(lpi_data_filteredNA_unique, index_vector = TRUE, name = '03processedData/constrain/napermutation_unique/', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - NA real dataaset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

dir.create("04FinalData/constrain/napermutation_unique/",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_resultNA$years <- years
write.csv(lpi_resultNA, '04FinalData/constrain/napermutation_unique/napermutation_unique.csv')
lpi_resultNA <- read.csv('04FinalData/constrain/napermutation_unique/napermutation_unique.csv')


colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple
f2b <- plot_lpi(lpi_resultNA, colr = colr, label_name = "Simulation - \nNA with real data");f2b
ggsave(filename="05Plots/Fig2b.jpeg", f2b, dpi = 300) ## plot used in the paper

## Create different datasets adding NA
lpi_data_filteredNAPer <- permutationLPI(lpi_data_filteredNA, nperm = 300, shuffle_zeros = FALSE, shuffle_NA = TRUE , years, S)

#### Validation of matrices NA approach
######################
sum(is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]])) ## Number of rows with NA
rowSums(is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]])) ## Number of rows with NA per row
sum(is.na(lpi_data_filtered))

#Validated NA consistency randomly
identical(sum(is.na(lpi_data_filtered)), sum(is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]]))  )
identical(rowSums(is.na(lpi_data_filtered)), rowSums((is.na(lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]]))) )

### Validate if the position of NA are different to the original structurel
identical(is.na(lpi_data_filtered), is.na( lpi_data_filteredNAPer[[as.numeric(sample((length(lpi_data_filteredNAPer)), 1))]] ))

#Validate if matrices are identical
for (i in 1:length(lpi_data_filteredNAPer)) {
  # Create subdirectory path
  subdir <- sprintf("03processedData/constrain/napermutations/processing/It_%d", i)

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
###################
## Zeros permutations
###################
lpi_data_filtered0[!mask0] <- species_data_final[!mask0]

#### Zero position are identical in both matrices
identical((ifelse(is.na(lpi_data_filtered0), FALSE, lpi_data_filtered0 == 0)),
(ifelse(is.na(lpi_data_filtered), FALSE, lpi_data_filtered == 0)))

lpi_data_filtered0_unique <- bino_id(lpi_data_filtered0, years, S)

dir.create("03processedData/constrain/zeropermutation_unique/",
           recursive = TRUE,
           showWarnings = FALSE)

# Compute LPI using simulated data and adding NA based in the real dataset
lpi_resultzero <- LPIMain(
  create_infile(lpi_data_filtered0_unique, index_vector = TRUE, name = '03processedData/constrain/zeropermutation_unique/', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - NA real dataaset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

dir.create("04FinalData/constrain/zeropermutation_unique/",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_resultzero$years <- years
write.csv(lpi_resultzero, '04FinalData/constrain/zeropermutation_unique/zeropermutation_unique.csv')
lpi_resultzero <- read.csv('04FinalData/constrain/zeropermutation_unique/zeropermutation_unique.csv')

f2c <- plot_lpi(lpi_resultzero, colr = colr, label_name = "Simulation - \n Zero in real data");f2c
ggsave(filename="05Plots/Fig2c.jpeg", f2c, dpi = 300) ## plot used in the paper
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

# Save matrices
for (i in 1:length(lpi_data_filtered0Per)) {
  # Create subdirectory path
  subdir <- sprintf("03processedData/constrain/Zeropermutations/processing/It_%d", i)
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

##############
#### END #####
##############  