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


# Assuming your data is stored in a matrix called lpi_data_filtered
lpi_data_filtered<- clean_data(lpi_data_filtered)
lpi_data_filteredNA<- clean_data(lpi_data_filteredNA)

mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
maskNA <- is.na(lpi_data_filtered)


# simulated data matrix
#########

load(file = paste0('00RawData/simulations.RData'))

# Simulate data matrix for LPI structure
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

# Add structure real data
lpi_data_filteredNA[!maskNA] <- species_data_final[!maskNA]
lpi_data_filteredNA <- bino_id(lpi_data_filteredNA, years, S)

### Permutations
######
# I did 100 permutations per approach to compare with the null model
# NAs permutations
#lpi_data_filteredNA <- lpi_data_filteredNA[, !names(lpi_data_filteredNA) %in% c('Binomial', 'ID')]
#lpi_data_filteredNAPer <- permutationLPI(lpi_data_filteredNA, nperm = 100, shuffle_zeros = FALSE, shuffle_NA = TRUE , years, S)

## parallel method
plan(multicore, workers = availableCores())
options(future.globals.maxSize = floor(251 /  availableCores()) * 1024^3) ## commented to use in computer of canada

args = commandArgs(trailingOnly=TRUE)
no <- as.numeric(args[1])
#nos <- seq(no, by = 25, length.out = 4)
#load RDS with results - only for the iteraction selected
lpi_data_filteredNAPer <- lapply(no, function(i) {
  readRDS(sprintf("lpi_temp/constrain/napermutations/matrix_%03d.rds", i))
})
#do the LPI
resultsPermuNA <- future_lapply(no, function(w) { #
    process_permutation(
      w = w,
      data_list = lpi_data_filteredNAPer,
      base_path = "lpi_temp/constrain/napermutations",
      title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA - permut")
})
