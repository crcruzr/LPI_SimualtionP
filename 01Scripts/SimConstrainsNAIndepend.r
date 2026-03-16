library(tidyverse)
library(rlpi)
library(future)
library(future.apply)
library(data.table)
library(patchwork)

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

## parallel method
plan(multicore, workers = availableCores())
options(future.globals.maxSize = floor(251 /  availableCores()) * 1024^3) ## commented to use in computer of canada

args = commandArgs(trailingOnly=TRUE)
no <- as.numeric(args[1])
#nos <- no
nos <- seq(no, 300, by = 100)

print(paste("Simulation starts at", Sys.time()))

#do the LPI
future_lapply(nos, function(w) { #
    process_permutation(
      w = w,
      base_path = "lpi_temp/constrain/napermutations",
      title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA - permut")
})

print(paste("Simulation finnished at", Sys.time()))


