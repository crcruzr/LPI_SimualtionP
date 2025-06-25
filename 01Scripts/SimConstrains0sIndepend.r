library(tidyverse)
library(rlpi)
library(future)
library(future.apply)
library(data.table)

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
lpi_data_filtered0 <- lpi_data %>% select(34:104)


# Assuming your data is stored in a matrix called lpi_data_filtered
lpi_data_filtered<- clean_data(lpi_data_filtered)
lpi_data_filtered0<- clean_data(lpi_data_filtered0)

mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
mask0 <- lpi_data_filtered == 0
mask0[is.na(mask0)] <- FALSE


# simulated data matrix
#########

load(file = paste0('00RawData/simulations.RData'))
# Simulate data matrix for LPI structure
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

#Add structure real data
lpi_data_filtered0[!mask0] <- species_data_final[!mask0]
lpi_data_filtered0 <- bino_id(lpi_data_filtered0, years, S)

###########
###0s permutations
#####

## parallel method
plan(multicore, workers = availableCores())
options(future.globals.maxSize = floor(251 /  availableCores()) * 1024^3) ## commented to use in computer of canada

#nos <- seq(no, by = 25, length.out = 4)
args = commandArgs(trailingOnly=TRUE)
no <- as.numeric(args[1])
nos <- seq(no, 100, by = 50)

print(paste("Simulation starts at", Sys.time()))
future_lapply(nos, function(w) {
  process_permutation(
    w, 
    base_path = "lpi_temp/constrain/Zeropermutations", 
    title_prefix = "LPI Results Simulated Data - Real Dataset - Only Zero - permut")
})
print(paste("Simulation finnished at", Sys.time()))
