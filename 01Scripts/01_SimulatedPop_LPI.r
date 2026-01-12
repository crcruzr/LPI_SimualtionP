library(rlpi)
library(ggplot2)
library(missMethods)
library(tidyverse)
library(RColorBrewer)

route <- '03processedData/'

set.seed(42)  # For reproducibility
source('01Scripts/Functions.r')

# Example simulation
sim_result <- pop_growth(N0 = 10, r = 0.1, K = 300, gen = 500, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)

# Add observation error to simulation results
obs_error <- rbinom(n = length(sim_result), size = sim_result, p = 0.1) # p=0.1 higest variation
plot(scale(obs_error))

# Simulate multiple species
num_years <- 500
num_species <- 100
species_data <- data.frame(matrix(NA, nrow = num_species, ncol = num_years))

ran_pop_sizes <- NA
start_time <- Sys.time()

# Simulate population growth for each species
for (i in 1:num_species) {
    init_pop <- as.integer(10^(runif(1, min = 1.1, max = 5)))
    
    sim_result <- pop_growth(N0 = init_pop, r = 0.5, K = as.integer((init_pop * 100) / 95), gen = num_years - 1, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)
    
    # Add observation error
    obs_error <- rbinom(n = length(sim_result), size = sim_result, p = 0.9)
    sim_result <- sim_result + obs_error
    species_data[i,] <- sim_result
    ran_pop_sizes <- c(ran_pop_sizes, init_pop)
} ;end_time <- Sys.time()

end_time - start_time #55.13609 mins

# Remove time series with zero values after 200 generations
species_data2 <- as.data.frame((species_data))
species_data_clean <- species_data2 %>%
  filter(complete.cases(.[, 200:ncol(.)])) %>%
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) %>%
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))

# Subset the cleaned data for a specific range

# Save cleaned data to disk
dir.create(paste0(route))
save(species_data_clean, file = paste0(route,'Species_simulations.RData'))

# Load and plot subset data
load(file = paste0('03processedData/Species_simulations.RData'))
plot(as.numeric(species_data_clean[9,]))

# Simulate data matrix for LPI structure
years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

#dimension LPI
species_data_subset <- species_data_clean[
  sample(nrow(species_data_clean), S),
  sample(ncol(species_data_clean), length(years))
]

plot(as.numeric(species_data_clean[1001,]))
species_data_final<- bino_id(species_data_subset, years, S)

dir.create("03processedData/complete/simulated/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

# Compute LPI using simulated data with the full dataset
lpi_result <- LPIMain(
  create_infile(species_data_final, index_vector = TRUE, name = '03processedData/complete/simulated/Complete_DataSet', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Full Dataset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

ggplot_lpi(lpi_result)

colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple
lpi_result$years <- years
f1a <- plot_lpi(lpi_result, colr = colr, label_name = "Simulation - Full Dataset")

dir.create("04FinalData/complete/simulated/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

write.csv(lpi_result, '04FinalData/complete/simulated/Complete_dataSet/Complete_dataSet.csv')

# Read and process real LPI data
###################################
lpi_data <- read.csv('00RawData/LPD2022_public.csv')

dir.create("03processedData/complete/real/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_resultR <- LPIMain(
  create_infile(lpi_data, index_vector = TRUE, name = '03processedData/complete/real/Complete_dataSet', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Real Data', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_resultR$years <- years
f1b <- plot_lpi(lpi_resultR, colr = colr, label_name = "Real Dataset")


dir.create("04FinalData/complete/real/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

write.csv(lpi_resultR, '04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')

## plot 1a y b

### Variation in the simulation
################################

lpi_data_filtered <- lpi_data %>% select(34:104) #only years

# Remove double quotes and convert non-"NULL" values to numeric
lpi_data_filtered<- clean_data(lpi_data_filtered)

# Resample simulated data to match real data dimensions
load(file = paste0('03processedData/Species_simulations.RData'))
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

# Add simulated data to real data structure
mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
lpi_data_filtered[!mask] <- species_data_final[!mask]

lpi_data_filtered <- bino_id(lpi_data_filtered, years, S)

# Compute LPI with the combined dataset 
dir.create("03processedData/complete/simulated/real_template",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_simul_real_temp <- LPIMain(
  create_infile(lpi_data_filtered, index_vector = TRUE, name = '03processedData/complete/simulated/real_template',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - real Template', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)
lpi_simul_real_temp$years <- years
f2a <- plot_lpi(lpi_simul_real_temp, colr = colr, label_name = "Simulation - Real Template")
f2a

### END ###

####### Adding only Zeros and NA in simulated Data
### Past to the constrain script
