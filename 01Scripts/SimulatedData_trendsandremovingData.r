
library(rlpi)
library(tidyverse)

set.seed(42)  # For reproducibility
source('01Scripts/Functions.r')

years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI
route <- '03OutData/'
# Simulate multiple species
num_years <- 500
num_species <- 60000
species_data <-species_data10 <- species_data90 <- data.frame(matrix(NA, nrow = num_species, ncol = num_years))

### Not variation
ran_pop_sizes0 <- NA
start_time <- Sys.time()

# Simulate population growth for each species
for (i in 1:num_species) {
  init_pop <- as.integer(10^(runif(1, min = 1.1, max = 5)))
  
  sim_result <- pop_growth(N0 = init_pop, r = 0.5, K = as.integer((init_pop * 100) / 95), gen = num_years - 1, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)
  
  # Add observation error
  obs_error <- rbinom(n = length(sim_result), size = sim_result, p = 0.0)
  sim_result <- sim_result + obs_error
  species_data[i,] <- sim_result
  ran_pop_sizes0 <- c(ran_pop_sizes0, init_pop)
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
save(species_data_clean, file = paste0('00RawData/simulations0.RData'))

### 10% variation
ran_pop_sizes10 <- NA
start_time <- Sys.time()

# Simulate population growth for each species
for (i in 1:num_species) {
  init_pop <- as.integer(10^(runif(1, min = 1.1, max = 5)))
  
  sim_result <- pop_growth(N0 = init_pop, r = 0.5, K = as.integer((init_pop * 100) / 95), gen = num_years - 1, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)
  
  # Add observation error
  obs_error <- rbinom(n = length(sim_result), size = sim_result, p = 0.0)
  sim_result <- sim_result + obs_error
  species_data10[i,] <- sim_result
  ran_pop_sizes10 <- c(ran_pop_sizes10, init_pop)
} ;end_time <- Sys.time()

end_time - start_time #55.13609 mins

# Remove time series with zero values after 200 generations
species_data2 <- as.data.frame((species_data10))
species_data_clean2 <- species_data2 %>%
  filter(complete.cases(.[, 200:ncol(.)])) %>%
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) %>%
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))

# Subset the cleaned data for a specific range

# Save cleaned data to disk
dir.create(paste0(route, 'SimDat_real_Shape/'))
save(species_data_clean2, file = paste0('00RawData/simulations10.RData'))

### 90% variation
ran_pop_sizes90 <- NA
start_time <- Sys.time()

# Simulate population growth for each species
for (i in 1:num_species) {
  init_pop <- as.integer(10^(runif(1, min = 1.1, max = 5)))
  
  sim_result <- pop_growth(N0 = init_pop, r = 0.5, K = as.integer((init_pop * 100) / 95), gen = num_years - 1, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)
  
  # Add observation error
  obs_error <- rbinom(n = length(sim_result), size = sim_result, p = 0.0)
  sim_result <- sim_result + obs_error
  species_data90[i,] <- sim_result
  ran_pop_sizes90 <- c(ran_pop_sizes90, init_pop)
} ;end_time <- Sys.time()

end_time - start_time #55.13609 mins

# Remove time series with zero values after 200 generations
species_data2 <- as.data.frame((species_data90))
species_data_clean3 <- species_data2 %>%
  filter(complete.cases(.[, 200:ncol(.)])) %>%
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) %>%
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))

# Subset the cleaned data for a specific range

# Save cleaned data to disk
dir.create(paste0(route, 'SimDat_real_Shape/'))
save(species_data_clean3, file = paste0('00RawData/simulations90.RData'))







