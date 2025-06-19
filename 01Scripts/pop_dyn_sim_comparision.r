
library(rlpi)
library(ggplot2)
library(missMethods)
library(tidyverse)
library(RColorBrewer)

setwd('/Users/ccruzr/Library/Mobile Documents/com~apple~CloudDocs/Cristian/Documents/Estudios/Postgrado/PhD/Projects/LPI_SimualtionP')
route <- '03OutData/'

# Function to simulate population growth
pop_growth <- function(N0 = NULL, r = NULL, K = NULL, rho = 1, gen, stochastic_r = FALSE, stochastic_K = FALSE, plotting = FALSE) {
  
  # Set default values for parameters
  if (is.null(r)) r <- 0.5
  if (is.null(K)) K <- 500
  if (is.null(N0)) N0 <- 1
  
  # Initialize population size vector
  pop_size <- numeric(gen + 1)
  pop_size[1] <- N0
  
  # Run simulation over generations
  for (i in 1:gen) {
    r_curr <- ifelse(stochastic_r, rnorm(1, r, 0.1), r)
    K_curr <- ifelse(stochastic_K, rnorm(1, K, 50), K)
    pop_size[i + 1] <- as.integer(pop_size[i] * exp(r_curr * (1 - pop_size[i] / K_curr)))
  }

  # Plot results if requested
  if (plotting) {
    plot(0:gen, pop_size, type = 'l', col = 'blue', ylim = c(0, max(pop_size)),
         xlab = 'Generation', ylab = 'Population Size', main = 'Population Growth Simulation')
    legend('bottomleft', legend = c('Population'), inset = c(0.05, 0.05),
           col = c('blue'), lty = 1)
  }
  return(pop_size)
}

set.seed(42)  # For reproducibility

# Example simulation
sim_result <- pop_growth(N0 = 10, r = 0.1, K = 300, gen = 500, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)

# Add observation error to simulation results
obs_error <- rbinom(n = length(sim_result), size = sim_result, p = 0.1)
plot(scale(obs_error))

# Simulate multiple species
num_years <- 500
num_species <- 60000
species_data <- data.frame(matrix(NA, nrow = num_species, ncol = num_years))

ran_pop_sizes <- NA
start_time <- Sys.time()

# Simulate population growth for each species
for (i in 1:num_species) {
    init_pop <- as.integer(10^(runif(1, min = 1.1, max = 5)))
    
    sim_result <- pop_growth(N0 = init_pop, r = 0.5, K = as.integer((init_pop * 100) / 95), gen = num_years - 1, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)
    
    # Add observation error
    obs_error <- rbinom(n = length(sim_result), size = sim_result, p = 0.1)
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
dir.create(paste0(route, 'SimDat_real_Shape/'))
save(species_data_clean, file = paste0('00RawData/simulations.RData'))

# Load and plot subset data
load(file = paste0(getwd(), '/',  route, '03OutData/simulations.RData'))
plot(as.numeric(species_data_subset[9,]))

# Simulate data matrix for LPI structure
num_species_final <- 32680
num_years_sub = 70

species_data_subset <- scale(species_data_clean[, c(300:370)])
species_data_final <- species_data_subset[1:num_species_final,]
species_data_final[, (num_years_sub) + 1] <- paste0('Species', 1:num_species_final)
colnames(species_data_final) <- c(paste0('X', (1949 + 1:num_years_sub)), 'Binomial')
species_data_final$ID <- 1:nrow(species_data_final)

## points distribution
density_records <- as.data.frame(bind_rows(
  (species_data_final)) %>% 
  mutate(ID = paste0("spp", 1:n())) %>% 
  pivot_longer(cols = -ID, names_to = "Year", values_to = "Value"),
)

ggsave(
 ggplot(species_data_final, aes(Year, Value)) +
    geom_hex(bins = 50, color = "grey") +
    theme_minimal() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    labs(x = "Year", y = "Count"),
 filename = "04Plots/_Density_Stable_popualtion_Sim_data_Full.png", 
 width = 180, height = 120, unit = "mm", dpi = 300
)

# Compute LPI using simulated data
lpi_result <- LPIMain(
  create_infile(species_data_final, index_vector = TRUE, name = 'lpi_temp/Complete_DataSet', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Full Dataset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

ggplot_lpi(lpi_result)

# Read and process real LPI data
lpi_data <- read.csv('00RawData/LPD2022_public.csv')
lpi_data_filtered <- lpi_data %>% select(34:104)

# Remove double quotes and convert non-"NULL" values to numeric
lpi_data_filtered[, 1:ncol(lpi_data_filtered)] <- lapply(lpi_data_filtered[, 1:ncol(lpi_data_filtered)], function(x) {
  ifelse(x == "NULL", x, gsub('^"|"$', '', x))
})
lpi_data_filtered[, 1:ncol(lpi_data_filtered)] <- lapply(lpi_data_filtered[, 1:ncol(lpi_data_filtered)], function(x) {
  ifelse(x == "NULL", NA, as.numeric(x))
})

# Resample simulated data to match real data dimensions
species_data_final_resampled <- species_data_subset[sample(nrow(species_data_subset), nrow(lpi_data_filtered)), ]

lpi_data_filtered_resampled <- lpi_data_filtered

# Add simulated data to real data structure
mask <- is.na(lpi_data_filtered_resampled) | lpi_data_filtered_resampled == 0
lpi_data_filtered_resampled[!mask] <- species_data_final_resampled[!mask]


lpi_data_filtered_resampled[, (num_years_sub) + 1] <- paste0('Species', 1:num_species_final)
colnames(lpi_data_filtered_resampled) <- c(paste0('X', (1949 + 1:num_years_sub)), 'Binomial')
lpi_data_filtered_resampled$ID <- 1:nrow(lpi_data_filtered_resampled)


# Compute LPI with the combined dataset
lpi_combined_result <- LPIMain(
  create_infile(lpi_data_filtered_resampled, index_vector = TRUE, name = 'lpi_temp/Complete_DataSet',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Real Dataset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

 ggsave(
 ggplot_lpi(lpi_combined_result),
 filename = "04Plots/_Density_Stable_popualtion_Sim_data_Real_Struct.png", 
 width = 180, height = 120, unit = "mm", dpi = 300
 )

# Apply window and trends to simulated data
species_data_window <- species_data_clean[, c(250:320)]
species_data_window <- species_data_window[sample(nrow(species_data_window), nrow(lpi_data_filtered)), ]
species_data_window_resampled <- lpi_data_filtered
species_data_window_resampled[!mask] <- species_data_window[!mask]

# Create data matrix for windowed LPI
species_data_window_resampled[, ncol(species_data_window_resampled) + 1] <- paste0('Species', 1:num_species_final)
colnames(species_data_window_resampled) <- c(paste0('X', (1949 + 1:(num_years_sub + 1))), 'Binomial')
species_data_window_resampled$ID <- 1:nrow(species_data_window_resampled)

# Compute LPI with windowed data
lpi_window_result <- LPIMain(
  create_infile(species_data_window_resampled, index_vector = TRUE, name = 'lpi_temp/Complete_DataSet',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Full Dataset Winow 2', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

 ggsave(
 ggplot_lpi(lpi_window_result),
 filename = "04Plots/Windown_moved_Density_Stable_popualtion_Sim_data_Real_Struct.png", 
 width = 180, height = 120, unit = "mm", dpi = 300
 )

# Trend simulation and comparison
years <- 1950:2019
x <- seq(0, 1, length.out = length(years))
vect_conv <- round(((60 * (1 - x^0.2)) + 40), 2); plot(vect_conv)
vect_linD <- round(((60 * (1 - x^1)) + 40), 2);  plot(vect_linD)
vect_conc <- round(((60 * (1 - x^5)) + 40), 2); plot(vect_conc)

# Compare different trend matrices
trend_list <- list(vect_conv, vect_linD, vect_conc)
trend_matrices <- list()

species_data_final2 <- species_data_final[, !names(species_data_final) %in%  c('Binomial', 'ID')]

for (i in 1:3) {
  trend_matrices[[i]] <- sweep((species_data_final2), MARGIN = 2, trend_list[[i]], '+')
}

# Verify similarity between trend matrices - It should be different 
identical(trend_matrices[[1]], trend_matrices[[2]])
identical(trend_matrices[[1]], trend_matrices[[3]])
identical(trend_matrices[[2]], trend_matrices[[3]])

# Update by removing the 'ID' and  binomial column 
lpi_data_filtered_resampled <- lpi_data_filtered_resampled[, !names(lpi_data_filtered_resampled) %in% c('Binomial', 'ID')]

# Create final datasets with trends
lpi_trend_matrices <- rep(list(lpi_data_filtered_resampled),3)

for (i in 1:3) {
  lpi_trend_matrices[[i]][!mask] <- trend_matrices[[i]][!mask]
}

# Add species identifiers and finalize datasets
for (i in 1:3) {
  lpi_trend_matrices[[i]][, num_years_sub + 1] <- paste0('Species', 1:num_species_final)
  colnames(lpi_trend_matrices[[i]]) <- c(paste0('X', (1949 + 1:num_years_sub)), 'Binomial')
  lpi_trend_matrices[[i]]$ID <- 1:nrow(lpi_trend_matrices[[i]])
}

# Simulate trends for different datasets
trend_names <- c('Convex Decrease', 'Linear Decrease', 'Concave Decrease')
high_results <- list()

for (i in 1:length(lpi_trend_matrices)) {
  high_results[[i]] <- LPIMain(
    create_infile(lpi_trend_matrices[[i]], index_vector = TRUE, 
                  name = paste0('lpi_temp/', trend_names[i]),
                  start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
    title = trend_names[i], REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
} 

# Visualize high variation trends
 ggsave(
high_variation_plot <- ggplot_multi_lpi(high_results, names = trend_names, 
title = 'Simulation Trends'),
 filename = "03_figures/Simulation_different_Trends.png", 
 width = 180, height = 120, unit = "mm", dpi = 300
 )

# Prepare long-format data for visualization
df_long_comb_HV <- bind_rows(
  lpi_trend_matrices[[1]] %>% mutate(ID = paste0("spp", 1:n())) %>%
    pivot_longer(cols = -ID, names_to = "Year", values_to = "Value") %>% mutate(Trend = "convex"),
  lpi_trend_matrices[[2]] %>% mutate(ID = paste0("spp", 1:n())) %>%
    pivot_longer(cols = -ID, names_to = "Year", values_to = "Value") %>% mutate(Trend = "linear"),
  lpi_trend_matrices[[3]] %>% mutate(ID = paste0("spp", 1:n())) %>%
    pivot_longer(cols = -ID, names_to = "Year", values_to = "Value") %>% mutate(Trend = "concave")
)


#Merge trend in unique dataset
i<-3
lpi_trend_matrices[[i]][!mask] <- trend_matrices[[i]][!mask]
trend_matrices[[i]][!mask]


a<- lpi_trend_matrices[[1]]
b<- lpi_trend_matrices[[2]]
c<- lpi_trend_matrices[[3]]

a$trend <- trend_names[1]
b$trend <- trend_names[2]
c$trend <- trend_names[3]


# Define a function to sample 33% of rows from a given data frame
sample_data <- function(df) {
  size <- floor(nrow(df) * 0.33)
  df[sample(1:nrow(df), size), ]
}
# Sample 33% of rows from 'a' and 'c' (two times)
join_trends <- rbind(sample_data(a), sample_data(c), sample_data(c))

join_trends$Binomial <- 1:nrow(join_trends) #re writting binomial species to advoid duplicated spp
join_trends$ID <- 1:nrow(join_trends)

Join_lpt_trends <- LPIMain(
    create_infile(join_trends, index_vector = TRUE, 
                  name = paste0('lpi_temp/join_trends'),
                  start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
    title = 'join_trends_LPI', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
 
 ggsave(
 ggplot_lpi(Join_lpt_trends),
 filename = "04Plots/Join_thrends_Con_line_convex.png", 
 width = 180, height = 120, unit = "mm", dpi = 300
 )