library(rlpi)
library(ggplot2)
library(missMethods)
library(tidyverse)
library(RColorBrewer)

route <- '03OutData/'

set.seed(42)  # For reproducibility
source('01Scripts/Functions.r')

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
load(file = paste0('00RawData/simulations.RData'))
plot(as.numeric(species_data_clean[9,]))

# Simulate data matrix for LPI structure

years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

species_data_subset <- as.data.frame(species_data_clean[sample(nrow(S), nrow(length(years))), ])
species_data_final<- bino_id(species_data_subset, years, S)

# Compute LPI using simulated data
lpi_result <- LPIMain(
  create_infile(species_data_final, index_vector = TRUE, name = 'lpi_temp/complete/simulated/Complete_DataSet', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Full Dataset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)
ggplot_lpi(lpi_result)

colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple
lpi_result$years <- years
f1S <- plot_lpi(lpi_result, colr = colr, label_name = "Simulation - Full Dataset")

# Read and process real LPI data
###################################
lpi_data <- read.csv('00RawData/LPD2022_public.csv')

lpi_resultR <- LPIMain(
  create_infile(lpi_data, index_vector = TRUE, name = 'lpi_temp/complete/real/Complete_dataSet', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Real Data', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_resultR$years <- years
f1R <- plot_lpi(lpi_resultR, colr = colr, label_name = "Real Dataset")
f1R
### Variation in the simulation
################################

lpi_data_filtered <- lpi_data %>% select(34:104)
# Remove double quotes and convert non-"NULL" values to numeric
lpi_data_filtered<- clean_data(lpi_data_filtered)

# Resample simulated data to match real data dimensions
load(file = paste0('00RawData/simulations.RData'))
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

# Add simulated data to real data structure
mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
lpi_data_filtered[!mask] <- species_data_final[!mask]

lpi_data_filtered <- bino_id(lpi_data_filtered, years, S)

# Compute LPI with the combined dataset
lpi_simul_real_temp <- LPIMain(
  create_infile(lpi_data_filtered, index_vector = TRUE, name = 'lpi_temp/complete/simulated/real_template',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - real Template', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)
lpi_simul_real_temp$years <- years
f1SR <- plot_lpi(lpi_simul_real_temp, colr = colr, label_name = "Simulation - Real Template")
f1SR

# Apply window and trends to simulated data
species_data_window <- species_data_clean[, c(400:470)]
species_data_window <- species_data_window[sample(nrow(species_data_window), nrow(lpi_data_filtered)), ]
species_data_window_resampled <- lpi_data_filtered[,-c(72,73)]
species_data_window_resampled[!mask] <- species_data_window[!mask]

# Create data matrix for windowed LPI
species_data_window_resampled <- bino_id(species_data_window_resampled, years, S)

# Compute LPI with windowed data
lpi_window_result <- LPIMain(
  create_infile(species_data_window_resampled, index_vector = TRUE, name = 'lpi_temp/complete/simulated/real_templateII',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - real Template 2', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_window_result$years <- years
f1SR2 <- plot_lpi(lpi_window_result, colr = colr, label_name = "Simulation - Real Template II")
f1SR2

















####### Code to evaluatae

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
lpi_data_filtered <- lpi_data_filtered[, !names(lpi_data_filtered) %in% c('Binomial', 'ID')]

# Create final datasets with trends
lpi_trend_matrices <- rep(list(lpi_data_filtered),3)

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