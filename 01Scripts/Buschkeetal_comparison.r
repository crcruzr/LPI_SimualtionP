####
##
####

## Buschke et al. code -  https://doi.org/10.1038/s41559-021-01494-0 

# Set the random seed to replicate the stochastic process in the manuscript
set.seed(42)
years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

# Create a dummy variable to standardise the simulation duration between 0 and 1 (used to generate non-linear trajectories)
x <- seq(0,1,l=length(years))

# Generate random noise for the low- and high-fluctuation scenarios for the 500 species. Set the first and last values to zero so all time-series share the same start and end values.
N.low <- matrix(rnorm(n=S*length(years),mean=0,sd=1), nrow=S,ncol=length(years)) ; N.low[,c(1,length(years))] <- 0
N.high <- matrix(rnorm(n=S*length(years),mean=0,sd=7), nrow=S,ncol=length(years)) ; N.high[,c(1,length(years))] <- 0

# Here I used the same naming conventionas in Figure 1. However, here the code '50' refers to a concave-up trajectory, '100' is a linear trajectory, and '150' is a concave-down trajectory
vect_50 <- ((60*(1 - x^5)) + 40)
vect_100 <- ((60*(1 - x^1)) + 40)
vect_150 <- ((60*(1 - x^0.2)) + 40)


# Add the noise to the trajectories to make S unique populations for the low ('l') fluctuation scenario
N_50l <- as.data.frame(sweep(N.low, MARGIN=2, vect_50, '+'))
N_100l <- as.data.frame(sweep(N.low, MARGIN=2, vect_100, '+'))
N_150l <- as.data.frame(sweep(N.low, MARGIN=2, vect_150, '+'))

noise_low <- list(N_50l, N_100l, N_150l)

# Add the noise to the trajectories to make S unique populations for the high ('h') fluctuation scenario
N_50h <- sweep(N.high, MARGIN=2, vect_50, '+')
N_100h <- sweep(N.high, MARGIN=2, vect_100, '+')
N_150h <- sweep(N.high, MARGIN=2, vect_150, '+')

noise_high <- list(N_50h, N_100h, N_150h)

#Real LPI data
library(tidyverse)
library(rlpi)

setwd('/Users/ccruzr/Library/Mobile Documents/com~apple~CloudDocs/Cristian/Documents/Estudios/Postgrado/PhD/Projects/LPI_SimualtionP')
route <- '03OutData/'
lpi_data <- read.csv('00RawData/LPD2022_public.csv')
lpi_data_filtered <- lpi_data %>% select(34:104)

# Remove double quotes and convert non-"NULL" values to numeric
lpi_data_filtered[, 1:ncol(lpi_data_filtered)] <- lapply(lpi_data_filtered[, 1:ncol(lpi_data_filtered)], function(x) {
  ifelse(x == "NULL", x, gsub('^"|"$', '', x))
})
lpi_data_filtered[, 1:ncol(lpi_data_filtered)] <- lapply(lpi_data_filtered[, 1:ncol(lpi_data_filtered)], function(x) {
  ifelse(x == "NULL", NA, as.numeric(x))
})

# Simulate data matrix for LPI structure
num_species_final <- 32680
num_years_sub  = 70

# Add simulated data to real data structure
mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0

# Create final datasets with trends
lpi_trend_matrices <- rep(list(lpi_data_filtered),3)

for (i in 1:3) {
  lpi_trend_matrices[[i]][!mask] <- noise_low[[i]][!mask]
}

## plots to some lines
plot(as.numeric(noise_low[[1]][15434,]))
points(as.numeric(lpi_trend_matrices[[1]][15434,]), col = 'red', pch = 19, cex = 1.5)

plot(as.numeric(noise_low[[2]][15434,]))
points(as.numeric(lpi_trend_matrices[[2]][15434,]), col = 'red', pch = 19, cex = 1.5)

plot(as.numeric(noise_low[[3]][15434,]))
points(as.numeric(lpi_trend_matrices[[3]][15434,]), col = 'red', pch = 19, cex = 1.5)

# Add species identifiers and finalize datasets
for (i in 1:3) {
  lpi_trend_matrices[[i]][, num_years_sub + 1] <- paste0('Species', 1:num_species_final)
  colnames(lpi_trend_matrices[[i]]) <- c(paste0('X', (1949 + 1:num_years_sub)), 'Binomial')
  lpi_trend_matrices[[i]]$ID <- 1:nrow(lpi_trend_matrices[[i]])
}

trend_names <- c('Convex Decrease', 'Linear Decrease', 'Concave Decrease')
low_results_real_Struct <- list()

for (i in 1:length(lpi_trend_matrices)) {
  low_results_real_Struct[[i]] <- LPIMain(
  create_infile(lpi_trend_matrices[[i]], index_vector = TRUE, name = paste0('lpi_temp/', trend_names[i]),
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
    title = trend_names[i], REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
}

low_variation_realStruct_plot <- ggplot_multi_lpi(low_results_real_Struct, names = trend_names, 
title = 'Buschke et al. Trends LPI original Structure')

 ggsave(
low_variation_realStruct_plot, filename = "04Plots/BuschkeetalLowTrends_LPIoriginalStrucH.png", 
 width = 180, height = 120, unit = "mm", dpi = 300
 )

## Adding species and ID to process the LPI 
for (i in 1:3) {
  noise_low[[i]][, num_years_sub + 1] <- paste0('Species', 1:num_species_final)
  colnames(noise_low[[i]]) <- c(paste0('X', (1949 + 1:num_years_sub)), 'Binomial')
  noise_low[[i]]$ID <- 1:nrow(noise_low[[i]])
}

low_results_Buschke <- list()

for (i in 1:length(noise_low)) {
  low_results_Buschke[[i]] <- LPIMain(
  create_infile(noise_low[[i]], index_vector = TRUE, name = paste0('lpi_temp/', trend_names[i], "L"),
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
    title = trend_names[i], REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
}

low_variation_plot <- ggplot_multi_lpi(low_results_Buschke, names = trend_names, 
title = 'Buschke et al. Paper Trends')


## High results

# Create final datasets with trends
lpi_trend_matricesH <- rep(list(lpi_data_filtered),3)


for (i in 1:3) {
  lpi_trend_matricesH[[i]][!mask] <- noise_high[[i]][!mask]
}

## plots to some lines
plot(as.numeric(noise_high[[1]][15434,]))
points(as.numeric(lpi_trend_matricesH[[1]][15434,]), col = 'red', pch = 19, cex = 1.5)
plot(as.numeric(noise_high[[2]][15434,]))
points(as.numeric(lpi_trend_matricesH[[2]][15434,]), col = 'red', pch = 19, cex = 1.5)
plot(as.numeric(noise_high[[3]][15434,]))
points(as.numeric(lpi_trend_matricesH[[3]][15434,]), col = 'red', pch = 19, cex = 1.5)

# Add species identifiers and finalize datasets
for (i in 1:3) {
  lpi_trend_matricesH[[i]][, num_years_sub + 1] <- paste0('Species', 1:num_species_final)
  colnames(lpi_trend_matricesH[[i]]) <- c(paste0('X', (1949 + 1:num_years_sub)), 'Binomial')
  lpi_trend_matricesH[[i]]$ID <- 1:nrow(lpi_trend_matricesH[[i]])
}
high_results_real_Struct <- list()

for (i in 1:length(lpi_trend_matricesH)) {
  high_results_real_Struct[[i]] <- LPIMain(
  create_infile(lpi_trend_matricesH[[i]], index_vector = TRUE, name = paste0('lpi_temp/', trend_names[i], 'H'),
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
    title = trend_names[i], REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
}

high_variation_realStruct_plot <- ggplot_multi_lpi(high_results_real_Struct, names = trend_names, 
title = 'Buschke et al Trends LPI original Structure')

 ggsave(
high_variation_realStruct_plot, filename = "04Plots/BuschkeetalHighTrends_LPIoriginalStrucH.png", 
 width = 180, height = 120, unit = "mm", dpi = 300
 )