
library(rlpi)
library(tidyverse)

set.seed(42)  # For reproducibility
source('01Scripts/Functions.r')

# Simulate multiple species
num_years <- 500
num_species <- 60000
species_data <-species_data10 <- species_data90 <- data.frame(matrix(NA, nrow = num_species, ncol = num_years))

### Structure Real Data
years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI
route <- '03OutData/'

### Not variation
######################

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
species_data_clean2 <- species_data2 |>
  filter(complete.cases(.[, 200:ncol(.)])) |>
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) |>
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))

# Subset the cleaned data for a specific range

# Save cleaned data to disk
save(species_data_clean2, file = paste0('00RawData/simulations0.RData'))

## Compute LPI with the combined dataset ##
species_data_final0 <- species_data_clean2[sample(nrow(species_data_clean2), S), sample(ncol(species_data_clean2), length(years))]
species_data_final0 <- bino_id(species_data_final0, years, S)

lpi_simul_00 <- LPIMain(
  create_infile(species_data_final0, index_vector = TRUE, name = 'lpi_temp/complete/simulated/Complete_0error',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data \n without sampling error', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple
lpi_simul_00$years <- years

f8 <- plot_lpi(lpi_simul_00, colr = colr, label_name = "Simulation - 10% error")
f8


### 10% variation
########################

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
species_data_clean <- species_data2 |>
  filter(complete.cases(.[, 200:ncol(.)])) |>
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) |>
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))

# Subset the cleaned data for a specific range

# Save cleaned data to disk
dir.create(paste0(route, 'SimDat_real_Shape/'))
save(species_data_clean, file = paste0('00RawData/simulations10.RData'))

## Compute LPI with the combined dataset ##
species_data_final <- species_data_clean[sample(nrow(species_data_clean), S), sample(ncol(species_data_clean), length(years))]
species_data_final <- bino_id(species_data_final, years, S)

lpi_simul_10 <- LPIMain(
  create_infile(species_data_final, index_vector = TRUE, name = 'lpi_temp/complete/simulated/Complete_10error',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - 10% sampling error', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple
lpi_simul_10$years <- years

f9 <- plot_lpi(lpi_simul_10, colr = colr, label_name = "Simulation - 10% error")
f9

### 90% variation
#######################

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
species_data_clean3 <- species_data2 |>
  filter(complete.cases(.[, 200:ncol(.)])) |>
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) |>
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))

# Subset the cleaned data for a specific range
# Save cleaned data to disk
dir.create(paste0(route, 'SimDat_real_Shape/'))
save(species_data_clean3, file = paste0('00RawData/simulations90.RData'))

## Compute LPI with the combined dataset ##
species_data_final90 <- species_data_clean3[sample(nrow(species_data_clean3), S), sample(ncol(species_data_clean3), length(years))]
species_data_final90 <- bino_id(species_data_final90, years, S)

lpi_simul_90 <- LPIMain(
  create_infile(species_data_final90, index_vector = TRUE, name = 'lpi_temp/complete/simulated/Complete_90error',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - 10% sampling error', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple
lpi_simul_90$years <- years

f10 <- plot_lpi(lpi_simul_90, colr = colr, label_name = "Simulation - 90% error")
f10

#################################################
#### Using the Falko T. Buschke et al trends ####
#################################################

# Trend simulation and comparison
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

# Verify similarity between trend matrices - They should be different 
identical(trend_matrices[[1]], trend_matrices[[2]])
identical(trend_matrices[[1]], trend_matrices[[3]])
identical(trend_matrices[[2]], trend_matrices[[3]])

lpi_data <- read.csv('00RawData/LPD2022_public.csv')
lpi_data_filtered <- lpi_data |> select(34:104)
# Update by removing the 'ID' and  binomial column 
lpi_data_filtered <- lpi_data_filtered[, !names(lpi_data_filtered) %in% c('Binomial', 'ID')]

# Create final datasets with trends
lpi_trend_matrices <- rep(list(lpi_data_filtered),3)
mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0

for (i in 1:3) {
  lpi_trend_matrices[[i]][!mask] <- trend_matrices[[i]][!mask]
}

# Add species identifiers and finalize datasets
for (i in 1:3) {
  lpi_trend_matrices[[i]] <- bino_id(lpi_trend_matrices[[i]], years, S)
}
plot(as.numeric((lpi_trend_matrices[[1]][60,c(1:72)])))
points(as.numeric((lpi_trend_matrices[[2]][60,c(1:72)])), add = T, col = 'red')
points(as.numeric((lpi_trend_matrices[[3]][60,c(1:72)])), add = T, col = 'blue')

# Simulate trends for different datasets
trend_names <- c('Convex Decrease', 'Linear Decrease', 'Concave Decrease')
high_results <- list()

for (i in 1:length(lpi_trend_matrices)) {
  high_results[[i]] <- LPIMain(
    create_infile(lpi_trend_matrices[[i]], index_vector = TRUE, 
                  name = paste0('lpi_temp/complete/simulated/HV_', trend_names[i]),
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
  lpi_trend_matrices[[1]] |> mutate(ID = paste0("spp", 1:n())) |>
    pivot_longer(cols = -ID, names_to = "Year", values_to = "Value") |> mutate(Trend = "convex"),
  lpi_trend_matrices[[2]] |> mutate(ID = paste0("spp", 1:n())) |>
    pivot_longer(cols = -ID, names_to = "Year", values_to = "Value") |> mutate(Trend = "linear"),
  lpi_trend_matrices[[3]] |> mutate(ID = paste0("spp", 1:n())) |>
    pivot_longer(cols = -ID, names_to = "Year", values_to = "Value") |> mutate(Trend = "concave")
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






