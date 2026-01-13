library(rlpi)
library(ggplot2)
library(missMethods)
library(tidyverse)
library(RColorBrewer)

route <- '03processedData/'
source('01Scripts/Functions.r')

# Function to simulate population growth
set.seed(42)  # For reproducibility

# Trend simulation and comparison
years <- 1950:2019
x <- seq(0, 1, length.out = length(years))
vect_conv <- round(((60 * (1 - x^0.2)) + 40), 2); plot(vect_conv)
vect_linD <- round(((60 * (1 - x^1)) + 40), 2);  plot(vect_linD)
vect_conc <- round(((60 * (1 - x^5)) + 40), 2); plot(vect_conc)

# Compare different trend matrices
trend_list <- list(vect_conv, vect_linD, vect_conc)

trend_matrices <- list()

# Load and plot subset data
load(file = paste0('03processedData/Species_simulations.RData'))

# Simulate data matrix for LPI structure
S <- 32680 ## Modified to add the same number of rows in the LPI

#dimension LPI
species_data_subset <- species_data_clean[
  sample(nrow(species_data_clean), S),
  sample(ncol(species_data_clean), length(years))
]

for (i in 1:3) {
  trend_matrices[[i]] <- sweep(species_data_subset, 2, trend_list[[i]] / 100, "*")

} 

# Verify similarity between trend matrices - It should be different 
identical(trend_matrices[[1]], trend_matrices[[2]])
identical(trend_matrices[[1]], trend_matrices[[3]])
identical(trend_matrices[[2]], trend_matrices[[3]])


plot(as.numeric(as.matrix(trend_matrices[[1]][1:10, ])))
plot(as.numeric(as.matrix(trend_matrices[[2]][1:10, ])))
plot(as.numeric(as.matrix(trend_matrices[[3]][1:10, ])))

# Add ID, binomial and ID
lpi_trend_matrices <- lapply(
  trend_matrices,
  bino_id,
  years =years,
  num_species = S
)

# Simulate trends for different datasets
trend_names <- c('Convex Decrease', 'Linear Decrease', 'Concave Decrease')
high_results <- list()

dir.create("03processedData/complete/simulated/Conv_conc_lin/",
           recursive = TRUE,
           showWarnings = FALSE)


for (i in 1:length(lpi_trend_matrices)) {
  high_results[[i]] <- LPIMain(
    create_infile(lpi_trend_matrices[[i]], index_vector = TRUE, 
                  name = paste0('03processedData/complete/simulated/Conv_conc_lin/', trend_names[i]),
                  start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
    title = trend_names[i], REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
}

# Unify plots
labels <- c("concave", "linear", "convex")
colors <- c("#558ed5", "#77933d", "#4a452a")  # your specified colors

to_plot<- out <- bind_rows(
  lapply(seq_along(high_results), function(i) {
    df <- as.data.frame(high_results[[i]])
    df$years <- 1950:2020
    df$label <- labels[i]
    df$sim <- 1
    df
  })
)


dir.create("04FinalData/complete/simulated/Conv_conc_lin/",
           recursive = TRUE,
           showWarnings = FALSE)

write.csv(to_plot, '04FinalData/complete/simulated/Conv_conc_lin/Conv_conc_lin.csv')

f1c <- plot_lpi_table(to_plot, colors = colors);f1c

write.csv()
### END ####
###########