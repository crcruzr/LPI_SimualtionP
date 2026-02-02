library(rlpi)
library(ggplot2)
library(missMethods)
library(tidyverse)
library(RColorBrewer)

route <- '03processedData/'
set.seed(42)  # For reproducibility
source('01Scripts/Functions.r')

# Example simulation
sim_result <- pop_growth(N0 = 10, gen = 500, stochastic_r = TRUE, stochastic_K = TRUE, plotting = T)
# Add observation error to simulation results
obs_error <- rbinom( n = length(sim_result),  size = round(sim_result), p = 0.1)
plot((sim_result +obs_error), col = 'red',  pch = 20)

# Simulate multiple species

num_years <- 500
num_species <- 40000
#species_data <- data.frame(matrix(NA, nrow = num_species, ncol = num_years))
species_data <- matrix(NA, nrow = num_species, ncol = num_years)
ran_pop_sizes <- numeric(num_species)
#ran_pop_sizes <- NA
start_time <- Sys.time()


for (i in 1:num_species) {
    init_pop <- as.integer(10^(runif(1, min = 1.1, max = 5)))
    
    sim_result <- pop_growth(N0 = init_pop, gen = num_years - 1, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)
    
    # Add observation error
    obs_error <- rbinom( n = length(sim_result),  size = round(sim_result), p = 0.1)
    sim_result <- sim_result + obs_error
    species_data[i,] <- sim_result
    ran_pop_sizes[i] <- init_pop
    
    #ran_pop_sizes <- c(ran_pop_sizes, init_pop)
} ;end_time <- Sys.time()


end_time - start_time #37.68823 mins

# Remove time series with zero values after 200 generations
species_data2 <- as.data.frame((species_data))
species_data_clean <- species_data2 %>%
  filter(complete.cases(.[, 200:ncol(.)])) %>%
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) %>%
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))
any(species_data_clean == 0)
# Subset the cleaned data for a specific range

# Save cleaned data to disk
dir.create(paste0(route))
save(species_data_clean, file = paste0(route,'Species_simulations.RData'))

# Load and plot subset data
load(file = paste0('03processedData/Species_simulations.RData'))
plot(as.numeric(species_data_clean[sample(1:nrow(species_data_clean),1),]))

# Simulate data matrix for LPI structure
years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 35996 ## Modified to add the same number of rows in the LPI

# Select the last population generation
cx <- ncol(species_data_clean) - length(years) + 1
species_data_subset <- species_data_clean[, cx:(cx + length(years) - 1)]

# Select randomly the number of population generation randoml
species_data_subset <- species_data_subset[sample(nrow(species_data_subset), S),]

plot(as.numeric(species_data_subset[sample(1:nrow(species_data_subset),1),]))
species_data_subset<- bino_id(species_data_subset, years, S)

dir.create("03processedData/complete/simulated/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

# Compute LPI using simulated data with the full dataset
lpi_result <- LPIMain(
  create_infile(species_data_subset, index_vector = TRUE, name = '03processedData/complete/simulated/Complete_DataSet', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Full Dataset', REF_YEAR = 1950, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

ggplot_lpi(lpi_result)

colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple

lpi_result$years <- c(years, 2021)
f1a <- plot_lpi(lpi_result, colr = colr, label_name = "Simulation Data");f1a
ggsave(filename=paste0("05Plots/Fig1a.jpeg"), f1a, dpi = 300) ## plot used in the paper

dir.create("04FinalData/complete/simulated/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

write.csv(lpi_result, '04FinalData/complete/simulated/Complete_dataSet/Complete_dataSet.csv')
lpi_result <- read.csv('04FinalData/complete/simulated/Complete_dataSet/Complete_dataSet.csv')

# Read and process real LPI data
###################################

#To process it you should download the LPD data from the LPI website https://www.livingplanetindex.org/data_portal and save it in the folder 00RawData.
# The file name should be adjusted if it is different

lpi_data <- read.csv('00RawData/LPD_2024_public.csv') #inclde in this folder the LPD data

dir.create("03processedData/complete/real/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_resultR <- LPIMain(
  create_infile(lpi_data, index_vector = TRUE, name = '03processedData/complete/real/Complete_dataSet', 
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Real Data', REF_YEAR = 1950, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_resultR$years <- c(years, 2021)

colr2 <- c("#ff7f0e", "#ffbb78")  # Orange + lighter orange
f1b <- plot_lpi(lpi_resultR, colr = colr2, label_name = "Living Planet \n Database");f1b

ggsave(filename=paste0("05Plots/Fig1b.jpeg"), f1b, dpi = 300) ## plot used in the paper

dir.create("04FinalData/complete/real/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

write.csv(lpi_resultR, '04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')
lpi_resultR <- read.csv('04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')

###############################
### Variation in the simulation
################################

lpi_data_filtered <- lpi_data %>% select(matches("^X[0-9]")) #only years

# Remove double quotes and convert non-"NULL" values to numeric
lpi_data_filtered<- clean_data(lpi_data_filtered)

# Resample simulated data to match real data dimensions
load(file = paste0('03processedData/Species_simulations.RData'))

# Select randomly the population generation
cx <- ncol(species_data_clean) - length(years) + 1
species_data_subset <- species_data_clean[, cx:(cx + length(years) - 1)]

# Select randomly the number of population generation
species_data_subset <- species_data_subset[sample(nrow(species_data_subset), nrow(lpi_data_filtered)),]
plot(as.numeric(species_data_subset[sample(1:nrow(species_data_subset),1),]))

# Add simulated data to real data structure
plot(as.numeric(lpi_data_filtered[80,]))
mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
lpi_data_filtered[!mask] <- species_data_subset[!mask]
lpi_data_filtered <- bino_id(lpi_data_filtered, years, S)
plot(as.numeric(lpi_data_filtered[80,c(1:71)]))

plot(as.numeric(lpi_data_filtered[sample(1:nrow(lpi_data_filtered),1),]))
any(lpi_data_filtered == 0)

# Compute LPI with the combined dataset 
dir.create("03processedData/complete/simulated/real_template",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_simul_real_temp <- LPIMain(
  create_infile(lpi_data_filtered, index_vector = TRUE, name = '03processedData/complete/simulated/real_template',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - real Template', REF_YEAR = 1950, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

dir.create("04FinalData/complete/simulated/real_template",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_simul_real_temp$years <- c(years, 2021)

write.csv(lpi_simul_real_temp, '04FinalData/complete/simulated/real_template/real_dataSet.csv')

f2a <- plot_lpi(lpi_simul_real_temp, colr = colr, show_label = FALSE, label_name = "") ;f2a
ggsave(filename=paste0("05Plots/Fig2a.jpeg"), f2a, dpi = 300) ## plot used in the paper
lpi_simul_real_temp <- read.csv('04FinalData/complete/simulated/real_template/real_dataSet.csv')

###########
### END ###
###########