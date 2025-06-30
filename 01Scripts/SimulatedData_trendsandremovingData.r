
library(rlpi)

set.seed(42)  # For reproducibility
source('01Scripts/Functions.r')

years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

load(file = paste0('00RawData/simulations.RData'))

### Increasing the sampling error 
species_data_final <- species_data_clean[sample(nrow(species_data_clean), S), sample(ncol(species_data_clean), length(years))]

plot(as.numeric(species_data_clean[sample(S, 1),]))

obs.error <- rbinom(n = length(species_data_clean), size = as.numeric(species_data_clean[sample(S, 1),]), p = 0.9) # p = Observer error values.

plot(obs.error)




## Adding the trends generated with Buskhele 



## Removing ramdomly the data


## Removing the data to Busquele