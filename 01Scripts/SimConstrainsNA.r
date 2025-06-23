library(tidyverse)
library(rlpi)
library(future)
library(future.apply)
library(data.table)
library(patchwork)

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
## Only with NA's
lpi_data_filteredNA <- lpi_data %>% select(34:104)


# Assuming your data is stored in a matrix called lpi_data_filtered
lpi_data_filtered<- clean_data(lpi_data_filtered)
lpi_data_filteredNA<- clean_data(lpi_data_filteredNA)

mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
maskNA <- is.na(lpi_data_filtered)


# simulated data matrix
#########

load(file = paste0('00RawData/simulations.RData'))

# Simulate data matrix for LPI structure
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

# Add structure real data
lpi_data_filteredNA[!maskNA] <- species_data_final[!maskNA]
lpi_data_filteredNA <- bino_id(lpi_data_filteredNA, years, S)

### Permutations
######
# I did 100 permutations per approach to compare with the null model
# NAs permutations
lpi_data_filteredNA <- lpi_data_filteredNA[, !names(lpi_data_filteredNA) %in% c('Binomial', 'ID')]
lpi_data_filteredNAPer <- permutationLPI(lpi_data_filteredNA, nperm = 100, shuffle_zeros = FALSE, shuffle_NA = TRUE , years, S)

## parallel method
plan(multicore, workers = availableCores())
options(future.globals.maxSize = floor(251 /  availableCores()) * 1024^3) ## commented to use in computer of canada

system.time({
  resultsPermuNA <- future_lapply(1:length(lpi_data_filteredNAPer), function(w) { #
    process_permutation(
      w = w,
      data_list = lpi_data_filteredNAPer,
      base_path = "lpi_temp/constrain/napermutations",
      title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA - permut"
    ) })
})


for (i in seq_along(resultsPermuNA)) {
  setDT(resultsPermuNA[[i]])  # convert in-place, no warning if already data.table
  resultsPermuNA[[i]][, years := years]
}

colr <- c("#1f77b4", "#aec7e8")  # Blue + lighter blue

p4b <- ggplot(
  purrr::map_df(seq_along(resultsPermuNA), ~ mutate(resultsPermuNA[[.x]], sim = .x, label = "Simulation with NA of the Real data")),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colr[2], "Simulation with NA of the Real data")) +
  scale_color_manual(name = NULL, values = setNames(colr[1], "Simulation with NA of the Real data")) +
  geom_hline(yintercept = 1, linetype = "solid", size = 1, color = "#666666") +
  coord_cartesian(ylim = c(0.0, 2)) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10), expand = c(0.03, 0.05)) +
  scale_y_continuous(limits = c(0, 2), expand = c(0.05, 0.002)) +
  guides(color = guide_legend(override.aes = list(fill = colr[2], color = colr[1], size = 15, alpha = 0.2))) +
  labs(x = "Year", y = "Index", title = "") +
  geom_vline(xintercept = 1950, color = "gray80", size = 0.5) +
  geom_hline(yintercept = 0, color = "gray80", size = 0.5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    axis.title = element_text(size = 25),
    axis.text.x = element_text(size = 20, hjust = 0.5, margin = margin(t = 5), angle = 0),
    axis.text.y = element_text(size = 20),
    axis.ticks.x = element_line(size = 0.8, color = "black"),
    axis.ticks.y = element_line(size = 0.8, color = "black"),
    legend.text = element_text(size = 25),
    legend.position = c(0.75, 0.8),
    panel.grid.major.y = element_line(size = 0.4, color = "gray80"),
    panel.grid.major.x = element_line(size = 0.4, color = "gray90")
  )

p4b

ggsave(filename=paste0( exroute, "/Fig4.jpeg"), p4b, dpi = 300) 
