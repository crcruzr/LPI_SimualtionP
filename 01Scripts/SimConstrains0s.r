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
lpi_data_filtered0 <- lpi_data %>% select(34:104)


# Assuming your data is stored in a matrix called lpi_data_filtered
lpi_data_filtered<- clean_data(lpi_data_filtered)
lpi_data_filtered0<- clean_data(lpi_data_filtered0)

mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
mask0 <- lpi_data_filtered == 0
mask0[is.na(mask0)] <- FALSE


# simulated data matrix
#########

load(file = paste0('00RawData/simulations.RData'))
# Simulate data matrix for LPI structure
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]

#Add structure real data
lpi_data_filtered0[!mask0] <- species_data_final[!mask0]
lpi_data_filtered0 <- bino_id(lpi_data_filtered0, years, S)

###########
###0s permutations
#####

lpi_data_filtered02 <- lpi_data_filtered0[, !names(lpi_data_filtered0) %in% c('Binomial', 'ID')]

lpi_data_filtered0Per <- permutationLPI(lpi_data_filtered02, nperm = 100, shuffle_zeros = TRUE, shuffle_NA = FALSE, years = years, S = S)


resultsPermu0 <- future_lapply(1:length(lpi_data_filtered0Per), function(w) {
  process_permutation(
    w, 
    data_list = lpi_data_filteredNAPer,
    base_path = "lpi_temp/constrain/Zeropermutation/", 
    title_prefix = "LPI Results Simulated Data - Real Dataset - Only Zero - permut")
})


#load RDS with results
resultsPermu01 <- lapply(1:length(lpi_data_filtered0Per), function(i) {
  readRDS(sprintf("lpi_temp/constrain/Zeropermutation/results/permutation_result_%03d.rds", i))
})


for (i in seq_along(resultsPermu01)) {
  setDT(resultsPermu01[[i]])  # convert in-place, no warning if already data.table
  resultsPermu01[[i]][, years := years]
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
