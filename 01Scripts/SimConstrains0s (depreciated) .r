
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
#only 0's
lpi_data_filtered0 <- lpi_data %>% select(34:104)

# Assuming your data is stored in a matrix called lpi_data_filtered
lpi_data_filtered<- clean_data(lpi_data_filtered)
lpi_data_filteredNA<- clean_data(lpi_data_filteredNA)
lpi_data_filtered0<- clean_data(lpi_data_filtered0)

mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
maskNA <- is.na(lpi_data_filtered)
mask0 <- lpi_data_filtered == 0
mask0[is.na(mask0)] <- FALSE

table(mask)
table(mask0)
table(maskNA)

# simulated data matrix
#########

load(file = paste0('00RawData/simulations.RData'))
plot(as.numeric(species_data_clean[80,]))

# Simulate data matrix for LPI structure
species_data_final <- species_data_clean[sample(nrow(species_data_clean), nrow(lpi_data_filtered)), sample(ncol(species_data_clean), ncol(lpi_data_filtered))]
plot(as.numeric(species_data_final[870,]))

# Add simulated data to real data structure

# Create final datasets with trends
# Add simulated data to real data structure
lpi_data_filtered[!mask] <- species_data_final[!mask]
## Add binomial and ID columns

lpi_data_filtered <- bino_id(lpi_data_filtered, years, S)
lpi_data_filtered[, (length(years)) + 1] <- paste0('Species', 1:S)
colnames(lpi_data_filtered) <- c(paste0('X', years), 'Binomial')
lpi_data_filtered$ID <- 1:nrow(lpi_data_filtered)

# Compute LPI with the combined dataset
lpi_combined_result <- LPIMain(
  create_infile(lpi_data_filtered, index_vector = TRUE, name = 'lpi_temp/constrain/Complete_DataSet',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Real Dataset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)
lpi_combined_result$years <- years

colr <- c("#9467bd", "#c5b0d5")  # Purple + lighter purple

p1 <- plot_lpi(data = lpi_combined_result, colr = colr, label_name = "Simulation with real data")
p1

ggsave(filename=paste0( exroute, "/Fig1.jpeg"), p1, dpi = 300) 

# Add structure real data
lpi_data_filteredNA[!maskNA] <- species_data_final[!maskNA]
lpi_data_filteredNA <- bino_id(lpi_data_filteredNA, years, S)

# Compute LPI with the combined dataset
lpi_combined_resultNA <- LPIMain(
  create_infile(lpi_data_filteredNA, index_vector = TRUE, name = 'lpi_temp/constrain/Complete_DataSetNAonly',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Real Dataset - )nly NA', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

ggplot_lpi(lpi_combined_resultNA)
lpi_combined_resultNA$years <- years
p2 <- plot_lpi(data = lpi_combined_resultNA, colr = colr, label_name = "Simulation with NA in Real data")
p2
ggsave(filename=paste0( exroute, "/Fig2.jpeg"), p2, dpi = 300) 

#Add structure real data
lpi_data_filtered0[!mask0] <- species_data_final[!mask0]
lpi_data_filtered0 <- bino_id(lpi_data_filtered0, years, S)

# Compute LPI with the combined dataset
lpi_combined_result0 <- LPIMain(
  create_infile(lpi_data_filtered0, index_vector = TRUE, name = 'lpi_temp/constrain/Complete_DataSet0sonly',
                start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Real Dataset - Only 0s', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)
lpi_combined_result0 <- data.table::fread('lpi_temp/constrain/Complete_DataSet0sonly_infile_Results.txt')
names(lpi_combined_result0)[1] <- 'years'
ggplot_lpi(lpi_combined_result0)

p3 <- plot_lpi(data = lpi_combined_result0, colr = colr, label_name = "Simulation with Zeros of the Real data")
p3
ggsave(filename=paste0( exroute, "/Fig3.jpeg"), p3, dpi = 300) 

### Permutations
######
# I did 100 permutations per approach to compare with the null model

# NAs permutations
lpi_data_filteredNA <- lpi_data_filteredNA[, !names(lpi_data_filteredNA) %in% c('Binomial', 'ID')]
lpi_data_filteredNAPer <- permutationLPI(lpi_data_filteredNA, nperm = 100, shuffle_zeros = FALSE, shuffle_NA = TRUE , years, S)

table(is.na(lpi_data_filteredNAPer[[1]]))
table(is.na(lpi_data_filteredNAPer[[100]]))

identical(lpi_data_filteredNAPer[[sample(1:100, 1)]], lpi_data_filteredNAPer[[sample(1:100, 1)]])

##parallel methods
for (i in 1:length(lpi_data_filteredNAPer)) {
  saveRDS(lpi_data_filteredNAPer[[i]], file = sprintf("lpi_temp/constrain/napermutations/matrix_%03d.rds", i))
}

parallel::detectCores() #workers
#round(as.numeric(gsub("[^0-9]", "", system("grep MemTotal /proc/meminfo", intern = TRUE))) / 1024^2, 2) # In gb

plan(multicore, workers = availableCores())
#options(future.globals.maxSize = floor(251 /  availableCores()) * 1024^3) ## commented to use in computer of canada

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
head(resultsPermu01[[1]])

#resultsPermuNA <- lapply(sprintf("lpi_temp/constrain/napermutations/NApermutation%d_infile_Results.txt", 1:100), data.table::fread) #to load dataflies
#lapply(resultsPermuNA, setnames, old = "V1", new = "years")
ggplot_lpi(resultsPermuNA[[1]])
ggplot_lpi(resultsPermuNA[[100]])
ggplot_multi_lpi(resultsPermuNA[90:100])+
  theme(legend.position = "none")
ggplot_multi_lpi(resultsPermuNA)+
  theme(legend.position = "none")

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

###########
###0s permutations
#####

lpi_data_filtered02 <- lpi_data_filtered0[, !names(lpi_data_filtered0) %in% c('Binomial', 'ID')]

#check to copmare the true 0's
table(lpi_data_filtered == 0)
table(lpi_data_filtered02 == 0)

lpi_data_filtered0Per <- permutationLPI(lpi_data_filtered02, nperm = 100, shuffle_zeros = TRUE, shuffle_NA = FALSE, years = years, S = S)

table(lpi_data_filtered0Per[[1]] == 0)
table(lpi_data_filtered0Per[[2]] == 0)
table(lpi_data_filtered0Per[[34]] == 0)
identical(lpi_data_filtered0Per[[sample(1:100, 1)]], lpi_data_filtered0Per[[sample(1:100, 1)]])

## parallel method
for (i in 1:length(lpi_data_filtered0Per)) {
  saveRDS(lpi_data_filtered0Per[[i]], file = sprintf("lpi_temp/constrain/Zeropermutation/matrix_%03d.rds", i))
}

plan(multicore, workers = availableCores())
#options(future.globals.maxSize = floor(251 /  availableCores()) * 1024^3) ## commented to use in computer of canada

resultsPermu0 <- future_lapply(1:length(lpi_data_filtered0Per), function(w) {
  process_permutation(
    w, 
    data_list = lpi_data_filteredNAPer,
    base_path = "lpi_temp/constrain/Zeropermutation/", 
    title_prefix = "LPI Results Simulated Data - Real Dataset - Only Zero - permut")
})

#lpi_data_filtered0Per <- vector("list", n_permu)

#load RDS with results
resultsPermu01 <- lapply(1:length(lpi_data_filtered0Per), function(i) {
  readRDS(sprintf("lpi_temp/constrain/Zeropermutation/results/permutation_result_%03d.rds", i))
})
ggplot_lpi(resultsPermu01[[1]])

ggplot_multi_lpi(resultsPermu01)+
  theme(legend.position = "none")


for (i in seq_along(resultsPermu01)) {
  setDT(resultsPermu01[[i]])  # convert in-place, no warning if already data.table
  resultsPermu01[[i]][, years := years]
}
head(resultsPermu01[[1]])


colr <- c("#1f77b4", "#aec7e8")  # Blue + lighter blue
ggplot(
  purrr::map_df(seq_along(resultsPermu01), ~ mutate(resultsPermu01[[.x]], sim = .x)),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = colr[2], alpha = 0.2, color = NA) +
  geom_line(color = colr[1],  size = 1) +
  geom_hline(yintercept = 1, linetype = "solid", size = 1, color = "#666666") +
  coord_cartesian(ylim = c(0.65, 1.7)) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5), expand = c(0, 0)) +
  theme_minimal() +
  labs(x = "Year", y = "Value", title = "Simulated data, Zero remplacing")+
  theme(
    axis.line.x = element_line(color = "black", size = 0.8),  # X axis line
    axis.line.y = element_line(color = "black", size = 0.8)   # Y axis line
  )


p5 <- ggplot(
  purrr::map_df(seq_along(resultsPermu01), ~ mutate(resultsPermu01[[.x]], sim = .x, label = "Simulation with Zeros of the Real data")),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colr[2], "Simulation with Zeros of the Real data")) +
  scale_color_manual(name = NULL, values = setNames(colr[1], "Simulation with Zeros of the Real data")) +
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

ggsave(filename=paste0( exroute, "/Fig5.jpeg"), p5, dpi = 300) 