
library(tidyverse)
library(rlpi)
library(future)
library(future.apply)
library(data.table)


# Seed for reproducibility
set.seed(42)
#functions 
source('01Scripts/Functions.r')
years <- 1950:2020 
colr <- c("#1f77b4", "#aec7e8")  # Blue + lighter blue

###################
###NAs permutations
###################

nper<-  list.files('03processedData/constrain/napermutations/processing/', pattern = "It_", full.names = TRUE)

#Calculate all the trends, using the diferent aproaches. It takes a lot of time (days)
system.time({
  resultsPermuNA <- future_lapply(nper, function(w) { #
    process_permutation(
      w = w,
      base_path = "03processedData/constrain/napermutations/",
      title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA - permut"
    ) })
})
## Move it to the final data folder
fs::dir_copy(path = '03processedData/constrain/napermutations/results/', new_path = '04FinalData/constrain/napermutations/results/', overwrite = TRUE)
#open all the results
resultsPermuNA <- lapply(1:length(nper), function(i) {
  filepath <- sprintf("04FinalData/constrain/napermutations/results/permutation_result_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  
  }
})
# Add years column
for (i in seq_along(resultsPermuNA)) {
  setDT(resultsPermuNA[[i]])  # convert in-place, no warning if already data.table
  resultsPermuNA[[i]][, years := years]
}
# Plotting NA permutations results
padic <- ggplot(
  purrr::map_df(seq_along(resultsPermuNA), ~ mutate(resultsPermuNA[[.x]], sim = .x, label = "Permutations using \n empirical-data zeros")),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colr[2], "Permutations using \n empirical-data zeros")) +
  scale_color_manual(name = NULL, values = setNames(colr[1], "Permutations using \n empirical-data zeros")) +
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
    legend.position = "none", # c(0.75, 0.8),
    panel.grid.major.y = element_line(size = 0.4, color = "gray80"),
    panel.grid.major.x = element_line(size = 0.4, color = "gray90")
  )

padic

ggsave(filename=paste0("05Plots/Figadic.jpeg"), padic, dpi = 300) 


###################
###0s permutations
###################

#Merge all of the iteractions
nf2 <-length(list.files('03processedData/constrain/Zeropermutations/results/', full.names = TRUE))
#Calculate all the trends, using the diferent aproaches. It takes a lot of time (days)
system.time({
  resultsPermu01 <- future_lapply(nf2, function(w) { #
    process_permutation(
      w = w,
      base_path = "03processedData/constrain/Zeropermutations/",
      title_prefix = "LPI Results Simulated Data - Real Dataset - Only Zero - permut"
    ) })
})
# Move it to the final data folder
fs::dir_copy(path = '03processedData/constrain/Zeropermutations/results/', new_path = '04FinalData/constrain/Zeropermutations/results/', overwrite = TRUE)

#Merge all of the iteractions
resultsPermu01 <- lapply(1:nf2, function(i) {
  filepath <- sprintf("04FinalData/constrain/Zeropermutations/results/permutation_result_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  # or NA, or any placeholder for missing files
  }
})
# Add years column
for (i in seq_along(resultsPermu01)) {
  setDT(resultsPermu01[[i]])  # convert in-place, no warning if already data.table
  resultsPermu01[[i]][, years := years]
}
# Plotting 0s permutations results
p2d <- ggplot(
  purrr::map_df(seq_along(resultsPermu01), ~ mutate(resultsPermu01[[.x]], sim = .x, label = "Permutations using \n empirical-data zeros")),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colr[2], "Permutations using \n empirical-data zeros")) +
  scale_color_manual(name = NULL, values = setNames(colr[1], "Permutations using \n empirical-data zeros")) +
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
    legend.position = "none", # c(0.75, 0.8),
    panel.grid.major.y = element_line(size = 0.4, color = "gray80"),
    panel.grid.major.x = element_line(size = 0.4, color = "gray90")
  )

p2d
ggsave(filename=paste0("05Plots/Fig2d.jpeg"), p2d, dpi = 300) 

###########
### END ###
###########