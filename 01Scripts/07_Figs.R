library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(rlpi)
library(data.table)

# Load functions
source('01Scripts/Functions.r')
colr <- c("#9467bd", "#c5b0d5") 
colr2 <- c("#ff7f0e", "#ffbb78") 
colors <- c("#558ed5", "#77933d", "#4a452a") 
colorsG <- c("#558ed5", "#77933d", "#4a452a", "#d87c30", "#5b9aa0")
years <- 1950:2020 ## Modified to add the same number of years in the LPI

### Fig 1 ####
lpi_result <- read.csv('04FinalData/complete/simulated/Complete_dataSet/Complete_dataSet.csv')
f1a <- plot_lpi_table(lpi_result, colr,  show_label = F); f1a
ggsave(filename=paste0("05Plots/Fig1a.jpeg"), f1a, dpi = 300) ## plot used in the paper

f1comb <- read.csv('04FinalData/complete/simulated/Conv_conc_lin/Conv_conc_lin.csv')
f1b <- f1comb %>%
  filter(label == 'concave')
f1b <- plot_lpi_table(f1b, colr = colr, show_label = F);f1b
ggsave(filename=paste0("05Plots/Fig1b.jpeg"), f1b, dpi = 300) ## plot used in the paper

f1c <- f1comb %>%
  filter(label == 'linear')
f1c <- plot_lpi_table(f1c, colr = colr, show_label = F );f1c
ggsave(filename=paste0("05Plots/Fig1c.jpeg"), f1c, dpi = 300) ## plot used in the paper

f1d <- f1comb %>%
  filter(label == 'convex')
f1d <- plot_lpi_table(f1d, colr = colr, show_label = F);f1d
ggsave(filename=paste0("05Plots/Fig1d.jpeg"), f1d, dpi = 300) ## plot used in the paper

plot_data2 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/convex_gapsMed.csv')
f1e <- plot_lpi_table(plot_data2, colr = colorsG, show_label = FALSE); f1e
ggsave(filename=paste0("05Plots/Fig1e.jpeg"), f1e, dpi = 300) ## plot used in the paper

plot_data3 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/linear_gapsMed.csv')
f1f <- plot_lpi_table(plot_data3, colr = colorsG, show_label = FALSE); f1f
ggsave(filename=paste0("05Plots/Fig1f.jpeg"), f1f, dpi = 300) ## plot used in the paper

plot_data1 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/Conv_gapsMed.csv')
f1g <- plot_lpi_table(plot_data1, colr = colorsG, show_label = FALSE); f1g
ggsave(filename=paste0("05Plots/Fig1g.jpeg"), f1g, dpi = 300) ## plot used in the paper


f1 <- (plot_spacer() | f1a| plot_spacer()) /
  (f1b | f1c| f1d) /
  (f1e | f1f| f1g)  &
  plot_annotation(tag_levels = "A")  &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    # Axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.title.x = element_text(size = 12),
    #axis.title.y = element_text(size = 12),
    # Tick labels
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    # Tick marks
    axis.ticks = element_line(size = 0.5),
    # Panel border
    panel.border = element_rect(size = 0.6),
    # Legend
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "mm"),
    legend.key.width  = unit(1, "mm")
  ) &
  guides(
    color = guide_legend(override.aes = list(size = 4))
  )
f1
ggsave(filename=paste0("05Plots/Fig1.jpeg"), f1, dpi = 300) ## plot used in the paper


##Fig 2

lpi_resultR <- read.csv('04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')
f2a <- plot_lpi_table(lpi_resultR, colr = colr2, show_label = FALSE);f2a
ggsave(filename=paste0("05Plots/Fig2a.jpeg"), f2a, dpi = 300) ## plot used in the paper

lpi_simul_real_temp <- read.csv('04FinalData/constrain/1_na_zero_permutations/without_permutation/without_permutationNAand0.csv')
f2b <- plot_lpi_table(lpi_simul_real_temp, colr = colr, show_label = FALSE, label_name = ""); f2b
ggsave(filename=paste0("05Plots/Fig2b.jpeg"), f2b, dpi = 300) ## plot used in the paper

lpi_resultzero <- read.csv('04FinalData/constrain/3_zero_permutations/without_permutation/without_permutationzero.csv')
f2c <- plot_lpi_table(lpi_resultzero, colr = colr, show_label = FALSE, label_name = "");f2c
ggsave(filename=paste0("05Plots/Fig2c.jpeg"), f2c, dpi = 300) ## plot used in the paper

lpi_resultNA <- read.csv('04FinalData/constrain/2_na_permutations/without_permutation/without_permutationNA.csv')
f2d <- plot_lpi_table(lpi_resultNA, colr = colr, show_label = FALSE, label_name = ""); f2d
ggsave(filename=paste0("05Plots/Fig2d.jpeg"), f2d, dpi = 300) ## plot used in the paper

#Merge all of the iteractions

nf1 <-length(list.files('03processedData/constrain/3_zero_permutations/simulatedData/results/', full.names = TRUE))
resultsPermu0 <- lapply(1:nf1, function(i) {
  filepath <- sprintf("03processedData/constrain/3_zero_permutations/simulatedData/results/permutation_result_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  # or NA, or any placeholder for missing files
  }
})

# Convert each data frame in the list to a data.table and add years column
for (i in seq_along(resultsPermu0)) {
  setDT(resultsPermu0[[i]])  # convert in-place, no warning if already data.table
  resultsPermu0[[i]][, years := c(years,2021)]
}

head(resultsPermu0[[1]],3)

f2e <- purrr::map_df(seq_along(resultsPermu0), ~ mutate(resultsPermu0[[.x]], sim = .x, label = "Permutations using \n empirical-data zeros"))
f2e <- lpi_multiplot(f2e, colr = colr); f2e
ggsave(filename=paste0("05Plots/Fig2e.jpeg"), f2e, dpi = 300) ## plot used in the paper

f2 <- (f2a | f2b) /
  (f2c | f2d) /
  (f2e| plot_spacer())  &
  plot_annotation(tag_levels = "A")  &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    # Axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.title.x = element_text(size = 12),
    #axis.title.y = element_text(size = 12),
    # Tick labels
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    # Tick marks
    axis.ticks = element_line(size = 0.5),
    # Panel border
    panel.border = element_rect(size = 0.6),
    # Legend
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "mm"),
    legend.key.width  = unit(1, "mm")
  ) &
  guides(
    color = guide_legend(override.aes = list(size = 4))
  )

f2

ggsave(filename=paste0("05Plots/Fig2.jpeg"), f2, dpi = 300) ## plot used in the paper

### Sup S1
nf2 <-length(list.files('03processedData/constrain/2_na_permutations/simulatedData/results/', full.names = TRUE))

resultsPermuNA <- lapply(1:nf2, function(i) {
  filepath <- sprintf("03processedData/constrain/2_na_permutations/simulatedData/results/permutation_result_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL 
  }
})

# Convert each data frame in the list to a data.table and add years column
for (i in seq_along(resultsPermuNA)) {
  setDT(resultsPermuNA[[i]])  # convert in-place, no warning if already data.table
  resultsPermuNA[[i]][, years := c(years,2021)]
}

head(resultsPermuNA[[1]],3)

# Plotting Fig Na and zero with empirical data
s1 <- purrr::map_df(seq_along(resultsPermuNA), ~ mutate(resultsPermuNA[[.x]], sim = .x, label = "Permutations using \n empirical-data NAs"))
s1 <- lpi_multiplot(s1, colr = colr); s1

ggsave(filename=paste0("05Plots/S1.jpeg"), s1, dpi = 300) 
