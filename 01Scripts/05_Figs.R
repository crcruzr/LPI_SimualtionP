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
f1a <- plot_lpi(lpi_result, colr = colr,  show_label = FALSE, label_name = ""); f1a

f1comb <- read.csv('04FinalData/complete/simulated/Conv_conc_lin/Conv_conc_lin.csv')
f1b <- f1comb %>%
      filter(label == 'concave')
f1b <- plot_lpi_table(f1b, colors = colors);f1b

f1c <- f1comb %>%
      filter(label == 'linear')
f1c <- plot_lpi_table(f1c, colors = colors);f1c

f1d <- f1comb %>%
      filter(label == 'convex')
f1d <- plot_lpi_table(f1d, colors = colors);f1d

plot_data2 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/convex_gapsMed.csv')
p11 <- plot_lpi_table(plot_data2, colors = colorsG, show_label = F); p11

plot_data3 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/linear_gapsMed.csv')
p12 <- plot_lpi_table(plot_data3, colors = colorsG, show_label = F);p12

plot_data2 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/Conv_gapsMed.csv')
p13 <- plot_lpi_table(plot_data2, colors = colorsG, show_label = F); p13


f1 <- (plot_spacer() | f1a| plot_spacer()) /
  (f1b | f1c| f1d) /
  (p11 | p12| p13)  &
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


ggsave(filename=paste0("05Plots/Fig1.jpeg"), f1, dpi = 300) ## plot used in the paper

##Fig 2

lpi_resultR <- read.csv('04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')
f2b <- plot_lpi(lpi_resultR, colr = colr2, show_label = FALSE);f2b

lpi_simul_real_temp <- read.csv('04FinalData/complete/simulated/real_template/real_dataSet.csv')
f2a <- plot_lpi(lpi_simul_real_temp, colr = colr, show_label = FALSE, label_name = ""); f2a

lpi_resultNA <- read.csv('04FinalData/constrain/napermutation_unique/napermutation_unique.csv')
f2d <- plot_lpi(lpi_resultNA, colr = colr, show_label = FALSE, label_name = "");f2b

lpi_resultzero <- read.csv('04FinalData/constrain/zeropermutation_unique/zeropermutation_unique.csv')
f2c <- plot_lpi(lpi_resultzero, colr = colr, show_label = FALSE, label_name = "");f2c

## Fig 2d
nf2 <-length(list.files('03processedData/constrain/Zeropermutations/results/', full.names = TRUE))
#Merge all of the iteractions
resultsPermu01 <- lapply(1:nf2, function(i) {
  filepath <- sprintf("03processedData/constrain/Zeropermutations/results/permutation_result_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  # or NA, or any placeholder for missing files
  }
})
# Convert each data frame in the list to a data.table and add years column
for (i in seq_along(resultsPermu01)) {
  setDT(resultsPermu01[[i]])  # convert in-place, no warning if already data.table
  resultsPermu01[[i]][, years := years]
}
# Plotting Fig 2d
p2e <- ggplot(
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
  ); p2e

f2 <-  (f2b| f2a) /
  (f2c | f2d) /
  (p2e |plot_spacer() )  &
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

ggsave(filename=paste0("05Plots/Fi2.jpeg"), f2, dpi = 300) ## plot used in the paper

################################################################################
## NA itaractions - additional plot to evaluate the interaction results with NA
################################################################################

# Number of permutations with NA
nper<-  length(list.files('03processedData/constrain/napermutations/results/', pattern = "It_", full.names = TRUE))
resultsPermuNA <- lapply(1:nper, function(i) {
  filepath <- sprintf("04FinalData/constrain/napermutations/results/permutation_result_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  
  }
})
# Convert each data frame in the list to a data.table and add years column
for (i in seq_along(resultsPermuNA)) {
  setDT(resultsPermuNA[[i]])  #
  resultsPermuNA[[i]][, years := years]
}
# Plotting additional figure for NA permutations
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

## END ##