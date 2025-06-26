library('data.table')
library('ggplot2')
library('tidyverse')

years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

## NA
#load RDS with results
nf <-length(list.files('lpi_temp/constrain/napermutations/results/'))

#Merge all of the iteractions
resultsPermuna <- lapply(1:nf, function(i) {
  filepath <- sprintf("lpi_temp/constrain/napermutations/results/permutation_result_%03d.rds", i)
  
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  # or NA, or any placeholder for missing files
  }
})

for (i in seq_along(resultsPermuna)) {
  setDT(resultsPermuna[[i]])  # convert in-place, no warning if already data.table
  resultsPermuna[[i]][, years := years]
}

colr <- c("#1f77b4", "#aec7e8")  # Blue + lighter blue

p4 <- ggplot(
  purrr::map_df(seq_along(resultsPermuna), ~ mutate(resultsPermuna[[.x]], sim = .x, label = "Simulation with NA of the Real data")),
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

p4

###########################
#load RDS with results

nf2 <-length(list.files('lpi_temp/constrain/Zeropermutations/results/'))

#Merge all of the iteractions
resultsPermu01 <- lapply(1:nf2, function(i) {
  filepath <- sprintf("lpi_temp/constrain/Zeropermutations/results/permutation_result_%03d.rds", i)
  
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  # or NA, or any placeholder for missing files
  }
})

for (i in seq_along(resultsPermu01)) {
  setDT(resultsPermu01[[i]])  # convert in-place, no warning if already data.table
  resultsPermu01[[i]][, years := years]
}

p5 <- ggplot(
  purrr::map_df(seq_along(resultsPermu01), ~ mutate(resultsPermu01[[.x]], sim = .x, label = "Simulation with zero of the Real data")),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colr[2], "Simulation with zero of the Real data")) +
  scale_color_manual(name = NULL, values = setNames(colr[1], "Simulation with zero of the Real data")) +
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

p5

ggsave(filename=paste0( "04Plots/Fig5_1.jpeg"), p5, dpi = 300) 
ggsave(filename=paste0(  "04Plots/Fig4_1.jpeg"), p4, dpi = 300) 




