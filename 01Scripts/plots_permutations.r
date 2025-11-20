library('data.table')
library('ggplot2')
library('tidyverse')
library('purrr')

years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

## Real LPI

real <- fread('lpi_temp/complete/real/Complete_dataSet_infile_Results.txt')

real$years <- years
colr <- c("#1f77b4", "#aec7e8")  # Blue + lighter blue

pr <- plot_lpi(real, colr, label_name = 'LPI')
pr
ggsave(filename=paste0(  "04Plots/FigR0.jpeg"), pr, dpi = 300)

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


## option 1
plotI <- map_df(seq_along(resultsPermuna), ~ mutate(resultsPermuna[[.x]], sim = .x, label = "Simulation with \n NA of the Real data"))
p6 <-plot_lpi_table(plotI, colors = colr, interaction = TRUE)

## option 2
p6 <- ggplot(
  purrr::map_df(seq_along(resultsPermuna), ~ mutate(resultsPermuna[[.x]], sim = .x, label = "Simulation with \n NA of the Real data")),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colr[2], "Simulation with \n NA of the Real data")) +
  scale_color_manual(name = NULL, values = setNames(colr[1], "Simulation with \n NA of the Real data")) +
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

p6

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
plotIt <- map_df(seq_along(resultsPermu01), ~ mutate(resultsPermu01[[.x]], sim = .x, label = "Simulation with zeros \n of the Real data"))
p7 <-plot_lpi_table(plotIt, colors = colr, interaction = TRUE)


p7 <- ggplot(
  purrr::map_df(seq_along(resultsPermu01), ~ mutate(resultsPermu01[[.x]], sim = .x, label = "Simulation with zeros \n of the Real data")),
  aes(x = years, y = LPI_final, group = sim)
) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colr[2], "Simulation with zeros \n of the Real data")) +
  scale_color_manual(name = NULL, values = setNames(colr[1], "Simulation with zeros \n of the Real data")) +
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

p7

ggsave(filename=paste0( "04Plots/Fig6.jpeg"), p6, dpi = 300) 
ggsave(filename=paste0(  "04Plots/Fig7.jpeg"), p7, dpi = 300) 


## concave convex and linear

labels <- c("concave", "linear", "convex")
colors <- c("#558ed5", "#77933d", "#4a452a")  # your specified colors


high_results[[1]] <- fread('lpi_temp/complete/simulated/HV_Convex Decrease_infile_Results.txt') 
high_results[[2]] <- fread('lpi_temp/complete/simulated/HV_Linear Decrease_infile_Results.txt') 
high_results[[3]] <- fread('lpi_temp/complete/simulated/HV_Concave Decrease_infile_Results.txt') 

for (i in seq_along(high_results)) {
  setDT(high_results[[i]])  # convert in-place, no warning if already data.table
  high_results[[i]][, years := years]
}

plot_data <-map_df(seq_along(high_results), ~ 
  mutate(high_results[[.x]], sim = .x, label = labels[.x])
)

p10 <- ggplot(plot_data, aes(x = years, y = LPI_final, group = sim)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2, color = NA) +
  geom_line(aes(color = label), size = 1) +
  scale_fill_manual(name = NULL, values = setNames(colors, labels)) +
  scale_color_manual(name = NULL, values = setNames(colors, labels)) +
  geom_hline(yintercept = 1, linetype = "solid", size = 1, color = "#666666") +
  coord_cartesian(ylim = c(0.0, 2)) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10), expand = c(0.03, 0.05)) +
  scale_y_continuous(limits = c(0, 2), expand = c(0.05, 0.002)) +
  labs(x = "Year", y = "Index", title = "") +
  geom_vline(xintercept = 1950, color = "gray80", size = 0.5) +
  geom_hline(yintercept = 0, color = "gray80", size = 0.5) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    axis.title = element_text(size = 25),
    axis.text.x = element_text(size = 20, hjust = 0.5, margin = margin(t = 5)),
    axis.text.y = element_text(size = 20),
    axis.ticks.x = element_line(size = 0.8, color = "black"),
    axis.ticks.y = element_line(size = 0.8, color = "black"),
    legend.text = element_text(size = 25),
    legend.position = c(0.75, 0.8),
    panel.grid.major.y = element_line(size = 0.4, color = "gray80"),
    panel.grid.major.x = element_line(size = 0.4, color = "gray90")
  )

p10
ggsave(filename=paste0(  "04Plots/Fig10.jpeg"), p10, dpi = 300)

#### Removing Data
missnm <-c('0.2', '0.4', '0.6', '0.8', '0.95')

Concave_miss_data <- lapply((list.files('lpi_temp/complete/simulated/removingData/', pattern = 'Concave.*_infile_Results.txt', full.names = T)), fread)
linear_miss_data <- lapply((list.files('lpi_temp/complete/simulated/removingData/', pattern = 'Linear.*_infile_Results.txt', full.names = T)), fread)
convex_miss_data <- lapply((list.files('lpi_temp/complete/simulated/removingData/', pattern = 'Convex.*_infile_Results.txt', full.names = T)), fread)

names(Concave_miss_data) <-  missnm
names(linear_miss_data) <-  missnm
names(convex_miss_data) <-  missnm

for (lst in list(Concave_miss_data, linear_miss_data, convex_miss_data)) {
  for (i in seq_along(lst)) {
    setDT(lst[[i]])[, years := years]
  }
}

colors <- c("#558ed5", "#77933d", "#4a452a", "#d87c30", "#5b9aa0")

##plot concave
plot_data2 <-map_df(seq_along(Concave_miss_data), ~ 
  mutate(Concave_miss_data[[.x]], sim = .x, label = missnm[.x])
)
p11 <- plot_lpi_table(plot_data2, colors = colors)
ggsave(filename=paste0(  "04Plots/Fig11.jpeg"), p11, dpi = 300)

# linear
plot_data3 <-map_df(seq_along(linear_miss_data), ~ 
  mutate(linear_miss_data[[.x]], sim = .x, label = missnm[.x])
)
p12 <- plot_lpi_table(plot_data3, colors = colors)
ggsave(filename=paste0(  "04Plots/Fig12.jpeg"), p12, dpi = 300)

plot_data4 <-map_df(seq_along(convex_miss_data), ~ 
  mutate(convex_miss_data[[.x]], sim = .x, label = missnm[.x])
)
p13 <- plot_lpi_table(plot_data4, colors = colors)
ggsave(filename=paste0(  "04Plots/Fig13.jpeg"), p13, dpi = 300)
