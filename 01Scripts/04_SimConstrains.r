
library(tidyverse)
library(rlpi)
library(future)
library(future.apply)
library(data.table)

#functions 
source('01Scripts/Functions.r')

# Set the random seed to replicate the stochastic process in the manuscript
set.seed(42)
years <- 1950:2020 ## Modified to add the same number of years in the LPI
S <- 32680 ## Modified to add the same number of rows in the LPI

parallel::detectCores() #workers
#round(as.numeric(gsub("[^0-9]", "", system("grep MemTotal /proc/meminfo", intern = TRUE))) / 1024^2, 2) # In gb

plan(multicore, workers = availableCores())
#options(future.globals.maxSize = floor(251 /  availableCores()) * 1024^3) ## commented to use in computer of canada

nper<-  length(list.files('03processedData/constrain/napermutations/processing/', pattern = "It_", full.names = TRUE))
nper<-4

#it takes time
system.time({
  resultsPermuNA <- future_lapply(1:nper, function(w) { #
    process_permutation(
      w = w,
      base_path = "03processedData/constrain/napermutations/",
      title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA - permut"
    ) })
})

#load RDS with results
resultsPermuNA2 <- lapply(1:nper, function(i) {
  readRDS(sprintf("03processedData/constrain/napermutations/results/permutation_result_%03d.rds", i))
})

ggplot_lpi(resultsPermuNA[[1]])

ggplot_multi_lpi(resultsPermuNA)+
  theme(legend.position = "none")

ggsave(filename=paste0( exroute, "/Fig4.jpeg"), p4b, dpi = 300) 

###########
###0s permutations
#####
nper<-  length(list.files('03processedData/constrain/napermutations/processing/', pattern = "It_", full.names = TRUE))

resultsPermu0 <- future_lapply(1:length(nper), function(w) {
  process_permutation(
    w, 
    data_list = lpi_data_filteredNAPer,
    base_path = "03processedData/constrain/napermutations/",
    title_prefix = "LPI Results Simulated Data - Real Dataset - Only Zero - permut")
})

#lpi_data_filtered0Per <- vector("list", n_permu)

#load RDS with results
resultsPermu0 <- lapply(1:nper, function(i) {
  readRDS(sprintf("lpi_temp/constrain/Zeropermutation/results/permutation_result_%03d.rds", i))
})

ggplot_lpi(resultsPermu0[[1]])

ggplot_multi_lpi(resultsPermu0)+
  theme(legend.position = "none")

ggsave(filename=paste0( exroute, "/Fig5.jpeg"), p5, dpi = 300) 