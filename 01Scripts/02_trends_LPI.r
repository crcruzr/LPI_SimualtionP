library(rlpi)
library(ggplot2)
library(missMethods)
library(tidyverse)
library(RColorBrewer)

route <- '03processedData/'
source('01Scripts/Functions.r')

# Function to simulate population growth
set.seed(42)  # For reproducibility

# Trend simulation and comparison
years <- 1950:2019
x <- seq(0, 1, length.out = length(years))
vect_conv <- round(((60 * (1 - x^0.2)) + 40), 2); plot(vect_conv)
vect_linD <- round(((60 * (1 - x^1)) + 40), 2);  plot(vect_linD)
vect_conc <- round(((60 * (1 - x^5)) + 40), 2); plot(vect_conc)

# Compare different trend matrices
trend_list <- list(vect_conv, vect_linD, vect_conc)

trend_matrices <- list()

# Load and plot subset data
load(file = paste0('03processedData/Species_simulations.RData'))

# Simulate data matrix for LPI structure
S <- 32680 ## Modified to add the same number of rows in the LPI

#dimension LPI
species_data_subset <- species_data_clean[
  sample(nrow(species_data_clean), S),
  sample(ncol(species_data_clean), length(years))
]

for (i in 1:3) {
  trend_matrices[[i]] <- sweep(species_data_subset, 2, trend_list[[i]] / 100, "*")

} 

# Verify similarity between trend matrices - It should be different 
identical(trend_matrices[[1]], trend_matrices[[2]])
identical(trend_matrices[[1]], trend_matrices[[3]])
identical(trend_matrices[[2]], trend_matrices[[3]])


plot(as.numeric(as.matrix(trend_matrices[[1]][1:10, ])))
plot(as.numeric(as.matrix(trend_matrices[[2]][1:10, ])))
plot(as.numeric(as.matrix(trend_matrices[[3]][1:10, ])))

# Add ID, binomial and ID
lpi_trend_matrices <- lapply(
  trend_matrices,
  bino_id,
  years =years,
  num_species = S
)

# Simulate trends for different datasets
trend_names <- c('Convex Decrease', 'Linear Decrease', 'Concave Decrease')
high_results <- list()

dir.create("03processedData/complete/simulated/Conv_conc_lin/",
           recursive = TRUE,
           showWarnings = FALSE)


for (i in 1:length(lpi_trend_matrices)) {
  high_results[[i]] <- LPIMain(
    create_infile(lpi_trend_matrices[[i]], index_vector = TRUE, 
                  name = paste0('03processedData/complete/simulated/Conv_conc_lin/', trend_names[i]),
                  start_col_name = "X1950", end_col_name = "X2019", CUT_OFF_YEAR = 1950),
    title = trend_names[i], REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
}

# Unify plots
labels <- c("concave", "linear", "convex")
colors <- c("#558ed5", "#77933d", "#4a452a")  # your specified colors

to_plot<- out <- bind_rows(
  lapply(seq_along(high_results), function(i) {
    df <- as.data.frame(high_results[[i]])
    df$years <- 1950:2020
    df$label <- labels[i]
    df$sim <- 1
    df
  })
)


dir.create("04FinalData/complete/simulated/Conv_conc_lin/",
           recursive = TRUE,
           showWarnings = FALSE)

write.csv(to_plot, '04FinalData/complete/simulated/Conv_conc_lin/Conv_conc_lin.csv')

f1c <- plot_lpi_table(to_plot, colors = colors);f1c

write.csv()

#### Removing data 
########################


dir.create("03processedData/complete/simulated/Conv_conc_lin_Remdt/",
           recursive = TRUE,
           showWarnings = FALSE)

### Remove randomlty data
red_values <- seq(.2, .8, by =.2)
red_values <- c(red_values, 0.95)
remove_data_vect <- list()
ad<- list()

## Loop to remove data using the original
for(g in 1:length(lpi_trend_matrices)) {
    for(w in 1:length(red_values)){
    ad[[w]] <- delete_MCAR(lpi_trend_matrices[[g]][,c(1:71)], red_values[w])
    ad[[w]] <- bino_id(ad[[w]], years, S)
}
names(ad)<-red_values 
remove_data_vect[[g]] <- ad
}

#test
head(remove_data_vect[[1]][[4]]) ##Convex with less 80
head(remove_data_vect[[1]][[5]]) ##Convex with less 80

names(remove_data_vect[[1]])
names(remove_data_vect)
names(remove_data_vect) <- trend_names
length(remove_data_vect)
length(remove_data_vect[[1]])

plot(as.numeric(as.matrix(remove_data_vect[[2]][[4]][1:100, ]))) ##Linear with less 80
plot(as.numeric(as.matrix(remove_data_vect[[2]][[1]][1:100, ]))) ##Linear with less 80
plot(as.numeric(as.matrix(remove_data_vect[[2]][[5]][1:100, ]))) ##Linear with less 80

plot(as.numeric(as.matrix(remove_data_vect[[3]][[5]][1:100, ]))) ##Linear with less 80


# Remove "Binomial" column from data frames in the third level
names = c('20%', '40%', '60%', '80%', '95%')

gF<-gT <- NA

## MEGA matrix to plot the data set
for (i in 1:3) {
  g.i2 <- remove_data_vect[[i]]
  g.i2 <- lapply(g.i2, function(x) x[!(names(x) %in% c("Binomial"))])

  for (h  in 1:length(g.i2)) {
    g.il<- as.data.frame(g.i2[[h]])
    g.i.i<- bind_rows(
    g.il |> mutate(ID = paste0("spp", 1:n())) |> pivot_longer(cols = -ID, names_to = "Year", values_to = "Value") |> mutate(Trend = names[h]))
    g.i.i$Category <- trend_names[i]
    gT <- rbind(gT, g.i.i)
   }
  gF <-rbind(gF, gT)
}

gF <- gF[!is.na(gF$Trend),]

gF$Trend <- factor(gF$Trend, levels = c('20%', '40%', '60%', '80%', '95%'))

#######
## Loop to generaate the LPI with linear, concave and convex  removing 0, 20%, 40%, 60% y 80% percent of the dataset
###########

RemovingData_results <- list()
min <- list()
route<-'03processedData/complete/simulated/Conv_conc_lin_Remdt/'

start_time <- Sys.time()
for (i in 1:length(remove_data_vect)) {
x<- remove_data_vect[[i]]
    for(j in 1:length(x)){
      x.j <- as.data.frame(x[[j]])
      x.j <- x.j[c(1:10),] ## Activate to test if everything works
      min[[j]]<- LPIMain( (create_infile(x.j, index_vector = TRUE,
                                                name = paste0(route, trend_names[i],'_removing', red_values[j],'%ofData'),
                                                start_col_name = "X1950",
                                                end_col_name = "X2019", CUT_OFF_YEAR = 1950)),
                                  title = paste0(trend_names[i], '. Removing ', red_values[j],' of Data') ,
                                  REF_YEAR = 1950,
                                  PLOT_MAX = 2019,
                                  BOOT_STRAP_SIZE = 1000,
                                  VERBOSE=FALSE
      )
    }
#names(min)<-paste0('removing_', red_values[j],'%_of_Data') 
RemovingData_results[[i]] <- min
};end_time <- Sys.time()
end_time - start_time #1.030387 mins


# Create a list to store the subsets


Concave_miss_data <- c(RemovingData_results[[1]][1:5])
linear_miss_data <- c(RemovingData_results[[2]][1:5])
convex_miss_data <- c(RemovingData_results[[3]][1:5])

names(Concave_miss_data) <-  names
names(linear_miss_data) <-  names
names(convex_miss_data) <-  names
years <- 1950:2020

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
#ggsave(filename=paste0(  "04Plots/Fig1d.jpeg"), p11, dpi = 300)

# linear
plot_data3 <-map_df(seq_along(linear_miss_data), ~ 
  mutate(linear_miss_data[[.x]], sim = .x, label = missnm[.x])
)
p12 <- plot_lpi_table(plot_data3, colors = colors)
#ggsave(filename=paste0(  "04Plots/Fig1e.jpeg"), p12, dpi = 300)

plot_data4 <-map_df(seq_along(convex_miss_data), ~ 
  mutate(convex_miss_data[[.x]], sim = .x, label = missnm[.x])
)
p13 <- plot_lpi_table(plot_data4, colors = colors)
#ggsave(filename=paste0(  "04Plots/Fig1f.jpeg"), p13, dpi = 300)


### END ####
###########