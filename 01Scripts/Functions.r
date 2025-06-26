##Functions
# Function to simulate population growth
pop_growth <- function(N0 = NULL, r = NULL, K = NULL, rho = 1, gen, stochastic_r = FALSE, stochastic_K = FALSE, plotting = FALSE) {
  
  # Set default values for parameters
  if (is.null(r)) r <- 0.5
  if (is.null(K)) K <- 500
  if (is.null(N0)) N0 <- 1
  
  # Initialize population size vector
  pop_size <- numeric(gen + 1)
  pop_size[1] <- N0
  
  # Run simulation over generations
  for (i in 1:gen) {
    r_curr <- ifelse(stochastic_r, rnorm(1, r, 0.1), r)
    K_curr <- ifelse(stochastic_K, rnorm(1, K, 50), K)
    pop_size[i + 1] <- as.integer(pop_size[i] * exp(r_curr * (1 - pop_size[i] / K_curr)))
  }

  # Plot results if requested
  if (plotting) {
    plot(0:gen, pop_size, type = 'l', col = 'blue', ylim = c(0, max(pop_size)),
         xlab = 'Generation', ylab = 'Population Size', main = 'Population Growth Simulation')
    legend('bottomleft', legend = c('Population'), inset = c(0.05, 0.05),
           col = c('blue'), lty = 1)
  }
  return(pop_size)
}

#Adjust the format as numeric
clean_data <- function(data) {
  data[] <- lapply(data, function(x) {
    # Step 1: Remove double quotes (keeping "NULL" as it is)
    x <- ifelse(x == "NULL", x, gsub('^"|"$', '', x))
    
    # Step 2: Replace "NULL" with NA and convert other values to numeric
    ifelse(x == "NULL", NA, as.numeric(x))
  })
  return(data)
}

## add mandatory columns to use the LPI 
bino_id <- function(data, years, num_species) {
  # Add a 'Binomial' column with 'Species1', 'Species2', ..., 'SpeciesN'
  data[, (length(years) + 1)] <- paste0('Species', 1:num_species)
  
  # Set the column names, including 'Binomial' as the last column
  colnames(data) <- c(paste0('X', years), 'Binomial')
  
  # Add an 'ID' column (unique row identifier)
  data$ID <- 1:nrow(data)
  
  return(data)
}

permutationLPI <- function(mat, nperm = 100, shuffle_zeros = TRUE, shuffle_NA = FALSE, years = years, S = S) {
  permuted_matrices <- vector("list", nperm)
pb <- txtProgressBar(min = 0, max = nperm, style = 3)
  for (i in 1:nperm) {
    permuted_mat <- t(apply(mat, 1, function(row) {
      na_positions <- is.na(row)
      zero_positions <- !is.na(row) & row == 0

      # Determine which positions are fixed depending on arguments
      fixed_positions <- logical(length(row))

      if (!shuffle_zeros) {
        fixed_positions <- fixed_positions | zero_positions
      }
      if (!shuffle_NA) {
        fixed_positions <- fixed_positions | na_positions
      }

      # Values to shuffle: all positions not fixed
      values_to_shuffle <- row[!fixed_positions]

      # Shuffle values
      shuffled_values <- sample(values_to_shuffle)

      # Create new row and place fixed values and shuffled values accordingly
      new_row <- row
      new_row[!fixed_positions] <- shuffled_values

      return(new_row)
    }))
    permuted_mat <- bino_id(as.data.frame(permuted_mat), years, S)
     permuted_matrices[[i]] <- permuted_mat
     setTxtProgressBar(pb, i)
  }
  return(permuted_matrices)
   close(pb)
}


##plot
plot_lpi <- function(data, colr, label_name = "LPIX") {
  if (!"years" %in% colnames(data)) {
    stop("The data must contain a 'years' column")
  }
  
  # Add a new column with the label_name for mapping fill and color
  data$label <- label_name
  
  ggplot(data, aes(x = years)) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2) +
    geom_hline(yintercept = 1, linetype = "solid", size = 0.5, color = "#000000") +
    geom_line(aes(y = LPI_final, color = label), size = 1) +
    scale_fill_manual(name = NULL, values = setNames(colr[2], label_name)) +
    scale_color_manual(name = NULL, values = setNames(colr[1], label_name)) +
    labs(x = "Year", y = "Index", title = "") +
    scale_x_continuous(breaks = seq(1950, 2020, by = 10), expand = c(0.03, 0.05)) +
    scale_y_continuous(limits = c(0, 2), expand = c(0.05, 0.002)) +
    guides(color = guide_legend(override.aes = list(fill = colr[2], color = colr[1], size = 15, alpha = 0.2))) +
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
}

#function toparalelize theprocesess


process_permutation <- function(w = 1, base_path = getwd(), 
                                title_prefix = "LPI Results") {
  
  # Define input and output directories
  input_path <- file.path(base_path, "processing", sprintf("It_%d", w))
  output_dir <- file.path(base_path, "results")
  
  # Ensure the results directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load the matrix
  mat <- readRDS(file.path(input_path, sprintf("matrix_%03d.rds", w)))
  
  result <- LPIMain(
    create_infile(
      mat,
      index_vector = TRUE,
      name = file.path(input_path, as.character(w)),
      start_col_name = "X1950",
      end_col_name = "X2019",
      CUT_OFF_YEAR = 1950
    ),
    title = paste(title_prefix, w),
    REF_YEAR = 1950,
    PLOT_MAX = 2019,
    BOOT_STRAP_SIZE = 10,
    VERBOSE = FALSE,  
    SHOW_PROGRESS = TRUE
  )
  
  message("Completed permutation ", w)
  saveRDS(result, file = file.path(output_dir, sprintf("permutation_result_%03d.rds", w)))
  
  return(result)
}

routeX <- '01Scripts/'

save.image(file =  paste0( "01Scripts/functionsLPIT.RData"))
