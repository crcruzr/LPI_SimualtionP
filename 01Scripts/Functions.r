##Functions


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


process_permutation <- function(w = 1, data_list, base_path = getwd(), 
                                title_prefix = "LPI Results") {
  mat <- readRDS(sprintf("%s/matrix_%03d.rds", base_path, w))
  
  result <- LPIMain(
    create_infile(
      data_list[[w]],
      index_vector = TRUE,
      name = paste0(base_path, "/", w),
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
  saveRDS(result, file = sprintf("%s/results/permutation_result_%03d.rds", base_path, w))
  
  return(result)
}


setwd('/Users/ccruzr/Library/Mobile Documents/com~apple~CloudDocs/Cristian/Documents/Estudios/Postgrado/PhD/Projects/LPI_SimualtionP')
routeX <- '01Scripts/'

save.image(file =  paste0( "01Scripts/functionsLPIT.RData"))
