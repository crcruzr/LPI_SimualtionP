library(tidyverse)
library(rlpi)
library(data.table)

#functions 
source('01Scripts/Functions.r')

## Run 300 ermutations
#It was running using ComputeofCanada (https://www.alliancecan.ca/en)
# using array of 1-10 with a time of 04h, four cpu per task and 30Gb of ram memory
args = commandArgs(trailingOnly=TRUE)
iteration_number <- as.numeric(args[1])

print(paste("Simulation starts at", Sys.time()))

# Process to do the iterations
process_permutation(
  iteration_number, 
  base_path = "03processedData/constrain/1_na_zero_permutations/simulatedData",
  title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA and zero - permutation"
)

print(paste("Iteration", iteration_number, "finished at", Sys.time()))
print('Permutations with the zeros and missing dataset finnished')

##############
#### END #####
##############  

# print(paste("Simulation starts at", Sys.time()))
# # Process to do the iterations
# process_permutation(  
#   iteration_number, 
#   base_path = "03processedData/constrain/1_na_zero_permutations/empiricalData",
#   title_prefix = "LPI Results - Real Dataset - Only NA and zero - permutation"
# )  

# print(paste("Iteraction", iteration_number, "finished at", Sys.time()))
# print('Permutations with the Empirical dataset')
