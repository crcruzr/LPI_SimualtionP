#### This folder contains

routine.r = underrevision

Functions.r = Functions that are used during the analisys

SimConstrains_datapreparation.r = Parameters required to running the iteration in the simualted data

plots_permutations.r = Routine to generate the plots.

SimConstrains.r = Full workflow to calculate the mask used in the simulated data to obtain:
                    -> Simulated data with NA cells presented in the real structure
                    -> Simulated data with 0s cells presented in the real structure
                    -> Iteractions to change the position of the NA in the simualted data 
                    -> Iteractions to change the position of the 0's in the simualted data

pop_dyn_sim_comparision.r = Routine to generate the simulated populations 
                            Calcualte the LPI with simulated populations
                            Calcualte the LPI with Real data        
                            Calculate the LPI using the simulated population data, using the Real data Template
                            Calculate the LPI using the simulated population data, using the Real data Template, moving the window

SimConstrains0sIndepend.r = Workflow to increase the number of iteractions to change the position of the 0's, using Compute of Canada

SimConstrainsNAIndepend.r = Workflow to increase the number of iteractions to change the position of the NA, using Compute of Canada

SimConstrains0s and SimConstrainsNA (depreciated) = Previous LPI simulations to calcualte the NA and 0's without Compute of Canada 