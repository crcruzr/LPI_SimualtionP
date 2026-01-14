# Project Overview

This repository contains the scripts, data, and outputs required to reproduce the full analytical pipeline used in the paper **Assessing the sensibility and robustness of the Living Planet Index through simulated population dynamics: Strengths, stability, and challenges**, including simulation, trend modeling, constraint application, and computation of the **Living Planet Index (LPI)**.

The analysis is based on the **Living Planet Database (LPD)** and on simulated population time series designed to evaluate how missing values (`NA`) and zero counts affect the LPI under different trend structures (concave, linear, and convex).

---

# Repository Structure

The repository is organized into the following directories:

## `00RawData`
IT should contains the **Living Planet Database (LPD)** used as the empirical reference for all analyses.
---

## `01Scripts`
Contains all scripts required to generate simulations, apply constraints, and compute the Living Planet Index.

### `01_SimulatedPop_LPI`
Generates simulated population time series and computes the LPI using both the simulated data and the LPD.

### `02_trends_LPI`
Applies concave, linear, and convex trends to the simulated data and computes the corresponding LPI values.

### `03_SimConstrains_datapreparation`
Prepares the simulated data by introducing `NA` values and zeros following the structure observed in the LPD, and formats the data for permutation and iteration analyses.

### `04_SimConstrains`
Computes the LPI using simulated data with imposed `NA` values and zeros, varying their positions across iterations.

---

## `02Documents`
Directory intended for working documents and notes. Files in this folder are not shared as part of the public repository.

---

## `03processedData`
Stores all intermediate data products generated during the different processing steps and analytical approaches.

---

## `04FinalData`
Contains the final datasets used for analysis and reporting.

---

## `05Plots`
Contains all figures and plots generated for the manuscript and supplementary materials.

---

# Reproducibility

All analyses can be reproduced by running the scripts in the `01Scripts` directory in numerical order. 

Intermediate outputs are stored in `03processedData`, and final results are written to `04FinalData`, with figures saved in `05Plots`.

