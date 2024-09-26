# Master Thesis: Revisiting the Productivity-Elasticity of Labor Demand - Reproduction of Key Results

## Overview
This repository contains the R code required to reproduce key results from my Master's thesis, which investigates the relationship between productivity growth and labor demand at the industry level. The empirical analysis is based on the EU-KLEMS dataset, an industry-level dataset covering 14 OECD countries from 1995 to 2020. The thesis replicates and extends the methodology of Autor et al. (2018).

## Requirements

### Data:
To run the scripts, download the following datasets from the [EU-KLEMS Database](https://euklems-intanprod-llee.luiss.it/download/):
- **Growth Accounts** dataset
- **National Accounts** dataset

### Software:
- R version 4.0 or higher.  
- Required R packages will be loaded automatically by the scripts.

## Instructions

### 1. Folder Setup
After downloading the datasets from EU-KLEMS, set up your folder structure as follows:
- **Folder 1**: Store the R scripts.
- **Folder 2**: Store the datasets (Growth Accounts and National Accounts) you downloaded from EU-KLEMS.

### 2. Modify Folder Paths
Before running the code, update the folder paths in the following scripts:
- `MA_00_Master.R`
- `MA_01_SETTINGS.R`

Ensure that the paths in these scripts point to the correct directories where you’ve stored your datasets and scripts.

### 3. Run the Code
Once the folders are set up and the paths are adjusted, you can reproduce the results by running the `MA_00_Master.R` script. This will execute all steps necessary to replicate the analysis and generate the key results presented in the thesis.

## File Structure
- [`MA_00_Master.R`](MA_00_Master.R) – Main script to run the entire analysis
- `MA_01_SETTINGS.R` – Contains path and settings configurations
- `MA_02_LOAD_DATA.R` – Imports datasets
- `MA_03_CLEAN_DATA.R` – Cleans and merges both datasets
- `MA_04_VARIABLES.R` – Generates the key variables required for the summary statistics and regressions
- `MA_05_SUMMARYSTATS.R` – Produces summary statistics and outputs LaTeX code
- `MA_06_REGRESSIONS.R` – Runs key regressions and outputs LaTeX code

## Reproducing Results
The scripts are designed to:
- Run the key regression models from Autor et al. (2018), extended for this study.
- Generate outputs that illustrate the relationship between productivity growth and labor demand.

## Thesis
You can read the full thesis [here](Master_Thesis.pdf).

## License
This project is licensed under the MIT License.

## Contact
For any questions or issues, feel free to contact me at [tillmanschenk@googlemail.com](mailto:tillmanschenk@googlemail.com).
