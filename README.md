# Heat exposure and mortality in Rio de Janeiro: sustaining the first Brazilian heat protocol

João Henrique de Araujo Morais, Valeria Saraceni, Caroline Dias Ferreira, Débora Medeiros de Oliveira e Cruz, Gislani Mateus Aguilar, Oswaldo Gonçalves Cruz.

- `00_aux_functions.R` includes common functions used throughout the analysis in the study.
- `01a_descriptive_analysis.R` generates basic description ("table one") of climate and temperature data (Tables 2 and 3 in the article).
- `01b_exploratory_patterns.R` fits the null models (only smooth and fixed terms, no crossbasis covariates) for mortality data and daily heat index. It plots both trends configuring the article's Figure 1. The smooth terms are also plotted and shown in Figure S1. Figure 4 of the article (HAAT metric demonstration) is also built here.
- `02_model_fitting.R` replicates the fitting process of all models included in the study. The model parameters and specified in the data frame `model_combinations`. The results are ran iteratively and stored in `results/dlnm_model_results.Rdata`, used in the subsequent scripts.
- `02b_model_fitting_HW.R` replicates the modeling procedure for the Heat Wave (HW) indicator variables, for comparison.
- `03_results_interpretation.R` contains the visualizations presented in the article.
- `04_model_metrics.R` loads all models fitted (including HW) for comparison based on AIC.