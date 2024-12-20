library(tidyverse)
library(mgcv)
library(dlnm)
library(splines)
library(patchwork)

source("00_aux_functions.R")

# Setup and data ----

cdc_groups <- get_cdc_groups()

temp_df <- read_csv("data/temp_df.csv", show_col_types = F)
do <- read_csv("data/do_data.csv", show_col_types = F)

# Calculating accumulated HAAT values

temp_df <- temp_df %>% 
  mutate(auc32_cum3 = auc32 + lag(auc32, 1) + lag(auc32, 2),
         auc36_cum3 = auc36 + lag(auc36, 1) + lag(auc36, 2),
         auc32_cum5 = auc32 + lag(auc32, 1) + lag(auc32, 2) + lag(auc32, 3) + lag(auc32, 4),
         auc36_cum5 = auc36 + lag(auc36, 1) + lag(auc36, 2) + lag(auc36, 3) + lag(auc36, 4),
         auc32_cum7 = auc32 + lag(auc32, 1) + lag(auc32, 2) + lag(auc32, 3) + lag(auc32, 4)  + lag(auc32, 5) + lag(auc32, 6),
         auc36_cum7 = auc36 + lag(auc36, 1) + lag(auc36, 2) + lag(auc36, 3) + lag(auc36, 4)  + lag(auc36, 5) + lag(auc36, 6)
  ) %>% 
  mutate(across(contains("cum"), ~replace_na(.x, 0)))

## Initializing data frames ----

RR_lag_df <- tibble(
  causa = character(),
  grupo = character(),
  metric = character(),
  at = integer(),
  lag = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

RR_df <- tibble(
  causa = character(),
  grupo = character(),
  metric = character(),
  at = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

metrics_df <- tibble(
  causa = character(),
  grupo = character(),
  metric = character(),
  aic = double(),
  ubre = double(),
  deviance = double(),
  null_deviance = double(),
  edf_values = double(),
  df_residual = double(),
  deviance_explained = double(),
  log_likelihood = double(),
  converged = logical()
)

## Model parameters ----

vars_params <- tribble(
  ~var, ~fun, ~df,
  "t_med", "bs", 3,
  "hi_med", "bs", 3,
  "num_hr_32", "ns", 1,
  "num_hr_33", "ns", 1,
  "num_hr_34", "ns", 1,
  "num_hr_35", "ns", 1,
  "num_hr_36", "ns", 1,
  "num_hr_37", "ns", 1,
  "num_hr_38", "ns", 1,
  "num_hr_39", "ns", 1,
  "num_hr_40", "ns", 1,
  "num_hr_41", "ns", 1,
  "num_hr_42", "ns", 1,
  "num_hr_43", "ns", 1,
  "num_hr_44", "ns", 1,
  "auc32", "ps", 15,
  "auc32_cum3", "ps", 15,
  "auc32_cum5", "ps", 15,
  "auc32_cum7", "ps", 15,
  "auc36", "ps", 15,
  "auc36_cum3", "ps", 15,
  "auc36_cum5", "ps", 15,
  "auc36_cum7", "ps", 15
)

model_combinations <- cdc_groups %>% 
  cross_join(tibble(
    age_group = c("Jovem", "Idoso")
  )) %>% 
  cross_join(
    vars_params
  ) %>% 
  mutate(likelihood = case_when(
    grupo == "Causas naturais" ~ "nb",
    TRUE ~ "poisson"
  ))

# Fitting -----

for (i in seq_len(nrow(model_combinations))) {
  
  #for (i in t_hi_indexes) { 
  
  print(paste0(Sys.time(), " - Starting ", 
               model_combinations$grupo[i], " (",
               model_combinations$age_group[i], "). Variable: ",
               model_combinations$var[i],
               "..."))
  
  ## Data ----
  
  do_i <- do %>% 
    filter(group_id == model_combinations$group_id[i],
           age_group == model_combinations$age_group[i]
    ) %>% 
    left_join(temp_df, by=c("data_obito"="dia")) %>% 
    mutate(var = get(model_combinations$var[i])) %>% 
    select(group_id:covid_id, t_med, hi_med, var)
  
  # Crossbasis ----
  
  cb_i <- crossbasis(
    do_i$var,
    lag = 10,
    argvar = list(
      fun = model_combinations$fun[i],
      df = model_combinations$df[i]
    ),
    arglag = list(fun="ns", df = 3)
  )
  
  if (model_combinations$fun[i] != "ps") {
    gam_i <- gam(
      n ~ cb_i + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
      data = do_i,
      family = model_combinations$likelihood[i],
      method = "REML"
    ) 
  } else {
    gam_i <- gam(
      n ~ cb_i + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
      data = do_i,
      family = model_combinations$likelihood[i],
      paraPen = list(cb_i = cbPen(cb_i)),
      method = "REML"
    )
  }
  
  # Result gathering ----
  
  range_i <- seq(
    min(do_i$var),
    max(do_i$var),
    length.out = 100
  )
  
  cen_i <- case_when(
    grepl("num_hr|auc", model_combinations$var[i]) ~ 0,
    TRUE ~ mean(do_i$var)
  ) 
  
  print(paste0("Using ", cen_i, " as center for variable."))
  
  pred_i <- gam_i %>% crosspred(basis = cb_i, at = range_i, cen = cen_i)
  
  ### Lagged RR df ----
  
  RR_lag_df <- RR_lag_df %>% 
    add_row(
      pred_i %>% 
        extract_RR_lag(range = range_i) %>% 
        mutate(causa = model_combinations$grupo[i], 
               grupo = model_combinations$age_group[i], 
               metric = model_combinations$var[i])
    )
  
  ### Overall RR df ----
  
  RR_df <- RR_df %>% 
    add_row(
      pred_i %>% 
        extract_RR(range = range_i) %>% 
        mutate(causa = model_combinations$grupo[i], 
               grupo = model_combinations$age_group[i], 
               metric = model_combinations$var[i])
    )
  
  ### Metrics df ----
  
  metrics_df <- metrics_df %>% 
    add_row(gam_i %>% 
              extract_fit_metrics(model_tag=model_combinations$var[i], 
                                  cdc_cause=model_combinations$grupo[i], 
                                  age_group=model_combinations$age_group[i])) 
  
  ## AUC Combined models ----
  
  if (grepl("auc", model_combinations$var[i])) {
    # run models for added effects also
    
    cb_t <- crossbasis(
      do_i$t_med,
      lag = 10,
      argvar = list(
        fun = "ns",
        df = 4
      ),
      arglag = list(fun="ns", df = 3)
    )
    
    gam_i_t <- gam(
      n ~ cb_t + cb_i + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
      data = do_i,
      family = model_combinations$likelihood[i],
      paraPen = list(cb_i = cbPen(cb_i)),
      method = "REML"
    )
    
    cb_hi <- crossbasis(
      do_i$hi_med,
      lag = 10,
      argvar = list(
        fun = "bs",
        df = 3
      ),
      arglag = list(fun="ns", df = 3)
    )
    
    gam_i_hi <- gam(
      n ~ cb_hi + cb_i + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
      data = do_i,
      family = model_combinations$likelihood[i],
      paraPen = list(cb_i = cbPen(cb_i)),
      method = "REML"
    )
    
    range_t <- seq(
      min(do_i$t_med),
      max(do_i$t_med),
      length.out = 100
    )
    
    cen_t <- mean(do_i$t_med)
    
    range_hi <- seq(
      min(do_i$hi_med),
      max(do_i$hi_med),
      length.out = 100
    )
    
    cen_hi <- mean(do_i$hi_med)
    
    pred_i_t <- gam_i_t %>% crosspred(basis = cb_i, at = range_i, cen = cen_i)
    pred_t_i <- gam_i_t %>% crosspred(basis = cb_t, at = range_t, cen = cen_t)
    pred_i_hi <- gam_i_hi %>% crosspred(basis = cb_i, at = range_i, cen = cen_i)
    pred_hi_i <- gam_i_hi %>% crosspred(basis = cb_hi, at = range_hi, cen = cen_hi)
    
    RR_lag_df <- RR_lag_df %>% 
      add_row(
        pred_i_t %>% 
          extract_RR_lag(range = range_i) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0(model_combinations$var[i], " + temperature"))
      ) %>% 
      add_row(
        pred_t_i %>% 
          extract_RR_lag(range = range_t) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0("temperature + ", model_combinations$var[i]))
      ) %>% 
      add_row(
        pred_i_hi %>% 
          extract_RR_lag(range = range_i) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0(model_combinations$var[i], " + heat_index"))
      ) %>% 
      add_row(
        pred_hi_i %>% 
          extract_RR_lag(range = range_hi) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0("heat_index + ", model_combinations$var[i]))
      )
    
    RR_df <- RR_df %>% 
      add_row(
        pred_i_t %>% 
          extract_RR(range = range_i) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0(model_combinations$var[i], " + temperature"))
      ) %>% 
      add_row(
        pred_t_i %>% 
          extract_RR(range = range_t) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0("temperature + ", model_combinations$var[i]))
      ) %>% 
      add_row(
        pred_i_hi %>% 
          extract_RR(range = range_i) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0(model_combinations$var[i], " + heat_index"))
      ) %>% 
      add_row(
        pred_hi_i %>% 
          extract_RR(range = range_hi) %>% 
          mutate(causa = model_combinations$grupo[i], 
                 grupo = model_combinations$age_group[i], 
                 metric = paste0("heat_index + ", model_combinations$var[i]))
      )
    
    metrics_df <- metrics_df %>% 
      add_row(gam_i_t %>% 
                extract_fit_metrics(model_tag=paste0(model_combinations$var[i], " + temperature"), 
                                    cdc_cause=model_combinations$grupo[i], 
                                    age_group=model_combinations$age_group[i])) %>% 
      add_row(gam_i_hi %>% 
                extract_fit_metrics(model_tag=paste0(model_combinations$var[i], " + heat_index"), 
                                    cdc_cause=model_combinations$grupo[i], 
                                    age_group=model_combinations$age_group[i]))
    
  }
}

save(RR_df, RR_lag_df, metrics_df, 
     file = paste0("results/dlnm_model_results.Rdata"))
