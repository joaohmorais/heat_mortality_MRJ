library(tidyverse)
library(mgcv)
library(dlnm)
library(splines)

source("00_aux_functions.R")

# Setup and data ----

cdc_groups <- get_cdc_groups()

temp_df <- read_csv("data/temp_df.csv", show_col_types = F)
do <- read_csv("data/do_data.csv", show_col_types = F)

t_med_q90 <- quantile(temp_df$t_med, probs = 0.9)
t_med_q925 <- quantile(temp_df$t_med, probs = 0.925)
t_med_q95 <- quantile(temp_df$t_med, probs = 0.95)
t_med_q975 <- quantile(temp_df$t_med, probs = 0.975)

temp_df_metrics <- temp_df %>% 
  mutate(
    above90 = t_med >= t_med_q90,
    above925 = t_med >= t_med_q925,
    above95 = t_med >= t_med_q95,
    above975 = t_med >= t_med_q975
  )

temp_df_metrics <- temp_df_metrics %>% 
  mutate(
    q90_2d = (above90 & lag(above90, 1)) | (above90 & lead(above90, 1)),
    q90_3d = 
      (lag(above90,2) & lag(above90,1) & above90) | 
      (lag(above90,1) & above90 & lead(above90, 1)) |
      (above90 & lead(above90, 1) & lead(above90, 2)),
    q90_4d = 
      (lag(above90, 3) & lag(above90, 2) & lag(above90, 1) & above90) | 
      (lag(above90, 2) & lag(above90, 1) & above90 & lead(above90,1)) |
      (lag(above90, 1) & above90 & lead(above90,1) & lead(above90,2)) |
      (above90 & lead(above90,1) & lead(above90,2) & lead(above90, 3)),
    q925_2d = (above925 & lag(above925, 1)) | (above925 & lead(above925, 1)),
    q925_3d = 
      (lag(above925,2) & lag(above925,1) & above925) | 
      (lag(above925,1) & above925 & lead(above925, 1)) |
      (above925 & lead(above925, 1) & lead(above925, 2)),
    q925_4d = 
      (lag(above925, 3) & lag(above925, 2) & lag(above925, 1) & above925) | 
      (lag(above925, 2) & lag(above925, 1) & above925 & lead(above925,1)) |
      (lag(above925, 1) & above925 & lead(above925,1) & lead(above925,2)) |
      (above925 & lead(above925,1) & lead(above925,2) & lead(above925, 3)),
    q95_2d = (above95 & lag(above95, 1)) | (above95 & lead(above95, 1)),
    q95_3d = 
      (lag(above95,2) & lag(above95,1) & above95) | 
      (lag(above95,1) & above95 & lead(above95, 1)) |
      (above95 & lead(above95, 1) & lead(above95, 2)),
    q95_4d = 
      (lag(above95, 3) & lag(above95, 2) & lag(above95, 1) & above95) | 
      (lag(above95, 2) & lag(above95, 1) & above95 & lead(above95,1)) |
      (lag(above95, 1) & above95 & lead(above95,1) & lead(above95,2)) |
      (above95 & lead(above95,1) & lead(above95,2) & lead(above95, 3)),
    q975_2d = (above975 & lag(above975, 1)) | (above975 & lead(above975, 1)),
    q975_3d = 
      (lag(above975,2) & lag(above975,1) & above975) | 
      (lag(above975,1) & above975 & lead(above975, 1)) |
      (above975 & lead(above975, 1) & lead(above975, 2)),
    q975_4d = 
      (lag(above975, 3) & lag(above975, 2) & lag(above975, 1) & above975) | 
      (lag(above975, 2) & lag(above975, 1) & above975 & lead(above975,1)) |
      (lag(above975, 1) & above975 & lead(above975,1) & lead(above975,2)) |
      (above975 & lead(above975,1) & lead(above975,2) & lead(above975, 3))
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

for (grupo in cdc_groups$group_id) {
  
  grupo_label <- cdc_groups$grupo[cdc_groups$group_id == grupo]
  
  print(paste0(Sys.time(), " - Starting ", grupo_label, "..."))
  
  if (grupo_label == "Causas naturais") {
    likelihood <- "nb"
  } else {
    likelihood <- "poisson"
  }
  
  do_idoso_hw <- do %>% 
    filter(group_id == grupo, age_group == "Idoso") %>% 
    left_join(temp_df_metrics %>% select(dia, t_med, starts_with("q")) %>% 
                mutate(across(starts_with("q"), as.integer)),
              by=c("data_obito"="dia"))
  
  for (hw_i in do_idoso_hw %>% select(starts_with("q")) %>% names()) {
    
    print(paste0("Heat wave: ", hw_i))
    
    do_idoso_hw <- do_idoso_hw %>% 
      mutate(hw = get(hw_i))
    
    cb_i_t <- crossbasis(
      do_idoso_hw$t_med,
      lag=10,
      argvar = list(fun="bs", df=4),
      arlag = list(fun="bs", df=4)
    )
    
    # crossbasis
    cb_i_hw <- crossbasis(
      do_idoso_hw$hw,
      lag=10,
      argvar = list(fun="ns", df=1),
      arglag=list(fun="bs", df=4)
    )
    
    # First model: heat wave only
    
    gam_i_idoso_hw <- gam(
      # first term adjusted to 12
      n ~ cb_i_hw + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
      data = do_idoso_hw,
      family = likelihood,
      #paraPen = list(cb_i_hw = cb_i_hw_pen),
      method = "REML"
    )
    
    gam_hw_idoso_pred <- crosspred(cb_i_hw,gam_i_idoso_hw,at=1, cen=0)
    
    RR_lag_df <- RR_lag_df %>% 
      add_row(
        gam_hw_idoso_pred %>% 
          extract_RR_lag(range = 1) %>% 
          mutate(causa = grupo_label, grupo = "Idoso", metric = hw_i)
      )
    
    RR_df <- RR_df %>% 
      add_row(
        gam_hw_idoso_pred %>% 
          extract_RR(range = 1) %>% 
          mutate(causa = grupo_label, grupo = "Idoso", metric = hw_i)
      )
    
    metrics_df <- metrics_df %>% 
      add_row(gam_i_idoso_hw %>% extract_fit_metrics(model_tag=hw_i, cdc_cause=grupo_label, age_group="Idoso"))
    
    # Added effects: heat wave + temperature
    
    gam_i_idoso_hw_t <- gam(
      # first term adjusted to 12
      n ~ cb_i_t + cb_i_hw + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
      data = do_idoso_hw,
      family = likelihood,
      #paraPen = list(cb_i_hw = cb_i_hw_pen),
      method = "REML"
    )
    
    gam_hw_idoso_t_pred <- crosspred(cb_i_hw,gam_i_idoso_hw_t,at=1, cen=0)
    
    min_t <- round(min(do_idoso_hw$t_med))
    max_t <- round(max(do_idoso_hw$t_med))
    
    gam_t_idoso_hw_pred <- crosspred(cb_i_t,gam_i_idoso_hw_t,at=min_t:max_t, cen = round(mean(do_idoso_hw$t_med)))
    
    RR_lag_df <- RR_lag_df %>% 
      add_row(
        gam_hw_idoso_t_pred %>% 
          extract_RR_lag(range = 1) %>% 
          mutate(causa = grupo_label, grupo = "Idoso", metric = paste0(hw_i, " + temperature"))
      )  %>% 
      add_row(
        gam_t_idoso_hw_pred %>% 
          extract_RR_lag(range = min_t:max_t) %>% 
          mutate(causa = grupo_label, grupo = "Idoso", metric = paste0("temperature + ", hw_i))
      )
    
    RR_df <- RR_df %>% 
      add_row(
        gam_hw_idoso_t_pred %>% 
          extract_RR(range = 1) %>% 
          mutate(causa = grupo_label, grupo = "Idoso", metric = paste0(hw_i, " + temperature"))
      ) %>% 
      add_row(
        gam_t_idoso_hw_pred %>% 
          extract_RR(range = min_t:max_t) %>% 
          mutate(causa = grupo_label, grupo = "Idoso", metric = paste0("temperature + ", hw_i))
      )
    
    metrics_df <- metrics_df %>% 
      add_row(gam_i_idoso_hw_t %>% extract_fit_metrics(model_tag=paste0(hw_i, " + temperature"), cdc_cause=grupo_label, age_group="Idoso"))
    
  }
  
}

hw_RR_df <- RR_df
hw_RR_lag_df <- RR_lag_df
hw_metrics_df <- metrics_df

save(hw_RR_df, hw_RR_lag_df, hw_metrics_df, file = "results/hw_results.Rdata")
