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

# Initializing

sens_RR_df <- tibble(
  causa = character(),
  grupo = character(),
  metric = character(),
  fun = character(),
  df_var = integer(),
  at = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

sens_RR_lag_df <- tibble(
  causa = character(),
  grupo = character(),
  metric = character(),
  fun = character(),
  df_var = integer(),
  at = integer(),
  lag = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

sens_metrics_df <- tibble(
  causa = character(),
  grupo = character(),
  metric = character(),
  fun = character(),
  df_var = integer(),
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

do <- do %>% 
  filter(age_group == "Idoso", group_id == 16) %>% 
  left_join(
    temp_df %>% select(dia, t_med:hi_max, num_hr_32, num_hr_36, num_hr_40, num_hr_42, num_hr_44, starts_with("auc")),
    by = c("data_obito" = "dia")
  )

# for continuous t_med and hi_med:

params_1 <- tribble(
  ~spline, ~df,
  "ns", 2,
  "ns", 4,
  "ns", 6,
  "bs", 3, 
  "bs", 4, 
  "bs", 6, 
  "bs", 8
)

# for hours of exposure and HAAT:

params_2 <- tribble(
  ~spline, ~df,
  "ns", 1, 
  "bs", 3, 
  "cr", 3, 
  "cr", 4, 
  "cr", 5,
  "cr", 6, 
  "ps", 5, 
  "ps", 10, 
  "ps", 15
)

parameters <- tibble(
  metric = c("hi_med", "t_med")
) %>% 
  cross_join(params_1) %>% 
  add_row(
    tibble(
      metric = do %>% 
        select(starts_with("num_hr") | starts_with("auc")) %>% 
        names()
    ) %>% 
      cross_join(params_2)
  )

for (metric_i in unique(parameters$metric)) {
  
  params_i <- parameters %>% 
    filter(metric==metric_i)
  
  for (j in seq_len(nrow(params_i))) {
    
    fun_i <- params_i$spline[j]
    df_i <- params_i$df[j]
    
    print(paste0("Trying combination: ", metric_i, " + fun:", fun_i, ", df: ", df_i))
    
    do <- do %>% 
      mutate(var = get(metric_i))
    
    cb_i <- crossbasis(
      do$var,
      lag = 10,
      argvar = list(fun=fun_i, df = df_i),
      arglag = list(fun="ns", df = 4)
    )
    
    if (grepl("auc", metric_i)) {
      
      cb_t <- crossbasis(
        do$t_med,
        lag=10,
        argvar = list(fun="bs", df=4),
        arglag = list(fun="ns", df=4)
      )
      
      formula <- n ~ cb_t + cb_i + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc")
    } else {
      
      formula <- n ~ cb_i + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc")
    }
    
    if (fun_i != "ps") {
      gam_i <- gam(
        formula,
        data = do,
        family = "poisson",
        method = "REML"
      ) 
    } else {
      gam_i <- gam(
        formula,
        data = do,
        family = "poisson",
        paraPen = list(cb_i = cbPen(cb_i)),
        method = "REML"
      )
    }
    
    range_i <- seq(
      min(do$var),
      max(do$var),
      length.out = 100
    )
    
    cen_i <- mean(do$var)
    
    pred_i <- gam_i %>% crosspred(basis = cb_i, at = range_i, cen = cen_i)
    
    sens_RR_df <- sens_RR_df %>% 
      add_row(
        pred_i %>% 
          extract_RR(range = range_i) %>% 
          mutate(causa = "Causas naturais", grupo = "Idoso", 
                 metric = metric_i,
                 df_var = attr(cb_i, "df")[1], 
                 fun=fun_i)
      )
    
    sens_RR_lag_df <- sens_RR_lag_df %>% 
      add_row(
        pred_i %>% 
          extract_RR_lag(range = range_i) %>% 
          mutate(causa = "Causas naturais", grupo = "Idoso", 
                 metric = metric_i,
                 df_var = attr(cb_i, "df")[1], 
                 fun=fun_i)
      )
    
    sens_metrics_df <- sens_metrics_df %>% 
      add_row(
        gam_i %>% extract_fit_metrics(model_tag = metric_i,
                                      cdc_cause = "Causas naturais",
                                      age_group = "Idoso"
        ) %>% 
          mutate(df_var = attr(cb_i, "df")[1], 
                 fun=fun_i)
      )
    
  }
  
}

# Comparison functions ----

sens_RR_df <- sens_RR_df %>% 
  mutate(params = paste0("fun: ", fun, ", df = ", df_var))
sens_RR_lag_df <- sens_RR_lag_df %>% 
  mutate(params = paste0("fun: ", fun, ", df = ", df_var))
sens_metrics_df <- sens_metrics_df %>% 
  mutate(params = paste0("fun: ", fun, ", df = ", df_var))

view_fit_metrics <- function(metric_df, metric_i) {
  metric_df %>% 
    filter(metric == metric_i) %>% 
    ggplot(aes(x=1-deviance_explained, y=aic)) + 
    geom_point() + 
    ggrepel::geom_text_repel(aes(label=params))
}

compare_RR_curve <- function(RR_df, metric_i) {
  RR_df %>% 
    filter(metric == metric_i) %>% 
    ggplot(aes(x=at, y = RR)) + 
    geom_ribbon(aes(ymin=RR_low, ymax=RR_upp, fill = params),  alpha=0.2) +
    geom_line(aes(color = params,group = params)) + 
    geom_hline(yintercept = 1) +
    guides(fill="none", color="none") +
    facet_wrap(~params, scales = "free")
}

# IC

sens_metrics_df %>% view_fit_metrics("hi_med")

sens_RR_df %>% compare_RR_curve("hi_med")

# Temperature

sens_metrics_df %>% view_fit_metrics("t_med")

sens_RR_df %>% compare_RR_curve("t_med")

# Number of hours

sens_metrics_df %>% view_fit_metrics("num_hr_32")
sens_RR_df %>% compare_RR_curve("num_hr_32")

# ps df 5, 10 ou 15

sens_metrics_df %>% view_fit_metrics("num_hr_36")
sens_RR_df %>% compare_RR_curve("num_hr_36")

sens_metrics_df %>% view_fit_metrics("num_hr_40")
sens_RR_df %>% compare_RR_curve("num_hr_40")

sens_metrics_df %>% view_fit_metrics("num_hr_42")
sens_RR_df %>% compare_RR_curve("num_hr_42")

sens_metrics_df %>% view_fit_metrics("num_hr_44")
sens_RR_df %>% compare_RR_curve("num_hr_44")

# AUC

sens_metrics_df %>% view_fit_metrics("auc32")
sens_RR_df %>% compare_RR_curve("auc32")

# ps df 5, ou bs df 3

sens_metrics_df %>% view_fit_metrics("auc32_cum3")
sens_RR_df %>% compare_RR_curve("auc32_cum3")

# ps qualquer grau

sens_metrics_df %>% view_fit_metrics("auc32_cum5")
sens_RR_df %>% compare_RR_curve("auc32_cum5")

# ps df 10 ou 15

sens_metrics_df %>% view_fit_metrics("auc32_cum7")
sens_RR_df %>% compare_RR_curve("auc32_cum7")

# ps df 10 ou 15

sens_metrics_df %>% view_fit_metrics("auc36")
sens_RR_df %>% compare_RR_curve("auc36")

# ps df 5

sens_metrics_df %>% view_fit_metrics("auc36_cum3")
sens_RR_df %>% compare_RR_curve("auc36_cum3")

# ps df 5 ou 10

sens_metrics_df %>% view_fit_metrics("auc36_cum5")
sens_RR_df %>% compare_RR_curve("auc36_cum5")

# ps df 15 ou 10

sens_metrics_df %>% view_fit_metrics("auc36_cum7")
sens_RR_df %>% compare_RR_curve("auc36_cum7")
