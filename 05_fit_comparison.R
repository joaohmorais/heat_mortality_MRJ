library(tidyverse)
library(mgcv)
library(patchwork)

Sys.setlocale("LC_ALL", "English")

# 0. Setup and data ----

source("00_aux_functions.R")

cdc_groups <- get_cdc_groups()

do_df <- read_csv("data/do_data.csv", show_col_types = F)
temp_df <- read_csv("data/temp_df.csv", show_col_types = F)

# Natural causes, for elderly
do_nat <- do_df %>% 
  filter(group_id == 17, age_group == "Idoso") 

# null model 

gam_nat_null <- gam(
  n ~ 1 + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
  data = do_nat,
  family = "nb"
)

# temperature model

cb_t <- crossbasis(
  do_nat$t_med,
  lag=10,
  argvar=list(fun="bs", df=3),
  arglag=list(fun="ns", df=4)
)

gam_nat_t <- gam(
  n ~ cb_t + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
  data = do_nat,
  family = "nb",
  method = "REML"
)

# temperature + AUC

cb_auc <- crossbasis(
  do_nat$auc32_cum7,
  lag=10,
  argvar=list(fun="ps", df=15),
  arglag=list(fun="ns", df=4)
)

gam_nat_auc <- gam(
  n ~ cb_t + cb_auc + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
  data = do_nat,
  family = "nb",
  paraPen = cbPen(cb_auc),
  method = "REML"
)

# temperature + heat wave

cb_hw <- crossbasis(
  as.integer(do_nat$q90_2d),
  lag=10,
  argvar = list(fun="ns", df=1),
  arglag=list(fun="ns", df=4)
)

gam_nat_hw <- gam(
  n ~ cb_t + cb_hw + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
  data = do_nat,
  family = "nb",
  method = "REML"
)

do_nat <- do_nat %>% 
  mutate(
    expected = fitted.values(gam_nat_null),
    fit_t = c(rep(NA, 10), fitted.values(gam_nat_t)),
    fit_auc = c(rep(NA, 10), fitted.values(gam_nat_auc)),
    fit_hw = c(rep(NA, 10), fitted.values(gam_nat_hw)),
  )