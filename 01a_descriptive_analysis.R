library(tidyverse)

source("00_aux_functions.R")

do_df <- read_csv("data/do_data.csv", show_col_types = F)

cdc_groups <- get_cdc_groups()

do_df <- do_df %>% 
  left_join(cdc_groups %>% select(group_id, grupo),
            by="group_id") %>% 
  relocate(grupo, .after=group_id)

do_df %>% 
  summarise(total = sum(n),
            median = median(n),
            .by = c(grupo, age_group)) %>% 
  left_join(
    do_df %>% 
      group_by(grupo, age_group) %>% 
      slice_max(order_by = n) %>% 
      ungroup() %>% 
      summarise(data_obito = max(data_obito), .by=c(grupo, age_group, n)) %>% 
      select(grupo, age_group, data_obito, max=n),
    by = c("grupo", "age_group")
  ) %>% 
  mutate(data_obito = format(data_obito, "%b %d, %Y") %>% str_to_title()) %>% 
  pivot_wider(names_from = age_group, values_from = c(total, median, data_obito, max)) %>% 
  mutate(max_Jovem = paste0(max_Jovem, " (", data_obito_Jovem, ")"),
         max_Idoso = paste0(max_Idoso, " (", data_obito_Idoso, ")")) %>% 
  select(-starts_with("data_obito")) %>% 
  write_csv("summary_deaths.csv")

temp_df %>% 
  select(dia, t_med, hi_med, num_hr_36, num_hr_40, num_hr_44, auc32, auc36)  %>% 
  pivot_longer(cols = t_med:auc36, names_to = "metric", values_to = "value") %>% 
  group_by(metric) %>% 
  summarise(min = min(value), mean = mean(value), q95 = quantile(value, probs=0.95),
            q99 = quantile(value, probs = 0.99),)
