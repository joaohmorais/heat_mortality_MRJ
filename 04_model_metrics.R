library(tidyverse)

load("../analises-calor/dlnm_model_results_hours2_2024_11_04.Rdata")
hour_metrics_df <- metrics_df

load("../analises-calor/dlnm_model_results_2024_11_04.Rdata")

metrics_df <- metrics_df %>% 
  filter(!grepl("num_hr_", metric)) %>% 
  add_row(hour_metrics_df)

load("../analises-calor/hw_results_cen0.Rdata")

metrics_df <- metrics_df %>% 
  add_row(hw_metrics_df)

table(metrics_df$metric)

metrics_df <- metrics_df %>% 
  mutate(metrics_group = case_when(
    grepl("auc", metric) & grepl("+ heat_index", metric) ~ "heat_index_auc",
    grepl("auc", metric) & grepl("+ temperature", metric) ~ "temperature_auc",
    grepl("auc", metric) ~ "auc",
    grepl("num_hr", metric) ~ "num_hr",
    grepl("q9", metric) & grepl("+ temperature", metric) ~ "hw_temperature",
    grepl("q9", metric) ~ "hw",
    grepl("hi_med", metric) ~ "heat_index",
    grepl("t_med", metric) ~ "temperature"
  )) %>% 
  relocate(metrics_group, .after=metric)

metrics_rank <- 
  metrics_df %>% 
  group_by(causa, grupo, metrics_group) %>% 
  slice_min(aic, with_ties = F) %>% 
  filter(grupo == "Idoso") %>% 
  ungroup() %>% 
  group_by(causa, grupo) %>% 
  mutate(ranking = min_rank(aic)) 

g_metrics <- 
  metrics_rank %>% 
  ungroup() %>% 
  left_join(cdc_groups %>% select(grupo, group_id), by=c("causa"="grupo")) %>% 
  mutate(medal = case_when(
    ranking == 1 ~ "gold",
    ranking == 2 ~ "silver",
    ranking == 3 ~ "bronze",
    TRUE ~ "other"
  ) %>% factor(levels = c("gold", "silver", "bronze", "other")),
  
  metrics_group = factor(
    metrics_group,
    levels = c("temperature", "heat_index", 
               "num_hr",
               "hw", "hw_temperature",
               "auc", "temperature_auc", "heat_index_auc") %>% rev(),
    labels = c("T only", "HI only",
               "Hours",
               "HW", "HW + T",
               "ARAC", "ARAC + T", "ARAC + HI") %>% rev()
  ),
  causa_eng = translate_cdc_groups(causa, reduced=2),
  causa_eng = fct_reorder(causa_eng, group_id)
  ) %>% 
  ggplot(aes(x=aic, y=metrics_group)) + 
  geom_point(aes(fill = medal, size=medal=="other"), color = "black", shape = 23) + 
  scale_fill_manual(values = c("#ffc000", "#dedede", "#cd6d32", "gray20"),
                    labels = c("Best", "2nd best", "3rd best", "Other")) +
  scale_size_manual(values = c(2,0.6)) +
  guides(size="none", fill = guide_legend(override.aes = list(size=4))) +
  facet_wrap(~causa_eng, scales = "free_x") + 
  labs(x="AIC", y="Model") +
  theme_bw() + 
  theme(legend.position = "top", legend.title = element_blank(),
        axis.text.x = element_text(size=4),
        axis.text.y = element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        panel.grid = element_line(size=0.4))

ggsave(
  g_metrics,
  filename = "img/img05_model_comparison.png",
  width = 7.5,
  units = "in",
  dpi = 300
)

ggsave(
  g_metrics,
  filename = "img/img05_model_comparison.jpeg",
  width = 7.5,
  units = "in",
  dpi = 300
)
