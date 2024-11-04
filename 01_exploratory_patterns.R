library(tidyverse)
library(mgcv)
library(patchwork)

# 0. Setup and data ----

source("00_aux_functions.R")

cdc_groups <- get_cdc_groups()

do_df <- read_csv("data/do_data.csv", show_col_types = F)
temp_df <- read_csv("data/temp_df.csv", show_col_types = F)

# Selected causes, for both age groups
do_selec <- do_df %>% 
  filter(group_id == 16) %>% 
  summarise(n=sum(n),
            .by=c(data_obito, dia_ano, time_id, covid_id))


# 1. Models ----

# Null model, only structure

gam_null_selec <- gam(
  n ~ 1 + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc") + s(covid_id, k=8, bs="cc"),
  data = do_selec,
  family = "poisson"
)

do_selec <- do_selec %>% 
  mutate(fitted = exp(predict(gam_null_selec, newdata=do_selec)) %>% as.vector()) 

# Model for heat index

temp_df <- temp_df %>% 
  mutate(time_id = seq_len(nrow(.)),
         dia_ano = yday(dia))

gam_hi <- gam(
  hi_med ~ 1 + s(time_id, k=12, bs = "tp") + s(dia_ano, k=6, bs="cc"),
  data = temp_df,
  family = "gaussian"
)

temp_df <- temp_df %>% 
  mutate(fitted_hi = predict(gam_hi, newdata=temp_df) %>% as.vector(),
         sd_hi = predict(gam_hi, newdata=temp_df, se.fit=T)$se.fit  %>% as.vector(),
         low_hi = fitted_hi - 1.96*sd_hi,
         upp_hi = fitted_hi + 1.96*sd_hi,
         diff_hi = hi_med - fitted_hi) 

# 2. Visualization ----

g_do <- 
  do_selec %>%  
  mutate(diff = n - fitted,
         low = fitted - 1.96 * sqrt(fitted),
         upp = fitted + 1.96 * sqrt(fitted)) %>% 
  ggplot(aes(x=data_obito)) +
  geom_segment(aes(xend=data_obito, y=fitted, yend=n, alpha=diff^2, color=diff), linewidth=0.2) + 
  geom_point(aes(y=n, color=diff), size=0.6) +
  scale_color_gradient2(low="blue", high="red", name = "Deaths (Observed - Expected)") + 
  scale_alpha_continuous(range = c(0.6, 1)) +
  geom_line(aes(y=fitted, linetype="Expected"), linewidth=0.8) +
  geom_line(aes(y=low, linetype="Lower"), linewidth=0.4) + 
  geom_line(aes(y=upp, linetype="Upper"), linewidth=0.4) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Deaths") +
  guides(alpha="none", 
         color = guide_colorbar(title.position="top"),
         linetype = guide_legend(direction = 'vertical', title.position = 'top')) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand=c(0,0)) +
  scale_y_continuous(name = "Daily deaths", breaks = seq(80, 200, by=20)) +
  
  annotate("segment", x = ymd("2023-07-01"), y = 195, xend = ymd("2023-10-17"), yend = 190,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +

  annotate("text", y = 194, x = ymd("2023-01-01"), label = "November 2023\nHeat Wave", size=2) +

  # influenza
  annotate("segment", x = ymd("2021-09-01"), y = 196, xend = ymd("2021-11-10"), yend = 190,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  annotate("text", y = 200, x = ymd("2021-06-01"), label = "Influenza", size=2) +

  # covid 1
  annotate("segment", x = ymd("2020-08-01"), y = 190, xend = ymd("2020-06-01"), yend = 188,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  # covid 2
  annotate("segment", x = ymd("2020-12-15"), y = 190, xend = ymd("2021-01-01"), yend = 180,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  annotate("text", y = 194, x = ymd("2020-11-01"), label = "Covid-19", size=2) +


  annotate("segment", x = ymd("2016-09-01"), y = 186, xend = ymd("2016-07-01"), yend = 176,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  annotate("text", y = 190, x = ymd("2016-09-01"), label = "Chikungunya", size=2) +
  
  
  # title
  
  annotate("text", y = 195, x = ymd("2014-01-01"), label = "A. Daily death count", fontface = "bold") +
  
  theme_minimal() + 
  theme(legend.position = "top",
        panel.grid.major = element_line(linewidth=0.2),
        panel.grid.minor.x = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=8),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=8),
        plot.margin = unit(c(0.2,0.2,0,0.2), "cm"),
        axis.line.y = element_line(color = "black", linewidth = 1),
        legend.title = element_text(hjust=0.5, size=8),
        legend.text = element_text(hjust=0.5, size=8))

g_hi <- temp_df %>% 
  ggplot(aes(x=dia)) + 
  geom_segment(aes(xend=dia, y=fitted_hi, yend=hi_med,
                   alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05),
                   color = hi_med
  ), linewidth=0.2) +
  geom_point(aes(y=hi_med, color = hi_med,
                 alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05)
  ), size=0.6) +
  geom_line(aes(y=fitted_hi, linewidth = "Average HI (trend)")) + 
  scale_linewidth_manual(values = c(0.8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  guides(alpha="none") +
  scale_color_gradientn(
    values = scales::rescale(c(15, 18, 20, 25, 28, 30, 32)),
    colors = c("#3422f5", "#3ba1eb", "#96d0fa", "#fffa91", "#f5733b", "#ba1a35")
  ) + 
  scale_x_date(expand=c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(16, 40, by=4), position = "right") +
  guides(color = guide_colorbar(title.position = "top")) +
  labs(x="Date", y = "Average HI (°C)", linewidth = "", color = "Daily average HI (°C)") +
  annotate("text", y = 39, x = ymd("2014-01-01"), label = "B. Daily average HI", fontface = "bold") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.title = element_text(hjust=0.5, size=8),
        legend.text = element_text(hjust=0.5, size=8),
        plot.margin = unit(c(0,0.2,0.2,0.2), "cm"),
        panel.grid.major = element_line(linewidth=0.2),
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_line(color = "black", linewidth = 1),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8)) 

g_composed <- g_do / g_hi

ggsave(
  g_composed, 
  filename = "img/img01_exploratory_patterns.tiff",
  width = 7.5,
  units = 'in',
  dpi = 300
)

ggsave(
  g_composed, 
  filename = "img/img01_exploratory_patterns.tiff",
  width = 7.5,
  units = 'in',
  dpi = 300
)
