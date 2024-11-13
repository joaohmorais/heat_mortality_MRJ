library(tidyverse)
library(mgcv)
library(patchwork)

Sys.setlocale("LC_ALL", "English")

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
  filename = "img/img01_exploratory_patterns.jpeg",
  width = 7.5,
  units = 'in',
  dpi = 300
)

# 3. AUC explanation ----

hourly_t <- read_csv("data/hourly_temp_23.csv", show_col_types = F) %>% 
  mutate(dt_registro = with_tz(dt_registro, "America/Sao_Paulo"))

nov23_wv <- hourly_t %>% 
  filter(dia >= '2023-11-11',
         dia <= '2023-11-18')

# expanded (interpolated)
nov23_wv_ex <- 
  approx(nov23_wv$dt_registro, nov23_wv$heat_index, n=100*length(unique(nov23_wv$dia))) %>% 
  as_tibble() %>% 
  mutate(dt_registro = seq(min(nov23_wv$dt_registro), max(nov23_wv$dt_registro), length.out = nrow(.))) 

nov23_daily <- temp_df %>% 
  filter(dia >= '2023-11-11',
         dia <= '2023-11-18') %>% 
  select(dia, t_med, hi_med) %>% 
  mutate(meio_dia = ymd_hm(paste0(dia, "12:00"), tz = "America/Sao_Paulo"))

nov23_auc <- temp_df %>% 
  select(dia, auc32, auc36) %>% 
  filter(dia >= '2023-11-11',
         dia <= '2023-11-18') %>% 
  mutate(meio_dia = ymd_hm(paste0(dia, "12:00"), tz = "America/Sao_Paulo"))

g_auc_nov <- 
  nov23_auc %>% 
  ggplot(aes(x=dia, y=1)) + 
  geom_line() + 
  geom_point(aes(size=auc36, fill=auc36), color="black", shape=21) +
  theme_minimal() +
  guides(size="none", fill = guide_colorbar(title.position="top")) +
  geom_vline(xintercept = seq(ymd("2023-11-11"), ymd("2023-11-20"), by=1) 
             + seq(0.618, 0.325, length.out=10)
             , alpha=0.2) +
  scale_size_continuous(range = c(2, 16),limits = c(0,120)
  ) +
  scale_x_date(date_breaks = "1 day", 
               labels = function(x) {
                 format(x, "%b %d") %>% str_to_title()
               }, position = "top") +
  labs(x="Data", fill = "(a), Accumulated Heat Area (°C * h)") +
  scale_fill_viridis_c(option = "C", 
                       values = seq(0, 120/max(nov23_auc$auc36), length.out=6),
                       breaks = seq(0, 120, by=30),
                       limits = c(0,120)) +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        panel.grid = element_blank(),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.title = element_text(hjust=0.5, size=8.6, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=10, face="bold"),
        plot.margin = unit(c(0.2, 0.2, 0, 0.2), "cm"))

g_ic_nov <- 
  nov23_wv %>% 
  ggplot(aes(x=dt_registro)) + 
  geom_ribbon(
    data = nov23_wv_ex %>% 
      mutate(y_min = 36, y_max = ifelse(y >= 36, y, 36)),
    aes(ymin = y_min, ymax=y_max),
    fill = "orange"
  ) +
  geom_hline(yintercept = 36, linetype = "dashed") +
  scale_y_continuous(breaks = seq(20, 50, by=2)
  ) +
  scale_x_datetime(
    expand=c(0,0),
    date_breaks = "6 hours",
    labels = function(x) {
      
      case_when(
        hour(x) == 0 ~ "\n",
        TRUE ~ format(x, "\n%Hh")
      )
    }
  ) +
  geom_vline(xintercept = nov23_wv %>% filter(hour(dt_registro)==0) %>% pull(dt_registro), alpha=0.2) +
  labs(x="Hour", y = "Heat Index (°C)") +
  geom_line(aes(y=heat_index)) +
  geom_line(
    data = nov23_daily,
    aes(x=meio_dia, y=hi_med, color = "Average daily HI")
  ) + 
  geom_point(data = nov23_daily,
             aes(x=meio_dia, y=hi_med, color = "Average daily HI")) +
  theme_minimal() + 
  annotate("text",
           x=ymd_hm("2023-11-15 00:00"),
           y=50,
           label = "(b), Hourly heat index (°C)",
           fontface="bold",
           size=3.2) +
  scale_color_manual(values = c("#c70559")) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0.2, 0, 0.2), "cm"),
        axis.line.y = element_line(linewidth = 0.6, color = "black"),
        axis.title.y = element_text(size=6, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(vjust=12, size=6),
        axis.text.y = element_text(size=6),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(t = 0, b=0, unit='cm'),
        legend.box.margin = margin(t = 0, b=0, unit='cm'))

g_nov_all <- 
  g_auc_nov / g_ic_nov + 
    plot_layout(heights = c(1,4)) + 
    plot_annotation(title = "November 2023 Heat Wave (Nov 11th-18th)",
                    theme = theme(
                      plot.title = element_text(hjust = 0.5, face="bold", size=10)))

ggsave(
  g_nov_all,
  filename = "img/img04_nov_heat_wave.tiff",
  width = 7.5,
  units = "in",
  dpi = 300
)

ggsave(
  g_nov_all,
  filename = "img/img04_nov_heat_wave.jpeg",
  width = 7.5,
  units = "in",
  dpi = 300
)

# EPI Poster graphs ----

q025 <- quantile(temp_df$hi_med, probs = 0.025)
q05 <- quantile(temp_df$hi_med, probs = 0.05)
q10 <- quantile(temp_df$hi_med, probs = 0.1)
q90 <- quantile(temp_df$hi_med, probs = 0.9)
q95 <- quantile(temp_df$hi_med, probs = 0.95)
q975 <- quantile(temp_df$hi_med, probs = 0.975)
q99 <- quantile(temp_df$hi_med, probs = 0.99)

temp_df %>% 
  filter(dia >= '2023-10-01') %>% 
  ggplot(aes(x=dia)) + 
  geom_segment(aes(xend=dia, y=fitted_hi, yend=hi_med,
                   alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05),
                   color = hi_med
  )) +
  geom_point(aes(y=hi_med, color = hi_med,
                 alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05)
  )) +
  geom_line(aes(y=fitted_hi, linewidth = "IC (tendência)")) + 
  geom_hline(yintercept = q025, linetype = "dashed", color = "purple") +
  annotate("text", x=ymd('2024-07-15'), y=q025+0.4, label = "Q02.5", color = "purple", size = 2.8) +
  geom_hline(yintercept = q05, linetype = "dashed", color = "blue") +
  annotate("text", x=ymd('2024-07-15'), y=q05+0.4, label = "Q05", color = "blue", size = 2.8) +
  geom_hline(yintercept = q10, linetype = "dashed", color = "lightblue") +
  annotate("text", x=ymd('2024-07-15'), y=q10+0.4, label = "Q10", color = "lightblue", size = 2.8) +
  geom_hline(yintercept = q95, linetype = "dashed", color = "yellow3") +
  annotate("text", x=ymd('2024-07-15'), y=q95+0.5, label = "Q95", color = "yellow3", size = 2.8) +
  geom_hline(yintercept = q975, linetype = "dashed", color = "orange3") +
  annotate("text", x=ymd('2024-07-15'), y=q975+0.5, label = "Q97.5", color = "orange3", size = 2.8) +
  geom_hline(yintercept = q99, linetype = "dashed", color = "red") +
  annotate("text", x=ymd('2024-07-15'), y=q99+0.5, label = "Q99", color = "red", size = 2.8) +
  scale_linewidth_manual(values = c(0.8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  guides(alpha="none") +
  scale_color_gradientn(
    values = scales::rescale(c(15, 18, 20, 25, 28, 30, 32)),
    colors = c("#3422f5", "#3ba1eb", "#96d0fa", "#fffa91", "#f5733b", "#ba1a35")
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  scale_y_continuous(breaks = seq(16, 40, by=4)) +
  guides(color = guide_colorbar(title.position = "top")) +
  labs(x="Data", y = "IC médio (°C)", linewidth = "\n\n", color = "IC médio diário (°C)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.title = element_text(hjust=0.5, size=10),
        legend.text = element_text(hjust=0.5, size=10),
        plot.margin = unit(c(0,0.2,0.2,0.2), "cm"),
        panel.grid.major = element_line(linewidth=0.2),
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_line(color = "black", linewidth = 1),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10)) 


g_serie_ic <- temp_df %>% 
  ggplot(aes(x=dia)) + 
  geom_segment(aes(xend=dia, y=fitted_hi, yend=hi_med,
                   alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05),
                   color = hi_med
  )) +
  geom_point(aes(y=hi_med, color = hi_med,
                 alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05)
  )) +
  geom_line(aes(y=fitted_hi, linewidth = "IC (tendência)"), color = "black") + 
  scale_linewidth_manual(values = c(1.2)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  guides(alpha="none") +
  scale_color_gradientn(
    values = scales::rescale(c(15, 18, 20, 25, 28, 30, 32)),
    colors = c("#3422f5", "#3ba1eb", "#96d0fa", "#fffa91", "#f5733b", "#ba1a35")
  ) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(16, 40, by=4)) +
  guides(color = guide_colorbar(title.position = "top")) +
  labs(x="Data", y = "IC médio (°C)", linewidth = "\n\n", color = "IC médio diário (°C)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.title = element_text(hjust=0.5, size=10, color = "white"),
        legend.text = element_text(hjust=0.5, size=10, color = "white"),
        plot.margin = unit(c(0,0.2,0.2,0.2), "cm"),
        panel.grid.major = element_line(linewidth=0.2, color = "white"),
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_line(color = "white", linewidth = 1),
        axis.text.y = element_text(size=10, color = "white"),
        axis.text.x = element_text(size=10, color = "white"),
        axis.title.y = element_text(size=10, color = "white"),
        axis.title.x = element_text(size=10, color = "white"))

ggsave(
  g_serie_ic,
  filename = "serie_ic.png",
  bg = "transparent",
  width = 3200,
  units = "px"
)
