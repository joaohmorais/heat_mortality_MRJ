library(tidyverse)
library(ggpubr)
library(patchwork)

source("00_aux_functions.R")

load("results/dlnm_model_results.Rdata")

cdc_groups <- get_cdc_groups()

# Tmed and HImed effect ----

plot_continuous_effect <- function(df, variable, age_group) {
  
  if (variable == "hi_med") {
    x_lab <- "Daily Average Heat Index (°C)"
    title <- "Heat Index effect, by causes"
  } else if (variable == "t_med") {
    x_lab <- "Daily Average Temperature (°C)"
    title <- "Temperature effect, by causes"
  }
  
  if (age_group == "Idoso") {
    subtitle <- "Elderly (>= 65 y.)"
  } else {
    subtitle <- "Young (< 65 y.)"
  }
  
  
  df %>% 
    filter(metric == variable, grupo == age_group) %>% 
    left_join(cdc_groups %>% select(grupo, group_id), by=c(causa = "grupo")) %>% 
    mutate(sign = case_when(
      RR_low > 1 ~ "Risk",
      RR_upp < 1 ~ "Protective effect  ",
      TRUE ~ "Non significant  "
    ) %>% factor(levels = c("Non significant  ", "Protective effect  ", "Risk")),
    causa = translate_cdc_groups(causa, reduced=T),
    causa = reorder(causa, group_id),
    RR_upp = case_when(RR_upp > 10 ~ 10, TRUE ~ RR_upp) # truncating RR limit
    ) %>% 
    ggplot(aes(x=at)) + 
    geom_ribbon(aes(ymin=RR_low, ymax=RR_upp), alpha = 0.2) + 
    geom_line(aes(y=RR, color = sign, group = causa), linewidth=0.3) + 
    scale_color_manual(values = c("black", "blue", "red")) +
    geom_hline(yintercept = 1, linetype="dashed", linewidth=0.2) +
    labs(x=x_lab, y = "Relative Risk (RR)", color = "", title = title, subtitle = subtitle) +
    facet_wrap(~causa, scales = "free", ncol=3) + 
    guides(color = guide_legend(override.aes = list(linewidth = 2))) +
    theme_minimal() + 
    theme(legend.position = "top", plot.title = element_text(hjust=0.5), 
          plot.subtitle = element_text(hjust=0.5, face = "bold", size=8),
          legend.title = element_blank(),
          legend.text = element_text(size=6),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=5),
          axis.title = element_text(size=8),
          strip.text = element_text(size=6),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth=0.2),
          plot.margin = margin(t = 0, 
                               r = 1, 
                               b = 0, 
                               l = 1),
          panel.margin.y = unit(0, "lines"),
          panel.margin.x = unit(0, "lines"),
          legend.key.width = unit(0.5, "cm")) 
} 

## HImed ----

g_hi_jovem <- RR_df %>% plot_continuous_effect(variable = "hi_med", age_group = "Jovem")
g_hi_idoso <- RR_df %>% plot_continuous_effect(variable = "hi_med", age_group = "Idoso")

g_legend <- get_legend(g_hi_jovem) %>% as_ggplot()

g_hi_jovem <- g_hi_jovem + guides(color="none") + theme(plot.title = element_blank())
g_hi_idoso <- g_hi_idoso + guides(color="none") + theme(plot.title = element_blank())

g_hi_both <- 
  g_legend / (g_hi_jovem | g_hi_idoso) + plot_layout(heights = c(1,16)) +
  plot_annotation(
    title = "Daily Heat Index Effect",
    theme = theme(plot.title = element_text(hjust = 0.5, face="bold", size=10))
  )

ggsave(
  g_hi_both,
  filename = "img/img02_hi_med_effect.tiff",
  width = 7.5,
  units = "in",
  dpi=300
)

ggsave(
  g_hi_both,
  filename = "img/img02_hi_med_effect.jpeg",
  width = 7.5,
  units = "in",
  dpi=300
)

## Tmed ----

g_t_jovem <- RR_df %>% plot_continuous_effect(variable = "t_med", age_group = "Jovem")
g_t_idoso <- RR_df %>% plot_continuous_effect(variable = "t_med", age_group = "Idoso")

g_legend <- get_legend(g_t_jovem) %>% as_ggplot()

g_t_jovem <- g_t_jovem + guides(color="none") + theme(plot.title = element_blank())
g_t_idoso <- g_t_idoso + guides(color="none") + theme(plot.title = element_blank())

g_t_both <- 
  g_legend / (g_t_jovem | g_t_idoso) + plot_layout(heights = c(1,16)) +
  plot_annotation(
    title = "Daily Temperature Effect",
    theme = theme(plot.title = element_text(hjust = 0.5, face="bold", size=10))
  )

ggsave(
  g_t_both,
  filename = "img/imgs01_t_med_effect.tiff",
  width = 7.5,
  units = "in",
  dpi=300
)

ggsave(
  g_t_both,
  filename = "img/imgs01_t_med_effect.jpeg",
  width = 7.5,
  units = "in",
  dpi=300
)

# Number of hours graph ----

RR_df_h <- RR_df %>% 
  filter(grepl("num_hr", metric)) %>% 
  mutate(hour_trunc = floor(at)) %>% 
  summarise(
    RR = median(RR),
    RR_low = median(RR_low),
    RR_upp = median(RR_upp),
    .by = c(causa, grupo, metric, hour_trunc)
  ) %>% 
  mutate(faixa = gsub("num_hr_", "", metric),
         sign = case_when(
           RR_low > 1 ~ "Risco",
           RR_upp < 1 ~ "Proteção",
           TRUE ~ "Não significativo"
         ))

g_nat <- 
  RR_df_h %>% 
  mutate(RR_cat = case_when(
    sign == "Não significativo" ~ NA_character_,
    TRUE ~ cut(RR, breaks = c(0, 1, 1.25, 1.5, 1.75, 2, 3, Inf))
  ),
  RR_cat = factor(RR_cat,
                  levels = c('(0,1]', '(1,1.25]', '(1.25,1.5]', '(1.5,1.75]', '(1.75,2]', '(2,3]', '(3,Inf]')
  ),
  faixa = as.integer(faixa)
  ) %>% 
  filter(hour_trunc>=1, grupo == "Idoso") %>% 
  filter(causa == "Causas naturais") %>% 
  ggplot(aes(x=hour_trunc, y=faixa)) + 
  geom_point(data=tibble(x=5, y=35), 
             aes(x,y, shape="Non-significant"),
             color = "#c7c7c7",
             size=8,
             inherit.aes = F) +
  geom_tile(aes(fill = RR_cat), color = "black") +
  geom_text(aes(label=round(RR, 1), color = RR_cat), size = 2) +
  scale_y_reverse(breaks = seq(32, 44), expand=c(0,0)) +
  scale_shape_manual(values = c(15)) +
  scale_x_continuous(breaks = seq(1, 24, by=1), expand=c(0,0), position = "top") +
  scale_color_manual(values = c("#5f6a4d",
                                "#e17e75",
                                "#b55148",
                                "#bb2a4f",
                                "#a71650",
                                "#e89ccd",
                                "#febefd"
  ), drop=F, na.value = "#c7c7c7"
  ) +
  scale_fill_manual(
    values = c("#e6f5d0",
               "#feebe2",
               "#fcc5c0",
               "#fa9fb5",
               "#f768a1",
               "#c51b8a",
               "#7a0177"
    ), drop=F, na.value = "#bdbdbd"
  ) +
  labs(x="Amount of hours", y = "Heat Index Threshold (°C)", fill = "Relative Risk (RR)", title = "Natural causes", shape = "\n\n\n") +
  guides(fill = guide_colorsteps(title.position="top", 
                                 direction = 'vertical'), color = "none") +
  theme_minimal() + 
  theme(legend.position = "top",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(1.2, "cm"),
        legend.title = element_text(hjust=0.5, face = "bold"),
        plot.title = element_text(hjust=0.5, face = "bold", size=10),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=8, face="bold"),
        axis.text = element_text(size=6),
        legend.text = element_text(size=10))

g_i <- list()

for (group_i in cdc_groups$grupo[1:16]) {
  g_i[[group_i]] <-  RR_df_h %>% 
    mutate(RR_cat = case_when(
      sign == "Não significativo" ~ NA_character_,
      TRUE ~ cut(RR, breaks = c(0, 1, 1.25, 1.5, 1.75, 2, 3, Inf))
    ),
    RR_cat = factor(RR_cat,
                    levels = c('(0,1]', '(1,1.25]', '(1.25,1.5]', '(1.5,1.75]', '(1.75,2]', '(2,3]', '(3,Inf]')
    ),
    faixa = as.integer(faixa)
    ) %>% 
    filter(hour_trunc>=1, grupo == "Idoso") %>% 
    filter(causa == group_i) %>% 
    ggplot(aes(x=hour_trunc, y=faixa)) + 
    geom_tile(aes(fill = RR_cat), color = "black") +
    scale_y_reverse(breaks = seq(32, 44)) +
    scale_x_continuous(breaks = seq(1, 24, by=1), position = "top") +
    scale_fill_manual(
      values = c("#e6f5d0",
                 "#feebe2",
                 "#fcc5c0",
                 "#fa9fb5",
                 "#f768a1",
                 "#c51b8a",
                 "#7a0177"
      ), drop=F, na.value = "#bdbdbd"
    ) +
    labs(x=translate_cdc_groups(group_i, reduced = T), 
         y = "Heat Index Threshold (°C)", fill = "RR") +
    guides(fill = "none", color = "none") +
    theme_void() + 
    theme(plot.title = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_text(face="bold", size=6),
          axis.text = element_blank())
}

hour_plot_layout <- "
  ABCDE
  FGGGH
  IGGGJ
  KGGGL
  MNOPQ
"

library(ggpubr)

g_legend <- get_legend(g_nat) %>% as_ggplot()


g_all <- 
  g_i$`Influenza e Pneumonia` + 
  g_i$`Respiratórias Crônicas Inferiores` + 
  g_i$`Outras doenças respiratórias` + 
  g_i$`Doenças hipertensivas` +
  g_i$IAM + 
  g_i$`Parada Cardíaca` + 
  (g_nat + guides(fill="none", shape = "none", color = "none")) +
  g_i$AVC + 
  g_i$`Outras doenças do Aparelho Circulatório` + 
  g_i$Neoplasias + 
  g_i$`Alzheimer e demência` + 
  g_i$Diabetes + 
  g_i$`Falência Renal` + 
  g_i$Sepse + 
  g_i$`Trato urinário` +
  g_i$`Causas indeterminadas` + 
  g_i$`Causas selecionadas` + 
  plot_layout(design = hour_plot_layout)

ggsave(
  g_all,
  filename = "img/img03_heat_hours.tiff",
  width = 7.5,
  units = 'in',
  dpi=300
)

ggsave(
  g_legend,
  filename = 'img03_legend.tiff',
  dpi=300
)

# CROSSING POINTS ANALYSIS ----

## Graph ----

RR_df <- RR_df %>% 
  mutate(faixa = gsub("num_hr_", "", metric) %>% as.factor())

RR_selec_hr <- RR_df %>% 
  filter(faixa %in% c(36, 40, 44), grupo == "Idoso",
         causa %in% c("Falência Renal", "Causas naturais",
                      "Causas selecionadas", "Diabetes", "Doenças hipertensivas",
                      "IAM")
  ) 

get_crossing_points <- function(df, threshold) {
  df %>% 
    nest(.by=c(causa, grupo, faixa)) %>% 
    mutate(coords = map(
      data, function(data) {
        approx(data$at, data$RR, n=1e4) %>% 
          as_tibble() %>% 
          rename(at=1, RR=2)
      }
    )) %>% 
    select(causa, grupo, faixa, coords) %>% 
    unnest(coords) %>% 
    filter(RR >= threshold) %>% 
    slice_min(at, by=c(causa, faixa)) %>% 
    mutate(at_round=round(at)) %>% 
    mutate(threshold = threshold) %>% 
    select(causa, grupo, at, at_round, RR, threshold, faixa)  
}

crossing_points_hr <- RR_selec_hr %>% 
  get_crossing_points(1.25) %>% 
  add_row(RR_selec_hr %>% get_crossing_points(1.5)) %>% 
  add_row(RR_selec_hr %>% get_crossing_points(2))


g_num_hr <- RR_selec_hr %>% 
  left_join(cdc_groups[,c(1,2)], by = c("causa"="grupo")) %>% 
  mutate(causa = translate_cdc_groups(causa),
         causa = fct_reorder(causa, group_id)) %>% 
  ggplot(aes(x=at)) + 
  #geom_ribbon(aes(ymin=RR_low, ymax=RR_upp, fill=faixa, group=faixa), alpha=0.1) + 
  geom_line(aes(y=RR, color=faixa, group=faixa), linewidth=0.6) +
  geom_hline(yintercept = 1.25, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 1) +
  geom_point(
    data = crossing_points %>% mutate(causa = translate_cdc_groups(causa)),
    aes(x=at, y=threshold, color = faixa), size=1
  ) +
  geom_text(
    data = crossing_points %>% mutate(causa = translate_cdc_groups(causa)),
    aes(x=at, y=threshold, label = paste0(round(at, 1)), color = faixa),
    nudge_y = ifelse(crossing_points$faixa == 36, -0.15, 0.1),
    nudge_x = ifelse(crossing_points$faixa == 44, -0.6, 
                     ifelse(crossing_points$faixa == 40, -0.3, 0)),
    size=2.4
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.25, 1.5, 2, 2.5)) +
  scale_color_manual(values = c("orange2", "red2", "purple3"),
                     labels = c("HI >= 36°C",
                                "HI >= 40°C",
                                "HI >= 44°C")) +
  scale_fill_manual(values = c("yellow4", "orange", "red2")) +
  facet_wrap(~causa, scales = "free") + 
  coord_cartesian(ylim = c(1, 2.5)) +
  labs(x="Amount of hours", y = "Relative Risk (RR)", title = "(a) Mortality Relative Risk (RR) according to amount of hours of exposure") +
  theme_minimal() + 
  theme(axis.text = element_text(size=6),
        axis.title = element_text(size=8),
        strip.text = element_text(size=8),
        legend.text = element_text(size=8),
        plot.title = element_text(size=10, face="bold", hjust=0.5),
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.grid = element_line(linewidth = 0.4),
        legend.position = "top")

RR_selec_auc <- RR_df %>% 
  filter(metric == "auc36", grupo == "Idoso",
         causa %in% c("Falência Renal", "Causas naturais",
                      "Causas selecionadas", "Diabetes", "Doenças hipertensivas",
                      "IAM")
  ) %>% 
  mutate(faixa = metric)

crossing_points_auc <- RR_selec_auc %>% 
  mutate(faixa = metric) %>% 
  get_crossing_points(1.25) %>% 
  add_row(RR_selec_auc %>% get_crossing_points(1.5)) %>% 
  add_row(RR_selec_auc %>% get_crossing_points(2))

g_auc <- RR_selec_auc %>% 
  left_join(cdc_groups[,c(1,2)], by = c("causa"="grupo")) %>% 
  mutate(causa = translate_cdc_groups(causa),
         causa = fct_reorder(causa, group_id)) %>% 
  ggplot(aes(x=at)) + 
  #geom_ribbon(aes(ymin=RR_low, ymax=RR_upp, fill=faixa, group=faixa), alpha=0.1) + 
  geom_line(aes(y=RR), linewidth=0.6, color = "red3") +
  geom_hline(yintercept = 1.25, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 1) +
  geom_point(data=crossing_points_auc %>% mutate(causa=translate_cdc_groups(causa)),
             aes(x=at, y=RR), color= "red3", size=1) +
  ggrepel::geom_text_repel(
    data = crossing_points_auc %>% mutate(causa=translate_cdc_groups(causa)),
    aes(x=at, y=threshold, label = paste0(round(at, 1))),
    size=2.4,
    nudge_x = 1.9,
    nudge_y = -0.1) +
  labs(x="HAAT (°C*h)", y = "Relative Risk (RR)", title = "(b) Mortality Relative Risk (RR) according to daily heat area values") +
  scale_y_continuous(breaks = c(0.5, 1, 1.25, 1.5, 2, 2.5)) +
  scale_color_manual(values = c("orange2", "red2", "purple3")) +
  facet_wrap(~causa, scales = "free") + 
  coord_cartesian(ylim = c(1, 2.5)) +
  theme_minimal() + 
  theme(axis.text = element_text(size=6),
        axis.title = element_text(size=8),
        strip.text = element_text(size=8),
        legend.position = "top",
        plot.title = element_text(size=10, face="bold", hjust=0.5),
        panel.grid = element_line(linewidth = 0.4),
        plot.margin = unit(c(0,0,0,0), "cm"))

g_auc <- RR_selec_auc %>% 
  left_join(cdc_groups[,c(1,2)], by = c("causa"="grupo")) %>% 
  mutate(causa = translate_cdc_groups(causa),
         causa = fct_reorder(causa, group_id)) %>% 
  ggplot(aes(x=at)) + 
  #geom_ribbon(aes(ymin=RR_low, ymax=RR_upp, fill=faixa, group=faixa), alpha=0.1) + 
  geom_line(aes(y=RR), linewidth=0.6, color = "red3") +
  geom_hline(yintercept = 1.25, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black", linewidth=0.2) +
  geom_hline(yintercept = 1) +
  geom_point(data=crossing_points_auc %>% mutate(causa=translate_cdc_groups(causa)),
             aes(x=at, y=RR), color= "red3", size=1) +
  ggrepel::geom_text_repel(
    data = crossing_points_auc %>% mutate(causa=translate_cdc_groups(causa)),
    aes(x=at, y=threshold, label = paste0(round(at, 1))),
    size=2.4,
    nudge_x = 1.9,
    nudge_y = -0.1) +
  labs(x="HAAT (°C*h)", y = "Relative Risk (RR)", title = "(b) Mortality Relative Risk (RR) according to daily heat area values") +
  scale_y_continuous(breaks = c(0.5, 1, 1.25, 1.5, 2, 2.5)) +
  scale_color_manual(values = c("orange2", "red2", "purple3")) +
  facet_wrap(~causa, scales = "free") + 
  coord_cartesian(ylim = c(1, 2.5)) +
  theme_minimal() + 
  theme(axis.text = element_text(size=6),
        axis.title = element_text(size=8),
        strip.text = element_text(size=8),
        legend.position = "top",
        plot.title = element_text(size=10, face="bold", hjust=0.5),
        panel.grid = element_line(linewidth = 0.4),
        plot.margin = unit(c(0,0,0,0), "cm"))

g_risk_thresholds <- g_num_hr / g_auc


ggsave(g_risk_thresholds,
       filename="img/risk_thresholds_larger.eps", 
       width = 7.5,
       height = 9,
       units = "in",
       dpi=300
)

ggsave(g_risk_thresholds,
       filename="img/risk_thresholds_larger.jpeg", 
       width = 7.5,
       height = 8,
       units = "in",
       dpi=300
)

### Supplementary: all causes ----

RR_auc_df <- RR_df %>% 
  filter(metric == "auc36", grupo == "Idoso", at > 0
  ) %>% 
  mutate(faixa = metric)

get_significance_cp <- function(df, breaks) {
  df %>% 
    mutate(RR = case_when(
      RR_low > 1 | RR_upp < 1 ~ RR,
      TRUE ~ NA_real_
    )) %>% 
    mutate(RR_cat = cut(RR, breaks = c(0, 1, 1.25, 1.5, 2, Inf), include.lowest = TRUE)) %>% 
    group_by(causa) %>% 
    mutate(change = (RR_cat != lag(RR_cat)) | (!is.na(RR_cat) & is.na(lag(RR_cat))))
}

RR_auc_cp <- 
  RR_auc_df %>% get_significance_cp() %>% 
  filter(change)

RR_auc_NA <- 
  RR_auc_df %>% 
  mutate(RR = case_when(
    RR_low > 1 | RR_upp < 1 ~ RR,
    TRUE ~ NA_real_
  )) %>% 
  mutate(turn_na = is.na(RR) & !is.na(lag(RR))) %>% 
  filter(turn_na)

RR_1.25_med <- RR_auc_cp %>% filter(RR_cat == "(1.25,1.5]") %>% pull(at) %>% median()
RR_1.50_med <- RR_auc_cp %>% filter(RR_cat == "(1.5,2]") %>% pull(at) %>% median()
RR_2.0_med <- RR_auc_cp %>% filter(RR_cat == "(2,Inf]") %>% pull(at) %>% median()

g_cross_all <- 
  RR_auc_df %>% 
  mutate(RR = case_when(
    RR_low > 1 | RR_upp < 1 ~ RR,
    TRUE ~ NA_real_
  )) %>% 
  left_join(cdc_groups %>% select(1,2), by = c("causa"="grupo")) %>% 
  mutate(causa = translate_cdc_groups(causa)) %>% 
  mutate(RR_cat = cut(RR, breaks = c(0, 1, 1.25, 1.5, 2, Inf), include.lowest = TRUE)) %>% 
  ggplot(aes(x=at, y=fct_reorder(causa, -group_id))) + 
  geom_vline(xintercept = c(RR_1.25_med, RR_1.50_med, RR_2.0_med),
             color = c("#df65b0", "#dd1c77", "#980043"),
             linetype="dashed", linewidth=1) +
  # annotate(
  #   "text",
  #   x=c(RR_1.25_med, RR_1.50_med, RR_2.0_med),
  #   y=c("Influenza and pneumonia"),
  #   label = c("Median RR > 1.25", "Median RR > 1.5", "Median RR > 2.0")
  # ) +
  geom_line(aes(color=RR_cat, group = causa), linewidth=2.5) + 
  geom_point(
    data = RR_auc_NA %>% mutate(causa=translate_cdc_groups(causa)),
    aes(x=at, y=causa), color = "gray40", size=5
  ) +
  geom_point(
    data = RR_auc_cp %>% mutate(causa=translate_cdc_groups(causa)),
    aes(x=at, y=causa, color = RR_cat), size=5
  ) +
  scale_x_continuous(breaks = seq(0,120,by=15), name="HAAT (°C*h)") +
  scale_color_manual(values = c("#d7b5d8", "#df65b0", "#dd1c77", "#980043"),
                     labels = function(breaks) {
                       case_when(
                         is.na(breaks) ~ "Non-significant",
                         breaks == "[0,1]" ~ "RR <= 1",
                         breaks == "(1,1.25]" ~ "RR > 1",
                         breaks == "(1.25,1.5]" ~ "RR > 1.25",
                         breaks == "(1.5,2]" ~ "RR > 1.5",
                         breaks == "(2,Inf]" ~ "RR > 2"
                       )
                     }) + 
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=8),
        legend.title = element_blank())

ggsave(g_cross_all,
       filename="img/supp_risk_thresholds.eps", 
       width = 7.5,
       height = 8,
       units = "in",
       dpi=300
)

ggsave(g_cross_all,
       filename="img/supp_risk_thresholds.jpeg", 
       width = 7.5,
       height = 8,
       units = "in",
       dpi=300
)

## Table ----

temp_df <- read_csv("data/temp_df.csv", show_col_types = F)

temp_metrics <- 
  temp_df %>% 
  summarise(tmed_min = min(t_med),
            tmed_q5 = quantile(t_med, 0.05),
            tmed_median = median(t_med),
            tmed_q95 = quantile(t_med, 0.95),
            tmed_max = max(t_med),
            himed_min = min(hi_med),
            himed_q5 = quantile(hi_med, 0.05),
            himed_median = median(hi_med),
            himed_q95 = quantile(hi_med, 0.95),
            himed_max = max(hi_med))

t_med_ecdf <- ecdf(temp_df$t_med)
hi_med_ecdf <- ecdf(temp_df$hi_med)

RR_t_hi_df <- RR_df %>% 
  filter(metric %in% c("t_med", "hi_med"))

RR_summary <- 
  # lowest
  RR_t_hi_df %>% 
  filter(metric == "t_med", at == temp_metrics$tmed_min) %>% 
  mutate(scenario = "lowest") %>% 
  add_row(
    RR_t_hi_df %>% 
      filter(metric == "hi_med", at == temp_metrics$himed_min) %>% 
      mutate(scenario = "lowest")
  ) %>% 
  # Q5
  add_row(
    RR_t_hi_df %>% 
      filter(metric == "t_med") %>% 
      mutate(q5_diff = abs(at - temp_metrics$tmed_q5)) %>% 
      slice_min(n=1, order_by = q5_diff, by = c(causa, grupo, metric), with_ties = F) %>% 
      mutate(scenario = "q5") %>% 
      select(-q5_diff)
  ) %>% 
  add_row(
    RR_t_hi_df %>% 
      filter(metric == "hi_med") %>% 
      mutate(q5_diff = abs(at - temp_metrics$himed_q5)) %>% 
      slice_min(n=1, order_by = q5_diff, by = c(causa, grupo, metric), with_ties = F) %>% 
      mutate(scenario = "q5") %>% 
      select(-q5_diff)
  ) %>% 
  # Q95
  add_row(
    RR_t_hi_df %>% 
      filter(metric == "t_med") %>% 
      mutate(q95_diff = abs(at - temp_metrics$tmed_q95)) %>% 
      slice_min(n=1, order_by = q95_diff, by = c(causa, grupo, metric), with_ties = F) %>% 
      mutate(scenario = "q95") %>% 
      select(-q95_diff)
  ) %>% 
  add_row(
    RR_t_hi_df %>% 
      filter(metric == "hi_med") %>% 
      mutate(q95_diff = abs(at - temp_metrics$himed_q95)) %>% 
      slice_min(n=1, order_by = q95_diff, by = c(causa, grupo, metric), with_ties = F) %>% 
      mutate(scenario = "q95") %>% 
      select(-q95_diff)
  ) %>% 
  add_row(
    RR_t_hi_df %>% 
      filter(metric == "t_med", at == temp_metrics$tmed_max) %>% 
      mutate(scenario = "max")
  ) %>% 
  add_row(
    RR_t_hi_df %>% 
      filter(metric == "hi_med", at == temp_metrics$himed_max) %>% 
      mutate(scenario = "max")
  )

RR_t_hi_df <- 
  RR_t_hi_df %>% 
  group_by(causa, grupo, metric) %>% 
  mutate(ascending = 
           (metric == "t_med" & at > temp_metrics$tmed_median) | 
           (metric == "hi_med" & at > temp_metrics$himed_median)
  )

turning_points <- 
  # for cold
  RR_t_hi_df %>% 
  filter(!ascending, RR_low > 1) %>% 
  slice_max(order_by = at, n=1, with_ties = F) %>% 
  ungroup() %>% 
  mutate(scenario = "cold",
         q = t_med_ecdf(at)) %>% 
  select(-ascending) %>% 
  # for hot
  add_row(
    RR_t_hi_df %>% 
      filter(ascending, RR_low > 1) %>% 
      slice_min(order_by = at, n=1, with_ties = F) %>% 
      ungroup() %>% 
      mutate(scenario = "hot",
             q = hi_med_ecdf(at)) %>% 
      select(-ascending)
  )

crossing_points <- 
  # for cold
  ## 1.25
  RR_t_hi_df %>% 
  filter(!ascending, RR_low > 1, RR > 1.25) %>% 
  slice_max(order_by = at, n=1, with_ties = F) %>% 
  ungroup() %>% 
  mutate(scenario = "cold",
         thresh = 1.25,
         q = case_when(
           metric == "t_med" ~ t_med_ecdf(at),
           metric == "hi_med" ~ hi_med_ecdf(at)
         )) %>% 
  select(-ascending) %>% 
  ## 1.5
  add_row(
    RR_t_hi_df %>% 
      filter(!ascending, RR_low > 1, RR > 1.5) %>% 
      slice_max(order_by = at, n=1, with_ties = F) %>% 
      ungroup() %>% 
      mutate(scenario = "cold",
             thresh = 1.5,
             q = case_when(
               metric == "t_med" ~ t_med_ecdf(at),
               metric == "hi_med" ~ hi_med_ecdf(at)
             )) %>% 
      select(-ascending)
  ) %>% 
  ## 2
  add_row(
    RR_t_hi_df %>% 
      filter(!ascending, RR_low > 1, RR > 2) %>% 
      slice_max(order_by = at, n=1, with_ties = F) %>% 
      ungroup() %>% 
      mutate(scenario = "cold",
             thresh = 2,
             q = case_when(
               metric == "t_med" ~ t_med_ecdf(at),
               metric == "hi_med" ~ hi_med_ecdf(at)
             )) %>% 
      select(-ascending)
  ) %>% 
  # for hot
  ## 1.25
  add_row(
    RR_t_hi_df %>% 
      filter(ascending, RR_low > 1, RR > 1.25) %>% 
      slice_min(order_by = at, n=1, with_ties = F) %>% 
      ungroup() %>% 
      mutate(scenario = "hot",
             thresh = 1.25,
             q = case_when(
               metric == "t_med" ~ t_med_ecdf(at),
               metric == "hi_med" ~ hi_med_ecdf(at)
             )) %>% 
      select(-ascending)
  ) %>% 
  add_row(
    RR_t_hi_df %>% 
      filter(ascending, RR_low > 1, RR > 1.5) %>% 
      slice_min(order_by = at, n=1, with_ties = F) %>% 
      ungroup() %>% 
      mutate(scenario = "hot",
             thresh = 1.5,
             q = case_when(
               metric == "t_med" ~ t_med_ecdf(at),
               metric == "hi_med" ~ hi_med_ecdf(at)
             )) %>% 
      select(-ascending)
  ) %>% 
  add_row(
    RR_t_hi_df %>% 
      filter(ascending, RR_low > 1, RR > 2) %>% 
      slice_min(order_by = at, n=1, with_ties = F) %>% 
      ungroup() %>% 
      mutate(scenario = "hot",
             thresh = 2,
             q = case_when(
               metric == "t_med" ~ t_med_ecdf(at),
               metric == "hi_med" ~ hi_med_ecdf(at)
             )) %>% 
      select(-ascending)
  )

crossing_points %>%
  left_join(cdc_groups %>% select(group_id, grupo), by=c("causa"="grupo")) %>% 
  mutate(causa = translate_cdc_groups(causa)) %>% 
  filter(scenario == "hot") %>% 
  mutate(point = paste0(round(at, 1), " (Q", round(100*q, 2), ")")) %>% 
  select(group_id, causa, grupo, metric, thresh, point) %>% 
  pivot_wider(values_from = point, names_from = metric) %>% 
  replace_na(list(t_med = "-", hi_med = "-")) %>% 
  mutate(point = paste0(t_med, " / ", hi_med)) %>% 
  select(-hi_med, -t_med) %>% 
  mutate(grupo = factor(grupo, levels = c("Jovem", "Idoso"))) %>% 
  arrange(grupo) %>% 
  pivot_wider(
    values_from = point,
    names_from = c(grupo, thresh)
  ) %>% 
  mutate(across(everything(), function(x) {ifelse(is.na(x), "- / -", x)}))  %>% 
  arrange(group_id) %>% 
  write_csv("results/crossing_points_hot.csv")

# Mean

crossing_points %>%
  left_join(cdc_groups %>% select(group_id, grupo), by=c("causa"="grupo")) %>% 
  mutate(causa = translate_cdc_groups(causa)) %>% 
  filter(scenario == "hot") %>% 
  summarise(q=mean(q), .by=c(grupo, metric, thresh)) %>% 
  mutate(at = case_when(
    metric == "t_med" ~ quantile(temp_df$t_med, probs = q),
    metric == "hi_med" ~ quantile(temp_df$hi_med, probs = q)
  ),
  point = paste0(round(at, 1), " (Q", round(100*q, 2), ")")) %>% 
  select(grupo, metric, thresh, point) %>% 
  pivot_wider(values_from = point, names_from = metric) %>% 
  mutate(point = paste0(t_med, " / ", hi_med)) %>% 
  select(-hi_med, -t_med) %>% 
  mutate(grupo = factor(grupo, levels = c("Jovem", "Idoso"))) %>% 
  arrange(grupo) %>% 
  pivot_wider(
    values_from = point,
    names_from = c(grupo, thresh)
  ) %>% 
  write_csv("results/crossing_points_hot_mean.csv")
