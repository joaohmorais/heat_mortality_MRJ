library(tidyverse)
library(sf)
library(OpenStreetMap)

# base tiles
osm_map <- openmap(c(-22.74, -43.84), c(-23.1, -43.05), zoom=12, type = c("esri-imagery"))
osm_crs <- osm_map$tiles[[1]]$projection
# Rio contour

contorno_rio <- st_read("data/contorno_rio.gpkg") %>% 
  st_transform(osm_crs)

estacoes_df <- read_csv("data/estacoes_ativas.csv", show_col_types = F) %>% 
  mutate(row=seq_len(nrow(.)))

estacoes_sf <- estacoes_df %>% 
  st_as_sf(coords=c("longitude", "latitude")) %>% 
  `st_crs<-`(4674)


g_map <- 
  osm_map %>% 
  autoplot.OpenStreetMap() + 
  geom_sf(data=contorno_rio, 
          aes(geometry=geom), 
          inherit.aes = F,
          fill = "transparent", linewidth = 0.6, color = "#ffffc4") + 
  geom_sf(data=estacoes_sf, aes(geometry=geometry, fill = fonte), inherit.aes = F, size = 6.4, shape=23, color="white", linewidth=2, alpha=0.8) +
  geom_sf_text(data=estacoes_sf %>% 
                 mutate(row = case_when(
                   row == 8 ~ "8/4", # overlapping stations
                   row == 4 ~ NA_character_,
                   TRUE ~ as.character(row)
                 )), aes(geometry=geometry, label = row), inherit.aes = F ,size = 2.6, color="white", fontface = "bold",
               nudge_x = c(rep(0,7), 10, rep(0, 8))) +
  theme_void() + 
  labs(fill = "Source", 
       caption = paste0(
         c("", "", "", "", "", "",
           "", "\n", "", "", "", "",
           "\n", "", "", ""), estacoes_df$row, ". ", estacoes_df$estacao_nome
       ) %>% paste(collapse = "; ")) +
  theme(legend.position = "top",
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.caption = element_text(size=6))

ggsave(g_map, 
       filename = "img/img_extra_station_map.jpg",
       width = 7.5,
       height = 4,
       units = "in",
       dpi = 300
       )
       