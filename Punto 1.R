######################################################################
#  Taller 2: Economía Urbana
#  Punto 1: Réplica del articulo de Leonardi y Moretti (2023)
#  Estudiantes: David Florez, Daniel Hernandez
#####################################################################

rm(list = ls()) #limpiamos el entorno
require("pacman") 

p_load(
 sf,  dplyr, ggplot2, scales, patchwork, tidyr )

######################
# RUTAS DE TRABAJO
######################

# Ruta principal
setwd("C:/Users/braya/OneDrive - Universidad de los andes/Escritorio/U/8vo semestre/Economía Urbana/Taller 2")


################################################################################
# 0. Carga de datos
################################################################################

# Cargamos los datos:
load("Data/Taller2_Ejercicio1.Rdata")



################################################################################
####### FIGURA 1 ###############################################################
################################################################################



# ===============================================================
# 1) Asegurar tipos consistentes
# ===============================================================
barrios    <- barrios    %>% mutate(ZONA180 = as.integer(ZONA180))
poblacion  <- poblacion  %>% mutate(zona180 = as.integer(zona180))
restaurants<- restaurants%>% mutate(zona180 = as.integer(zona180))

# ===============================================================
# 2) Conteo de restaurantes por barrio y año
# ===============================================================
rest_counts <- restaurants %>%
  summarise(
    n_rest_2004 = sum(!is.na(lat2004) & !is.na(long2004)),
    n_rest_2012 = sum(!is.na(lat2012) & !is.na(long2012)),
    .by = zona180
  )

# ===============================================================
# 3) Unir con población y calcular número per cápita (por 1000 hab)
# ===============================================================
data_milan <- barrios %>%
  left_join(poblacion,  by = c("ZONA180" = "zona180")) %>%
  left_join(rest_counts, by = c("ZONA180" = "zona180")) %>%
  mutate(
    n_rest_2004  = coalesce(n_rest_2004, 0),
    n_rest_2012  = coalesce(n_rest_2012, 0),
    rest_pc_2004 = n_rest_2004 * 1000 / day_pop,  # igual a gen prolat2004_r
    rest_pc_2012 = n_rest_2012 * 1000 / day_pop
  )

# ===============================================================
# 4) Diferencia respecto al promedio de ciudad (en restaurantes/1000 hab)
# ===============================================================
data_milan <- data_milan %>%
  mutate(
    rel_2004 = rest_pc_2004 - mean(rest_pc_2004, na.rm = TRUE),
    rel_2012 = rest_pc_2012 - mean(rest_pc_2012, na.rm = TRUE)
  )

# ===============================================================
# 5) Crecimiento logarítmico 2004–2012 (para el 3er mapa)
# ===============================================================
data_milan <- data_milan %>%
  mutate(
    growth_0412 = log(rest_pc_2012 + 1e-9) - log(rest_pc_2004 + 1e-9)
  )

# ===============================================================
# 6) Paletas de color y cortes según el paper
# ===============================================================
breaks_vals <- c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14)
labels_vals <- c("-4–-2", "-2–0", "0–2", "2–4", "4–6",
                 "6–8", "8–10", "10–12", "12–14")
paleta_milan <- c("#f5e6cf", "#f3d6b2", "#f5c38d", "#f5a666",
                  "#ef854a", "#e85d28", "#cc3b1c", "#9b1e0b", "#6e0000")

data_milan <- data_milan %>%
  mutate(
    cat_rel_2004 = cut(rel_2004, breaks = breaks_vals,
                       labels = labels_vals, include.lowest = TRUE),
    cat_rel_2012 = cut(rel_2012, breaks = breaks_vals,
                       labels = labels_vals, include.lowest = TRUE)
  )

# ===============================================================
# 7) Mapas 2004 y 2012 — con NA blancos y una sola leyenda
# ===============================================================
map_2004 <- ggplot(data_milan) +
  geom_sf(aes(fill = cat_rel_2004), color = "black", size = 0.1, na.fill = "white") +
  scale_fill_manual(
    values = paleta_milan, drop = FALSE,
    name = "",
    na.value = "white"
  ) +
  theme_void() +
  ggtitle("Número per cápita de restaurantes en 2004") +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

map_2012 <- ggplot(data_milan) +
  geom_sf(aes(fill = cat_rel_2012), color = "black", size = 0.1, na.fill = "white") +
  scale_fill_manual(
    values = paleta_milan, drop = FALSE,
    name = "",
    na.value = "white"
  ) +
  theme_void() +
  ggtitle("Número per cápita de restaurantes en 2012") +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# ===============================================================
# 8) Mapa de crecimiento logarítmico 2004–2012 (%)
# ===============================================================
breaks_growth <- c(-0.6, -0.3, -0.2, 0, 0.2, 0.3, 0.6)
labels_growth <- c("-0.6–-0.3", "-0.3–-0.2", "-0.2–0", "0–0.2", "0.2–0.3", "0.3–0.6")
paleta_growth <- c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

data_milan <- data_milan %>%
  mutate(cat_growth = cut(growth_0412, breaks = breaks_growth,
                          labels = labels_growth, include.lowest = TRUE))

map_growth <- ggplot(data_milan) +
  geom_sf(aes(fill = cat_growth), color = "black", size = 0.1, na.fill = "white") +
  scale_fill_manual(values = paleta_growth, drop = FALSE,
                    name = "",
                    na.value = "white") +
  theme_void() +
  ggtitle("Porcentaje de crecimiento en el número de restaurantes per cápita") +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.5, "cm")
  )

# ===============================================================
# 9) Disposición final tipo paper — solo la leyenda del 2012
# ===============================================================

# Quitamos la leyenda del primer mapa
map_2004 <- map_2004 + theme(legend.position = "none")

# Mantenemos la leyenda del segundo
map_2012 <- map_2012 + theme(legend.position = "right")

# Combinamos arriba (dos mapas naranjas con una sola leyenda)
top_row <- map_2004 + map_2012 + plot_layout(widths = c(1, 1))

# Combinamos con el mapa azul debajo
final_plot <- top_row / map_growth + plot_layout(heights = c(1, 1))

# Mostrar o guardar
final_plot
ggsave("results/figure1_replica.pdf", final_plot, width = 8, height = 7, dpi = 300)





################################################################################
####### FIGURA 2 ###############################################################
################################################################################

# ===============================================================
# 1) Preparar datos de precios
# ===============================================================
# Asegurarse de que las variables de precio no tengan NA
precios_2004 <- restaurants$prezzo2004[!is.na(restaurants$prezzo2004)]
precios_2012 <- restaurants$prezzo2012[!is.na(restaurants$prezzo2012)]

# ===============================================================
# 2) Estimar densidades no paramétricas
# ===============================================================

# --- Epanechnikov ---
dens_epa_2004 <- density(precios_2004, kernel = "epanechnikov")
dens_epa_2012 <- density(precios_2012, kernel = "epanechnikov")

# --- Gaussiano ---
dens_gau_2004 <- density(precios_2004, kernel = "gaussian")
dens_gau_2012 <- density(precios_2012, kernel = "gaussian")

# ===============================================================
# 3) Graficar las estimaciones
# ===============================================================

library(ggplot2)

# Convertir a data frame para ggplot
df_dens <- data.frame(
  x = c(dens_epa_2004$x, dens_epa_2012$x,
        dens_gau_2004$x, dens_gau_2012$x),
  y = c(dens_epa_2004$y, dens_epa_2012$y,
        dens_gau_2004$y, dens_gau_2012$y),
  Año = rep(c("2004", "2012", "2004", "2012"),
            each = length(dens_epa_2004$x)),
  Kernel = rep(c("Epanechnikov", "Epanechnikov", "Gaussiano", "Gaussiano"),
               each = length(dens_epa_2004$x))
)

# ===============================================================
# Gráfico tipo paper (monocromático y sobrio)
# ===============================================================

ggplot(df_dens, aes(x = x, y = y, color = Año, linetype = Año)) +
  geom_line(size = 0.9) +
  facet_wrap(~Kernel, scales = "free_y") +
  scale_color_manual(values = c("2004" = "#1b4965", "2012" = "#ca6702")) +
  labs(
    x = "Precio del restaurante",
    y = "Densidad estimada",
    color = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(
    color = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE)
  )

ggsave("results/Ejercicio_1_Punto_2_a.pdf", width = 9, height = 4, dpi = 300)




################################################################################
#### FIGURA 3 ##################################################################
################################################################################

# ===============================================================
# 1) Preparar datos
# ===============================================================
precios_2004 <- restaurants$prezzo2004[!is.na(restaurants$prezzo2004)]
precios_2012 <- restaurants$prezzo2012[!is.na(restaurants$prezzo2012)]

# ===============================================================
# 2) Calcular ancho de banda rule-of-thumb
# ===============================================================
bw_2004 <- bw.nrd0(precios_2004)
bw_2012 <- bw.nrd0(precios_2012)

bw_alt <- c(bw_2004 / 2, bw_2004, bw_2004 * 2)
names(bw_alt) <- c("h/2", "h", "2h")

# ===============================================================
# 3) Estimar densidades con kernel Epanechnikov
# ===============================================================
dens_all <- list()
for (name in names(bw_alt)) {
  dens_all[[name]] <- list(
    "2004" = density(precios_2004, kernel = "epanechnikov", bw = bw_alt[name]),
    "2012" = density(precios_2012, kernel = "epanechnikov", bw = bw_alt[name])
  )
}

# ===============================================================
# 4) Armar data frame
# ===============================================================
df_dens <- do.call(rbind, lapply(names(dens_all), function(name) {
  data.frame(
    x = c(dens_all[[name]]$`2004`$x, dens_all[[name]]$`2012`$x),
    y = c(dens_all[[name]]$`2004`$y, dens_all[[name]]$`2012`$y),
    Año = rep(c("2004", "2012"), each = length(dens_all[[name]]$`2004`$x)),
    Bandwidth = name
  )
}))

# ===============================================================
# 5) Gráfico profesional tipo paper
# ===============================================================
library(ggplot2)

ggplot(df_dens, aes(x = x, y = y, color = Año, linetype = Año)) +
  geom_line(size = 0.9) +
  facet_wrap(~Bandwidth, scales = "free_y", ncol = 1,
             labeller = as_labeller(c("h/2" = "Ancho de banda: h/2",
                                      "h" = "Ancho de banda: h (rule-of-thumb)",
                                      "2h" = "Ancho de banda: 2h"))) +
  scale_color_manual(values = c("2004" = "#1b4965", "2012" = "#ca6702")) +
  labs(
    x = "Precio del restaurante",
    y = "Densidad estimada"
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
    plot.margin = margin(5, 5, 5, 5)
  )
ggsave("results/Ejercicio_1_Punto_2_.pdf", width = 8, height = 8, dpi = 300)




################################################################################
### FIGURA 4 ################################################################
################################################################################


# ===============================================================
# 1) Filtrar los 5 barrios con mayor crecimiento per cápita
# ===============================================================
top5 <- data_milan %>%
  st_drop_geometry() %>%
  arrange(desc(growth_0412)) %>%
  slice_head(n = 5) %>%
  pull(ZONA180)

# Subconjunto espacial de esos barrios
barrios_top5 <- barrios %>% filter(ZONA180 %in% top5)

# Puntos de restaurantes por año en esos barrios
rest_2004 <- restaurants %>%
  filter(zona180 %in% top5 & !is.na(lat2004) & !is.na(long2004)) %>%
  st_as_sf(coords = c("long2004", "lat2004"), crs = 4326) %>%
  st_transform(crs = 32632) # sistema métrico (UTM zona norte Italia)

rest_2012 <- restaurants %>%
  filter(zona180 %in% top5 & !is.na(lat2012) & !is.na(long2012)) %>%
  st_as_sf(coords = c("long2012", "lat2012"), crs = 4326) %>%
  st_transform(crs = 32632)

# ===============================================================
# 2) Calcular distancias bilaterales y densidad observada (0–1 km)
# ===============================================================
get_dist_density <- function(points_sf) {
  d <- st_distance(points_sf)
  d[lower.tri(d, diag = TRUE)] <- NA
  d <- as.numeric(d[!is.na(d)]) / 1000  # convertir a km
  density(d, from = 0, to = 1, n = 100, bw = "nrd0", kernel = "gaussian")
}

dens_obs_2004 <- get_dist_density(rest_2004)
dens_obs_2012 <- get_dist_density(rest_2012)

# ===============================================================
# 3) Simulaciones aleatorias (contrafactual)
# ===============================================================
set.seed(1205)
B <- 1000
boot_dens_2004 <- matrix(NA, nrow = length(dens_obs_2004$x), ncol = B)
boot_dens_2012 <- matrix(NA, nrow = length(dens_obs_2012$x), ncol = B)

for (i in 1:B) {
  samp_2004 <- st_sample(barrios_top5, size = nrow(rest_2004), type = "random")
  samp_2012 <- st_sample(barrios_top5, size = nrow(rest_2012), type = "random")
  boot_dens_2004[, i] <- get_dist_density(samp_2004)$y
  boot_dens_2012[, i] <- get_dist_density(samp_2012)$y
}

# ===============================================================
# 4) Calcular percentiles 5% y 95% para el intervalo de confianza
# ===============================================================
q05_2004 <- apply(boot_dens_2004, 1, quantile, probs = 0.05)
q95_2004 <- apply(boot_dens_2004, 1, quantile, probs = 0.95)
q05_2012 <- apply(boot_dens_2012, 1, quantile, probs = 0.05)
q95_2012 <- apply(boot_dens_2012, 1, quantile, probs = 0.95)

# ===============================================================
# 5) Graficar (línea observada vs. intervalos simulados)
# ===============================================================
plot_dov <- function(x, obs, q05, q95, year) {
  df <- data.frame(dist = x, obs = obs, q05 = q05, q95 = q95)
  ggplot(df, aes(x = dist)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "grey80", alpha = 0.5) +
    geom_line(aes(y = obs), color = ifelse(year == 2004, "#1b4965", "#ca6702"), size = 1) +
    labs(
      title = paste0( year),
      x = "Distancia entre restaurantes (km)",
      y = "Densidad de pares"
    ) +
    theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA)
    )
}

p2004 <- plot_dov(dens_obs_2004$x, dens_obs_2004$y, q05_2004, q95_2004, 2004)
p2012 <- plot_dov(dens_obs_2012$x, dens_obs_2012$y, q05_2012, q95_2012, 2012)

# Mostrar juntos
p2004 + p2012

ggsave("results/Ejercicio_1_Punto_2_c.pdf", width = 9, height = 4, dpi = 300)





