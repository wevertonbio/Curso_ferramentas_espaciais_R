#### Dualidade de Hucthinson ####
# Representando pontos no espaço geográfico e no espaço ambiental

# Artigos legais sobre o assunto:
browseURL("https://www.pnas.org/doi/pdf/10.1073/pnas.0901650106")
browseURL("https://www.nature.com/articles/s41586-023-06577-5.pdf")
# Climatic conditions that cover more extensive land areas (and times) are believed to support more individuals, leading to larger populations. Larger populations, in turn, are associated with increased rates of speciation and reduced rates of extinction

# Carregar pacotes
library(terra)
library(mapview)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(data.table)
library(tidyterra)
library(patchwork)
library(ggpubr)

#### Ecorregioes: espaço geográfico vs espaço ambiental ####

# Importar shapefile de ecorregioes
eco <- vect("Data/Ecoregions2017.gpkg")
plot(eco)

# Cortar para o Parana
pr <- vect("Data/Parana.gpkg")

pr_eco <- crop(eco, pr)
plot(pr_eco)

# Importar variaveis ambientais
variaveis <- rast("Variaveis/Variaveis_neotropicos.tif")
plot(variaveis$bio_1)

# Cortar para Parana
v_pr <- crop(variaveis, pr, mask = TRUE)
#Selecionar apebas variaveis de interesse
var_to_keep <- readRDS("Variaveis/Variaveis_para_manter.rds")
v <- v_pr[[var_to_keep]]

# Como ficam as ecorregiões no espaço geográfico?
plot(v$bio_6)
plot(pr_eco, add = TRUE)

# Como ficam as ecorregiões no espaço ambiental?
# Importar tabela de variáveis extraídas para cada ecorregião
eco <- fread("Data/My_data/Ecoregions_variables.gzip")
View(eco)

# Como exemplo, vamos usar bio_6 (temp. mes mais frio) e bio_12 (precipitação anual)

# Definir cores manualmente
unique(eco$ECO_NAME)

cores <- c("#D55E00", "#009E73", "#F0E442", "#0072B2", "#CC79A7")
cores <- setNames(cores, nm = unique(eco$ECO_NAME))

g_env <- ggplot() +
  geom_point(data = eco, aes(x = bio_6, y = bio_12,
                             fill = ECO_NAME),
             pch = 21, alpha = 0.6, cex = 2) +
  scale_fill_manual(values = cores, name = "Ecoregion") +
  xlab("Minimum temp. coldest quarter") +
  ylab("Annual Precipitation") +
  ggtitle("Environmental space") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) #+
  #guides(fill = guide_legend(nrow = 3))
g_env

# Vamos plotar o mapa
eco_map <- ggplot() +
  geom_spatvector(data = pr_eco, aes(fill = ECO_NAME)) +
  scale_fill_manual(values = cores, name = "Ecoregion") +
  ggtitle("Geographic space") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))
eco_map

# Vamos unir os plots
geo_env <- (eco_map + g_env) & theme(legend.position = "none")

# Obter legenda
legenda <- ggpubr::get_legend(eco_map)
geo_env2 <- geo_env / legenda +
  plot_layout(heights = c(1, 0.15))
geo_env2

# Salvar
ggsave("images/Geo_env_Ecorregioes.png",
       geo_env2,
       width = 9, height = 4, dpi = 600, scale = 1.3)

#### Registros de ocorrência - Espaço geográfico vs Espaço ambiental ####
# Assim como vimos quais partes do espaço ambiental cada ecorregião ocupa, podemos ver quais partes do espaço ambiental disponível uma espécie ocupa

# Importar registros de ocorrência
occ <- fread("Ocorrencias/Araucaria angustifolia/Ocorrencias_cleaned.gz")

# Espacializar e plotar
pts <- vect(occ, geom = c(x = "decimalLongitude", y = "decimalLatitude"),
            crs = "epsg:4326")
# Extrair as variáveis para os pontos
pts_var <- terra::extract(variaveis, pts, xy = TRUE, na.rm = TRUE)

# Quando falamos do "nicho" ocupado por uma espécie, é recomendável definir uma "área acessivel" para espécie
# Por exemplo, algumas regiões dos Andes podem ter condições de temperatura e precipitação semelhantes a Floresta de Araucarias (frio e umido)
# Porém, a Araucária não consegue se dispersar naturalmente até lá.

# Vamos definir a área acessível para nossa espécie como um minimo poligono convexo com um buffer de 100km
area_acessivel <- hull(pts) %>%
  buffer(width = 100 * 1000)
plot(area_acessivel)
plot(pts, add = T)

# Cortar variaveis para area acessivel
v_acessivel <- crop(variaveis, area_acessivel, mask = TRUE)
plot(v_acessivel$bio_1)

# Converter as variaveis cortadas para data.frame
df <- as.data.frame(v_acessivel, na.rm = TRUE, xy = TRUE)
# Identificar que são pontos de todo neotropico
df <- df %>%
  mutate(Points = "Acessible area", .before = 1)
head(df)

# Identificar que são pontos de ocorrência
pts_var <- pts_var %>%
  mutate(Points = "Occurrences", .before = 1)
head(df)

# Vamos unir esses dados
d <- bind_rows(pts_var, df)

# Vamos sumarizar as variaveis em dois eixos de PCA
nomes_variaveis <- names(variaveis)
nomes_variaveis
# Remover tipo de solo
variaveis_continuas <- setdiff(nomes_variaveis, "soilType")
variaveis_continuas
# Fazer PCA
pca <- prcomp(d[, variaveis_continuas], scale = TRUE, center = TRUE)
# Predizer PCA para dataframe
pca_df <- predict(pca, d)

# Unir dados
d_pca <- d %>% select(Points) %>% bind_cols(pca_df)
head(d_pca)

#Separar dataframe após fazer pca
dados_pontos <- d_pca %>% filter(Points == "Occurrences")
dados_area_acessivel <- d_pca %>% filter(Points == "Acessible area")

# Plotar com ggplot
occ_env <- ggplot() +
  geom_point(data = dados_area_acessivel,
             aes(x = PC1, y = PC2, colour = "Acessible area"),
             cex = 3) +
  geom_point(data = dados_pontos,
             aes(x = PC1, y = PC2, colour = "Occurrences"),
             cex = 3, alpha = 0.5) +
  scale_colour_manual(values = c("gray", "forestgreen"),
                      name = "Point") +
  ggtitle("Environmental space") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))
occ_env

# Obter mapa da America do Sul
sa <- vect("Data/South_America.gpkg")
plot(sa)

# Cortar area acessivel para continente (usando sa)
area_acessivel <- crop(area_acessivel, sa)
plot(area_acessivel)

# Obter limites da area acessivel
limites <- ext(area_acessivel)

# Plotar no espaço geográfico
occ_geo <- ggplot() +
  geom_spatvector(data = sa, aes(fill = "Inaccessible area")) +
  geom_spatvector(data = area_acessivel, aes(fill = "Accessible area"),
                  alpha = 0.7) +
  geom_spatvector(data = pts, aes(fill = "Occurrences"), col = "forestgreen",
                  size = 1,
                  pch = 21) +
  scale_fill_manual(values = c("gray", "white", "forestgreen"), name = NULL) +
  coord_sf(xlim = c(limites[1] - 0.5, limites[2] + 0.5),
           ylim = c(limites[3] - 0.5, limites[4] + 0.5),
           expand = T) +
  ggtitle("Geographical space") +
  theme(legend.position = "right",
        panel.background = element_rect(fill = 'aliceblue', colour = NA),
        panel.border = element_rect(colour = "black", linewidth = 2, fill = NA),
        plot.title = element_text(hjust = 0.5, face = "bold"))
occ_geo

# Vamos unir os plots
occ_geo_env <- (occ_geo + occ_env) & theme(legend.position = "none")
occ_geo_env

# Obter legenda
legenda <- ggpubr::get_legend(occ_geo + theme(legend.position = "bottom"))
occ_geo_env2 <- occ_geo_env / legenda +
  plot_layout(heights = c(1, 0.15))
occ_geo_env2

# Salvar
ggsave("images/Geo_env_Ocorrencias.png",
       occ_geo_env2,
       width = 9, height = 4, dpi = 600, scale = 1.3)

# Espécies com distribuição disjunta (ex: Amazonia e Mata Atlântica)
# Será que elas ocupam o mesmo espaço ambiental?

# Espécies invasoras (ex: nativa da África e invasora no Brasil):
# Será que populações nativas e invasoras ocupam o mesmo nicho?

