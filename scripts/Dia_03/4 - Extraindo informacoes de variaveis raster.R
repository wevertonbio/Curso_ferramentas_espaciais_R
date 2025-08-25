# Carregar pacotes
library(terra)
library(mapview)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(data.table)

#### Extrair variáveis para polígonos ####

# Importar shapefile de ecorregioes
eco <- vect("Data/Ecoregions2017.gpkg")
plot(eco)

# Cortar para o Parana
pr <- vect("Data/Parana.gpkg")

pr_eco <- crop(eco, pr)
plot(pr_eco)
mapview(pr_eco)

# Importar variaveis ambientais
variaveis <- rast("Variaveis/Variaveis_neotropicos.tif")
plot(variaveis$bio_1)

# Vamos trabalhar apenas com as variaveis selecionadas com baixa correlacao
var_to_keep <- readRDS("Variaveis/Variaveis_para_manter.rds")
var_to_keep
#Fazer subset de variaveis
v <- variaveis[[var_to_keep]]
plot(v)
# Podemos usar a funcao zonal para extrair estatisticas de cada "zona" de um poligono
# Por exemo, média, valor máximo ou mínimo, moda, etc
eco_stats <- zonal(v, pr_eco)
eco_stats
# Adicionar informacao de ecorregiao
eco_stats <- eco_stats %>%
  mutate(Ecoregion = pr_eco$ECO_NAME, .before = 1)
View(eco_stats)

# Médias são interessantes para ver diferenças, mas elas podem mentir...
# Vamos ver essas diferenças em um boxplot
# Para isso, precisamos extrair os pixels para cada ecorregicao
eco_ext <- terra::extract(v, pr_eco, #Forçar usar extract de terra
                   xy = TRUE) # Para retornar coordenadas
head(eco_ext)
# Cria uma coluna ID que corresponde à ordem das geometrias
pr_eco_df <- as.data.frame(pr_eco) %>%
  mutate(ID = 1:n())
pr_eco_df
# Unir dados
eco_ext <- right_join(pr_eco_df, eco_ext, by = "ID")

# Agora, podemos plotar os boxplots...
boxplot(eco_ext$bio_6 ~ eco_ext$ECO_NAME)
# Vamos melhorar isso com ggplot
# Vamos usar uma paleta de cores do Museu de Arte de Nova York...
browseURL("https://github.com/BlakeRMills/MetBrewer")
library(MetBrewer)
minhas_cores <- met.brewer(name = "Lakota")
minhas_cores
show_col(minhas_cores)


# Vamos converter os dados de wider para longer format
d <- pivot_longer(eco_ext,
                  cols = bio_6:slope, # Quais variáveis vão aparecer na coluna Variables?
                  names_to = "Variables", #Coluna que vai armazenar os colnames
                  values_to = "Value") #Nome da coluna que vai armazenar os valores
# Plot com ggpplot
g_box <- ggplot(d) +
  geom_boxplot(aes(x = ECO_NAME, y = Value, fill = ECO_NAME)) +
  scale_fill_manual(values = minhas_cores,
                    name = "Ecoregion") +
  ggpubr::theme_pubclean() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  facet_wrap(. ~ Variables,scales = "free", nrow = 2)
g_box
# Qual a ecoregião mais quente? E a mais fria?
# Qual a ecoregião com lugares mais altos? E com maiores declividades?

# Vamos salvar o data.frame com as variáveis extraídas para cada região para usar depois
dir.create("Data/My_data")
fwrite(eco_ext, "Data/My_data/Ecoregions_variables.gzip")

#### Extrair variáveis para pontos de ocorrência ####

# Importar registros de ocorrência
occ <- fread("Ocorrencias/Araucaria angustifolia/3-Ocorrencias_CoordinateCleaner.gz")

# Espacializar e plotar
pts <- vect(occ, geom = c(x = "decimalLongitude", y = "decimalLatitude"),
            crs = "epsg:4326")
mapview(eco) + mapview(pts)

# Antes de extrair informações das variáveis para os pontos, vamos ver quais ecoregiões tem mais registros da espécie
occ_eco <- terra::extract(eco, pts) # Demora um tempo...
head(occ_eco)
# Quantos pontos em cada ecoregiao?
occ_eco %>% count(ECO_NAME) %>% arrange(desc(n))

# Outra opção
table(occ_eco$ECO_NAME) %>% sort(decreasing = TRUE)

# Agora, vamos extrair as variáveis para os pontos
pts_var <- terra::extract(v, pts, xy = TRUE)

# Podemos extrair esses valores, de modo que eles sejam adicionados aos objeto espacial dos pontos
names(pts) #Ver informações associadas aos pontos
pts_var2 <- terra::extract(v, pts,
                           bind = TRUE) #Adicionar valores ao spatvector
class(pts_var2)
names(pts_var2)

#Vamos plotar e colorir os pontos por temperatura
mapview(pts_var2, zcol = "bio_6",
        col.regions = rev(pals::brewer.spectral(10)),
        layer.name = "Minimum temp. coldest quarter")

# Ou por elevação
# Por algum motivo, elevation não extraiu de forma correta
# Vamos adicionar os valores manualmente
pts_var2$elevation <- pts_var$elevation
mapview(pts_var2, zcol = "elevation",
        col.regions = rev(pals::brewer.spectral(10)),
        layer.name = "Elevation")

#Ver médias das variáveis nas ocorrências
pts_var %>% select(bio_6:slope) %>% colMeans()


# Também podemos fazer um boxplot com os valores das variáveis nos locais de ocorrência da espécie
# Tranformar tabela de wider para longer
pts_longer <- pivot_longer(pts_var,
                           cols = bio_6:slope, # Quais variáveis vão aparecer na coluna Variables?
                           names_to = "Variables", #Coluna que vai armazenar os colnames
                           values_to = "Value") #Nome da coluna que vai armazenar os


# Plot com ggpplot
g_box_occ <- ggplot(pts_longer) +
  geom_boxplot(aes(y = Value), fill = "forestgreen") +
  ggpubr::theme_pubclean() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  facet_wrap(. ~ Variables,scales = "free", nrow = 2)
g_box_occ

# Ou um histograma
g_hist <- ggplot(pts_longer) +
  geom_histogram(aes(x = Value), fill = "forestgreen", col = "black") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom") +
  facet_wrap(. ~ Variables,scales = "free", nrow = 2)
g_hist

# Porém, existe uma maneira ainda melhor de visualizar a distribuição desses dados
# Essa maneira tem a ver com a dualidade de Hutcinson... (próximo script)
