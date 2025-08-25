#### Outliers no espaço ambiental ####

library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(flexsdm) #Para identificar outliers no espaço ambiental
library(tidyr) #Manipulação de data.frames
library(ggplot2)
library(ggpubr)

# Importar variaveis ambientais
variaveis <- rast("Variaveis/Variaveis_neotropicos.tif")
plot(variaveis$bio_1)

#Selecionar apebas variaveis de interesse
var_to_keep <- readRDS("Variaveis/Variaveis_para_manter.rds")
var_to_keep
v <- variaveis[[var_to_keep]]
names(v)

# Importar registros de ocorrência
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros
occ <- fread(file.path(sp_dir, "3-Ocorrencias_CoordinateCleaner.gz"))

# Espacializar e plotar
pts <- vect(occ, geom = c(x = "decimalLongitude", y = "decimalLatitude"),
            crs = "epsg:4326")

# Extrair as variáveis para os pontos
pts_var <- terra::extract(v, pts,
                          xy = TRUE, #Retornar coordenadas na tabela
                          na.rm = TRUE) #Remover Nas

# Converter de wide para long format
d <- pivot_longer(data = pts_var, #Tabela para converter
                  cols = bio_6:slope, #Colunar para transformar em valores
                  names_to = "variaveis", #Nome da coluna que vai abrigar os nomes das variaveis
                  values_to = "valores") #Nome da coluna que vai abrigar os valores das variaveis

# Boxplot para identificar outliers
# IQR (Intervalo interquartil): Q3 - Q1
# Outliers: Q3 + 1.5 x IQR | Q1 - 1.5 * IQR
g_box <- ggplot() + #Chamar ggpot
  geom_boxplot(data = d, #Boxplot
               aes(x = variaveis, y = valores), #Eixo X e Y do boxplot
               outlier.colour = "firebrick") + #Cor dos outliers
  facet_wrap(.~variaveis, #Determinar um plot separado por variavel
             scale = "free", #Range diferente para cada variável
             nrow =2) + #Numero de linhas no layout to plot
  ggpubr::theme_pubclean() #Tema do plot
g_box

# O pacote flexsdm oferece várias opções para identificar outliers
?env_outliers

# Boxplot

# Reverse Jackknife
# Referencia: https://assets.ctfassets.net/uo17ejk9rkwj/46SfGRfOesU0IagMMAOIkk/1c03ea3e21fcd9025cc800d786890e72/Principles_20and_20Methods_20of_20Data_20Cleaning_20-_20ENGLISH.pdf
# Como funciona:
  # Definie uma distância máxima da média
  # Distancia máxima = (0.95 * sqrt(numero_de_variaveis) + 0.2
  # Calcula distancia (normalizada) da média de cada ponto
  # Valores com distancia acima da distancia maxima são identificados como outliers

# Random Forest outliers
# Reference: https://doi.org/10.1111/jbi.13122
# Utiliza algoritmo Random Forest para "agrupar" pontos de acordo com proximidade
# Considerando todas as variáveis ambientais
# Algoritmo também calcula um "score" de outlier, ou seja, quão longe está um ponto de todos os outros
# Seleciona valor de score de outlier que é maior do que 95% dos outros scores
# Registros acima desse valor indicam que ponto está isolado, sendo classificado como outlier

# Vamos usar a função env_outliers para detectar outliers nos pontos de ocorrência
# Na tabela de ocorrência, precisamos criar uma coluna chamada "pr_ab" indicando os pontos de presença (1) e ausência (0)
# Nesse caso, todos os valores são de presença (valor 1)
occ <- occ %>%
  mutate(pr_ab = 1) #Cria coluna pr_ab e preenche com 1

# Quanto mais variáveis considerarmos, maiores as chances dos pontos serem classificados como outliers
# Corremos o risco do ponto ser um outlier de uma variável que nem é importante para definir a distribuição da espécie
# O ideal é usar poucas variáveis, e variáveis que temos certeza que afetam a espécie
# Aqui, vamos usar bio_6 (minima temperatura do mês mais frio)
# E bio_16 (precipitação do mês mais úmido)
variaveis_teste_outlier <- v[[c("bio_6", "bio_16")]]

# Detectar outliers
outliers_env <- env_outliers(data = occ, #Tabela com ocorrências
                             pr_ab = "pr_ab", #Nome da coluna com presenca (1) e ausencia (0)
                             x = "decimalLongitude", #Coluna com longitude
                             y = "decimalLatitude", #Coluna com latitude
                             id = "ID", #Nome da coluna com ID do registro
                             env_layer = variaveis_teste_outlier #Variaveis raster
                             )
# Função cria colunas informando se registro é um outlier (1) ou não (0)
outliers_env %>% select(starts_with(".")) %>% View()
# .out_bxpt: outliers identificados pelo método de boxplot
# .out_jack: outliers identificados pelo método de jackknife reverso
# out_svm: método de SVM (não aplicado a dados de apenas presença)
# .out_rf: método de Random Forest (não aplicado a dados de apenas presença)
# .out_rfout: método de outliers de random forest
# .out_sum: quantos métodos identificaram o registro como outlier?

# Vamos ver esses registros no espaço geográfico
# Espacializar pontos
pts_out <- vect(outliers_env,
                geom = c(x = "decimalLongitude", y = "decimalLatitude"),
                crs = "epsg:4326")
mapview(pts_out, zcol = ".out_sum",
        col.regions = c("forestgreen", "yellow","firebrick"),
        burst = TRUE)

# Vamos ver esses pontos no espaço ambiental
# Adicionar coluna de outliers ao objeto pts_var
pts_var_out <- bind_cols(pts_var,
                     outliers_env[, c(".out_sum")])
g_out_env <- ggplot() + #Chamar ggplot
  geom_point(data = pts_var_out, #Plot de pontos
             aes(x = bio_6, y = bio_16, #Eixo X e Y
                 colour = as.factor(.out_sum)), #Coluna para usar para colorir
             cex = 3, alpha = 0.5) + #Tamanho e transparencia dos pontos
  scale_colour_manual(values = c("forestgreen", "gold2","firebrick"), #Cores
                      name = "Sum of detected outliers") + #Titulo na legenda
  xlab("Min Temperature of Coldest Month") + #Titulo do eixo X
  ylab("Precipitation of Wettest Quarter") + #Titulo do eixo X
  ggpubr::theme_pubclean() #Tema do plot
g_out_env

# Remover ou não os outliers?
# Em espécies com muitos registros, outliers podem não fazer muita diferença
# Mais impacto em espécies com poucos registros
# Também depende do objetivo do estudo:
  # Impacto de mudanças climáticas: Outliers podem representar populações que conseguem suportar climas mais quente ou mais secos
      # Nesse caso, talvez seja uma boa ideia manter alguns outliers
  # Procura por novas populações: outliers podem representar condições ambientais onde espécie é rara, mais difícil de encontrar
      # Nesse caso, é interessante remover os outliers

# Na dúvida, faça suas análises com e sem os outliers para ver a diferença

# Aqui, vamos remover os outliers
occ_sem_out <- outliers_env %>% filter(.out_sum == 0)
# Salvar
fwrite(occ_sem_out,
       "Ocorrencias/Araucaria angustifolia/4-Ocorrencias_sem_outliers.gz")

# Vamos salvar os IDs dos pontos removidos
arquivo_original <- "Ocorrencias/Araucaria angustifolia/3-Ocorrencias_CoordinateCleaner.gz"
id_outliers <- setdiff(occ$ID, occ_sem_out$ID)
removidos <- list("arquivo_original" = arquivo_original,
                  outliers_ambientais = id_outliers)
removidos
saveRDS(removidos,
        file.path(sp_dir, "Removidos", "4-Outliers.rds"))
