#### Modelos de Nicho Ecológico com kuenm2 ####
# kuenm = Kansas University Ecological Niche Models

# kuenm2 é uma atualização do kuenm:
browseURL("https://peerj.com/articles/6281.pdf")

# Site do pacote
browseURL("https://marlonecobos.github.io/kuenm2/")


# kuenm2 se baseia na ideia de modelos baseados em APENAS-presença (presence-only)
# Ajuste e validação dos modelos se baseia na ideia de que a única informação confiável que temos são os pontos de ocorrência
# Regiões sem registros de ocorrência não são tradados como ausências ou pseudo-ausências
# São tratados como background: pontos que nos informam as condições gerais dos ambientes que a espécie colonizou ou tentou colonizar
# Ex: Imagine uma região com muitas regiões muito frias e algumas poucas regiões mais quentes
# Se a maior parte dos registros de ocorrência está em regiões mais quentes (que são mais "raras" no background), isso é um forte indício que a espécie prefere regiões mais quentes
# Agora, se a maior parte dos registros está em regiões mais frias (que são mais "comuns" no backgroud), isso não necessariamente indica que a espécie prefere regiões mais frias. Ela pode estar lá simplesmente porque é uma região no espaço ambiental mais comum e, ao acaso, é mais provável que ela ocorra lá.

#' Ideia principal do kuenm:
#' Para cada espécie, cria diversos modelos candidatos.
#' Cada modelo candidato é uma combinação única de:
#'   - Variáveis ambientais
#'   - Feature (resposta): linear, quadrático, produto, theshold e hinge
#'   - Regularization multiplier: quanto menor o valor, mais complexo e ajstado
#'                                é o modelo
#'
#' Dentre os modelos candidatos, são selecionados os melhores modelos baseado em:
#'  - Valor de pROC: seleciona modelos com valores de pROC significativamente
#'    maiores que de um modelo nulo.
#'  - Taxa de erro: seleciona modelos com taxa de omissão menor que a taxa de erro
#'    especificada.
#'  - AIC: dos modelos que sobraram, é calculado o valor de Delta AIC e mantido os
#'    modelos com Delta AIC menor que 2 (ou outro valor especificado): modelo mais
#'    simples
#'
#' O modelo candidato que passa por todos esses filtros é considerado o melhor
#' modelo.
#' Se mais de um modelo candidato for selecionado, é possível obter um
#' consenso/ensemble dos modelos: média ou mediana.


#' Novidades do kuenm2:
#' - Usa maxnet ao invés de maxent (mais rápido e menos arquivos gerados).
#' - Também ajusta modelos com GLM (maior confiabilidade nos pontos de ocorrência)
#' - Ajuste de modelos em paralelo (mais rápido)
#' - Possibilidade de testar e remover modelos com curvas côncavas (maior
#' adequabilidade nos extremos)
#' Preparação dos dados e projeções muito mais fácil e rápida.
#' Várias opções de explorar os dados antes de calibrar os modelos.
#' Curvas de resposta e importância de variáveis
#' Análise de consensos entre diferentes GCMs: quantos GCMs predizem ganho ou
#' perda de área adequada?
#' Análise de risco de extrapolação de projeções.
#' Possibilidade de transformar variáveis brutas em PCA-variáveis de duas
#' maneiras: externamente ou internamente.


# Carregar pacotes
library(kuenm2)
library(terra)
library(data.table)
library(dplyr)
library(mapview)

#### Importar variáveis ####
variaveis <- rast("Variaveis/Variaveis_neotropicos.tif")

# Excluir variáveis 08, 09, 18 e 19 por apresentarem variações bruscas
# Essas variações não são naturais, mas artefatos estatísticos pela maneira como
# são calculadas
# https://onlinelibrary.wiley.com/doi/abs/10.1111/aec.13234
# Identificar posição das variáveis
id_remove <- which(names(variaveis) %in% c("bio_8", "bio_9", "bio_18", "bio_19"))
variaveis <- variaveis[[-id_remove]] #Remover variáveis
names(variaveis) #Ver variáveis que sobraram

#Se for selecionar variaveis, siga esses passos
# Importar variáveis selecionadas por correlação
var_to_keep <- readRDS("Variaveis/Variaveis_para_manter.rds")
v <- variaveis[[var_to_keep]] #Selecionar variáveis

#### Araucaria angustifolia ####

#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia" #Definir nome da espécie
sp_dir <- file.path("Ocorrencias/", sp) #Definir diretorio com ocorrencias da espécie
sp_dir

#Criar diretório "Models" para salvar modelos
sp_model <- file.path("Models/kuenm/", sp)
dir.create(sp_model, recursive = TRUE)

# Importar registros
occ <- fread(file.path("Ocorrencias/", sp,
                       "5-Ocorrencias_baixa_autocorrelacao_espacial.gz"),
             data.table = FALSE)

# Definir área acessível
# Área acessível NÃO É SUA ÁREA DE ESTUDO (a não ser que a espécie seja endemica da sua área de estudo)
# Área acessível não é um continente (a não ser que sua espécie tenha ampla distribuição no continente)
# Área acessível deve representar as áreas que a espécie colonizou ou tentou colonizar num período relevante de tempo (ex: ultimos 130mil anos)

# Artigos importantes sobre isso:
browseURL("https://www.sciencedirect.com/science/article/pii/S0304380011000780")
browseURL("https://onlinelibrary.wiley.com/doi/pdf/10.1111/geb.12678")
browseURL("https://escholarship.org/content/qt8hq04438/qt8hq04438.pdf")
browseURL("https://github.com/wevertonbio/grinnell")

# Aqui, vamos definir a área acessível (M) como um minimo polígono convexo com
# buffer de 500km
#Espacializar pontos
pts <- vect(occ, geom = c(x = "x", #Converte pontos para spatvector
                          y = "y"), crs = "+init=epsg:4326")
area_acessivel <- hull(pts) %>% #Criar minimo poligono convexo
  buffer(width = 500 * 1000) #Adicionar buffer de 100km
# Ver mapa
mapview(area_acessivel) +
  mapview(pts)

#### Preparar dados ####
?prepare_data #Ajuda da função

#Quantos pixels eu tenho na área acessível
v_m <- crop(v, area_acessivel, mask = TRUE) #Cortar variáveis para o M da espécie
global(v_m$bio_6, fun="notNA") #Ver quantidade de pixels que não são NAs
# Geralmente, um background definido como 10% a 20% dos pontos é suficiente
global(v_m$bio_6, fun="notNA") * 0.1

sp_swd <- prepare_data(algorithm = "maxnet", #Maxnet (maxent) ou GLM
                       occ = occ, #Tabela com ocorrências
                       species = sp, #Nome da espécie (opcional)
                       x = "x", y = "y", #Colunas em occ com longitude (x) e latitude (y)
                       raster_variables = v, #Variáveis raster
                       mask = area_acessivel, #Area acessivel para cortar raster (opcional)
                       # categorical_variables = "soilType", #Alguma variável categorica?
                       # do_pca = TRUE, #Fazer PCA das variáveis?
                       n_background = 5000, #Numero de pontos de background
                       partition_method = "kfolds", #Método de partição (treino e teste)
                       n_partitions = 4, #Numero de partições
                       min_number = 4, #Numero minimo de variáveis em cada modelo candidato
                       features = c("l", "lq", "lqp"), #Tipo de resposta
                       r_multiplier = c(0.1, 0.5, 1, 3, 5), #Regularizadores
                       seed = 42) #Seed para definir partição dos dados
sp_swd #Ver objeto
sp_swd$formula_grid %>% nrow() #Ver numero de modelos candidatos
View(sp_swd$formula_grid) #Ver formulas de cada modelo candidato
View(sp_swd$calibration_data) #Ver dados de calibração

#Salvar dados de preparação
saveRDS(sp_swd, file.path(sp_model, "calibration_data.rds"))

# Ver distribuição espacial de dados de presença e de background/ausência
# No espaço geográfico
p_geo <- explore_partition_geo(data = sp_swd, raster_variables = v_m)
plot(p_geo)
mapview(p_geo$Presence)
# No espaço ambiental
explore_partition_env(data = sp_swd)

#### Calibrar modelos candidatos e selecionar melhores modelos ####
?calibration

#Ver numero de cores disponíveis
parallel::detectCores()

m <- calibration(data = sp_swd, #Output de prepare_data
                 error_considered = c(10, 15, 20), #Taxas de erros consideradas para validar modelos
                 omission_rate = 15, #Taxa de erros usada para selecionar melhor modelo
                 remove_concave = TRUE, #Remover curvas concavas?
                 parallel = TRUE, #Rodar em paralelo?
                 ncores = 8) #Se sim, usar quantos cores?
m #Ver objeto

# Ver todas as métricas de todos os modelos candidatos
View(m$calibration_results$Summary)
# Ver métricas de modelos selecionados
View(m$selected_models)
# Resumo da seleção de modelos: indices de modelos removidos e selecionados
m$summary

#Salvar modelos candidatos
saveRDS(m, file.path(sp_model, "candidate_models.rds"))

#Importar objeto novamente
m <- readRDS(file.path(sp_model, "candidate_models.rds"))


### Ajustar modelos selecionados ####
# Podemos ajustar os modelos finais usando réplicas ou usando todos os dados de
# ocorrência em um único modelo (n_replicates = 1)

fm <- fit_selected(calibration_results = m, #Output de calibration()
                   replicate_method = "kfolds", #Método de partição (se n_replicates > 1)
                   n_replicates = 4, #Numero de réplicas
                   parallel = TRUE, #Em paralelo?
                   ncores = 4, #Numero de cores em paralelo
                   seed = 42) #Seed para definir partição dos dados
fm #Ver objeto
# Alguns objetos resultantes:
names(fm$Models) #ID dos modelos ajustados
fm$thresholds #Thresholds para binarizar modelos individuais e consensos
# Theshold para binarizar corresponde ao threshold de taxa de erro determinado
# em calibration

#Salvar modelos finais
saveRDS(fm, file.path(sp_model, "fitted_models.rds"))

#Importar objeto novamente
fm <- readRDS(file.path(sp_model, "fitted_models.rds"))


#### Projeção do modelo para cenário único ####
?predict_selected

p <- predict_selected(models = fm, #Modelos finais ajustados
                      raster_variables = v, #Variáveis
                      # mask = area_acessivel, #Mascara para cortar variaveis, aqui M da espécie
                      progress_bar = TRUE)


plot(p$Model_1994$Partitions) #Predições de cada réplica
plot(p$Model_1994$Model_consensus) #Consensos das réplicas
plot(p$General_consensus$mean) #Consensos entre os modelos (se houver mais de um modelo)

# Vamos usar a area acessivel para delimitar melhor a potencial distribuição da espécie
# Temos dois jeitos de fazer isso
# A mais comum, é simplesmente cortar o raster de predições usando a area acessivel como máscara
p_acessivel <- crop(p$General_consensus$mean, area_acessivel, mask = TRUE)
plot(p_acessivel)

#Ver com mapview
mapview(p_acessivel) + mapview(pts, cex = 4)

# Outra maneira, é usar a area_acessivel para definir como 0 a adequabilidade de toda área fora da área acessível
p_acessivel2 <- mask(p$General_consensus$mean, area_acessivel,
                     updatevalue = 0)
plot(c(p$General_consensus$mean,
       p_acessivel2),
     main = c("Sem M", "Com M"))
# Essa ultima abordagem é útil quando estamos trabalhando com várias espécies com Ms diferentes

# Quando ajustamos os modelos, definimos uma taxa de erro de 10%
# Que significa que toleramos que 10% dos nossos registros estejam em condições ambientais que não representam o nicho ocupado pela espécie
# Podemos usar essa taxa para binarizar os modelos
# Para isso, usamos o valor de adequabilidade que deixará ~10% dos registros em pixels inadequados (chamado threshold)
# Esse valor de threshold é armazenado no output de fit_selected()
fm$thresholds

# Vamos usar o threshold da média dos consensos
thr <- fm$thresholds$consensus$mean
thr

# Mapa binarizado
p_bin <- (p_acessivel >= thr) * 1 #Todos os valores iguais ou acima do thr se tornam 1, e todos os valores abaixo se tornam 0
plot(p_bin)
#Ver com mapview
mapview(p_bin, col.regions = c("gray", "forestgreen")) + mapview(pts, cex = 4)

#Vamos salvar o mapa binarizado
writeRaster(p_bin,
            file.path(sp_model, "Present_binarized.tif"))

#### Curvas de resposta ####
# Qual o efeito de cada variável sobre a adequabilidade (quando todas as outras
# variáveis são mantidas constantes em sua média)?

# Ver variáveis disponíveis para curvas de resposta
sapply(fm$Models, function(x) names(x[[1]]$betas), simplify = FALSE)
# Além das curvas, o gráfico também mostra os limites da variável na área de
# calibração

# Curvas de respostas considerando todos os modelos
response_curve(models = fm,
               variable = "bio_5")
response_curve(models = fm,
               variable = "bio_6")
response_curve(models = fm,
               variable = "bio_12")
response_curve(models = fm,
               variable = "bio_15")
response_curve(models = fm,
               variable = "elevation")

# Curvas de resposta de produtos - interação entre duas variáveis
bivariate_response(models = fm, modelID = "Model_1994",
         variable1 = "bio_15", variable2 = "bio_12")
bivariate_response(models = fm, modelID = "Model_1994",
                   variable1 = "bio_5", variable2 = "bio_15")
bivariate_response(models = fm, modelID = "Model_1994",
                   variable1 = "bio_5", variable2 = "bio_12")
bivariate_response(models = fm, modelID = "Model_1994",
                   variable1 = "bio_5", variable2 = "elevation")
bivariate_response(models = fm, modelID = "Model_1994",
                   variable1 = "bio_12", variable2 = "elevation")

#### Importância das variáveis ####
?var_importance

imp <- variable_importance(models = fm)
plot_importance(imp)
View(imp)








