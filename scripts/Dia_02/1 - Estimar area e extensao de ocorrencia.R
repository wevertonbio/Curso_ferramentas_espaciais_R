#### Estimando área (AOO) e extensão (EOO) de ocupação de espécies ####
library(terra)
library(dplyr)
library(imager)
library(data.table)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)

# Diferença entre área (AOO) e extensão (EOO) de ocupação
image <- load.image("images/aoo_eoo.png") #Carregar imagem de exemplo
plot(image, axes = F) #Plotar imagem

# EOO: Extensão de ocupação: polígono com range/limites de distribuição
# AOO: Área de ocupação: células/pixels (geralmente de 2km x 2km) com ocupação da espécie.

# Alguns pacotes, como o "red", calculam automaticamente a EOO e a AOO:
library(red)

# Importar registros de ocorrência filtrados
occ <- fread("Ocorrencias/Araucaria angustifolia/3-Ocorrencias_CoordinateCleaner.gz")

# Calcular EOO com pacote red
eoo_red <- eoo(occ[, c("decimalLongitude", "decimalLatitude")])
eoo_red # Valor em km2

# Calcular aoo com pacote red
aoo_red <- aoo(occ[, c("decimalLongitude", "decimalLatitude")])
aoo_red # Valor em km2

# Porém, essas funções se limitam aos registros de ocorrência da espécie.
# Os pontos de ocorrência podem representar apenas uma parte da área de ocupação da espécie, pois nem todos os locais foram amostrados
# Podemos melhorar as estimativas de AOO e EOO

# Vamos espacializar as ocorrências
pts <- vect(occ, geom = c(x = "decimalLongitude", y = "decimalLatitude"),
            crs = "epsg:4326")
plot(pts)

#### Minímo polígono convexo ao redor dos pontos ####
mpc <- hull(pts)
# Plot estático
plot(mpc)
plot(pts, add = T)
# Plot interativo
mapview(mpc) + mapview(pts, cex = 2)
# Calcular extensão de ocorrência
eoo_mpc <- expanse(mpc, unit = "km")
eoo_mpc # Valor em km2
eoo_red # Mesmo valor calculado pelo pacote red

# Podemos ser mais otimistas e considerar que a espécie ocupa uma região um pouco maior que a definida pelo polígono
# Vamos adicionar um buffer de 100km ao redor do polígono...
mpc_buffer <- buffer(mpc,
                     width = 100 * 1000) #Tamanho do buffer em metros)
mapview(mpc_buffer, col.regions = "orange") +
  mapview(mpc, col.regions = "firebrick") +
  mapview(pts, cex = 2)
# EOO de poligono com buffer
expanse(mpc_buffer, unit = "km")

# Porém, perceba que parte dessa área cai no mar
# Podemos refinar esse polígono considerando apenas a porção terrestre
# Vamos importar um mapa mundi
mundo <- vect(rnaturalearth::countries110)
plot(mundo)

#Cortar poligono de EOO
eoo_mpc_new <- crop(mpc_buffer, mundo)
mapview(mpc_buffer, layer.name = "Antigo EOO", col.regions = "firebrick") +
  mapview(eoo_mpc_new, layer.name = "Novo EOO", col.regions = "forestgreen") +
  mapview(pts, cex = 2)
# Nova Extensão de Ocorrencia
expanse(eoo_mpc_new, unit = "km")

#### Minimo polígono convexo por grupo ####
# Para espécies com distribuição disjunta (ex: Amazonia e Mata Atlântica), é interessante criar polígonos disjuntos
# Para isso, podemos separar os grupos usando clusters (k-means)

# Extrair coordenadas em uma matrix
coordenadas <- occ %>% select(decimalLongitude, decimalLatitude) %>%
  as.matrix()
# Separar os pontos em dois grupos
grupos <- kmeans(coordenadas, centers = 2)
table(grupos$cluster) #Numero de pontos em cada grupo
# Adicionar essa informação aos pontos espacializados
pts$grupo <- grupos$cluster
#Plotar
mapview(pts, zcol = "grupo")

# Agora, podemos criar poligonos convexos para cada grupo
# Vamos também adicionar um buffer de 100km e limitar ao continente
# Dica: Vamos usar funções do dplyr. O ponto (.) indica que a função deverá herdar o objeto resultante da função anterior
mpc_grupo <- hull(pts, by = "grupo") %>% # Minimo pol. convexo por grupo
  buffer(., width = 100 * 1000) %>%  #Adicionar buffer de 100km
  crop(., mundo) #Cortar usando o mapa do mundo
# Plot
mapview(mpc_grupo, legend = FALSE) +
  mapview(pts, cex = 2, zcol = "grupo", col.regions = c("blue", "red"))
# Vamos ver a EOO agora
expanse(mpc_grupo, unit = "km") #Um para cada grupo
expanse(mpc_grupo, unit = "km") %>% sum() #Total

# Porém, perceba que existe uma área de sobreposição devido ao buffer, que está sendo contada duas vezes na estimativa de área
# Vamos fundir esses dois poligonos
mpc_unico <- terra::aggregate(mpc_grupo)
# Veja a diferença
plot(mpc_grupo, main = "Por grupo")
plot(mpc_unico, main = "Unificado")
expanse(mpc_unico, unit = "km")


### Área de ocupação ####
# Para definir a área de ocupação, a IUCN sugere uma área de 2km x 2km
# Vamos criar um raster com essa resolução aproximada, para usar de base
# No R, a resolução é trabalhada em graus
# A circunferencia da Terra é de cerca de 40075.017
# Dividindo isso por 360 graus, temos que cada grau corresponde a...
40075.017 / 360 #km2
# Para obter a resolução em km, basta dividir a resolução desejada por 111.3195
resolucao <- 2 / 111.3195
resolucao #Esse valor é aproximado, pois não estamos no Equador e a circunferencia da Terra é menor (nos polos é de 40007.863)

# Além da resolução, o raster base também precisa de uma extensão (limites de longitude e latitude)
# Vamos definir como limite de extensão para o raster um poligono convexo ao redor dos nossos pontos com buffer de 200km
ext_buffer <- hull(pts) %>%
  buffer(., width = 200 * 1000)
plot(ext_buffer)
# Obter extensão
ext_maximo <- ext(ext_buffer)
ext_maximo

# Criar raster base
base <- rast(extent = ext_maximo, res = resolucao, vals = 1)
res(base) * 111.3195 #Resolução em km2
mapview(base)

# Agora, vamos separar os pixels com ocorrência dos pixels sem ocorrência
aoo_occ <- crop(base, pts, mask = TRUE) %>%
  trim() #Para remover NAs em excesso em torno do raster
# Plot estático
plot(aoo_occ, col = "firebrick")
# Plot iterativo
mapview(aoo_occ)
# mapview(aoo_occ, maxpixels = 1157730) #Para mostrar os pixels com tamanho original. CUIDADO PARA NÃO MATAR O R por falta de memória RAM!

# Calcular AOO
expanse(aoo_occ, unit = "km")

#### AOO com buffer ao redor dos pontos ####
# Podemos obter uma estimativa mais otimista da área de ocupação da espécie
# Podemos considerar como AOO os pixels onde a espécie ocorre e também os pixels ao redor

# Temos duas maneiras de fazer isso:

#### Identificando células adjacentes com a função adjacent ####
cells_occ <- as.data.frame(aoo_occ, cells = TRUE)
cells_occ <- cells_occ$cell

# Encontrar células adjacentes
celulas_vizinhas <- adjacent(aoo_occ, cells_occ,
                             directions = 8,
                             pairs = T)
View(celulas_vizinhas)
# Obter celulas vizinhas (segunda coluna)
celulas_vizinhas <- unique(celulas_vizinhas[,2])

# Obter células vizinhas unicas (que não tem ocorrencias)
celulas_vizinhas_unicas <- setdiff(celulas_vizinhas, cells_occ)

# Criar uma cópia do aoo para não modificar o original diretamente
aoo_vizinho <- aoo_occ

# Atribuir o valor 2 às células vizinhas únicas
aoo_vizinho[celulas_vizinhas_unicas] <- 2
plot(aoo_vizinho)
mapview(aoo_vizinho, maxpixels = "1157730")
# Em adjacent(), mude a direção para 8 e depois para 16 para ver a diferença
# Vamos deixar com 8

# Calcular AOO
#Por valor (1 = ocorrencia, 2 = celula vizinha a ocorrencia)
expanse(aoo_vizinho, byValue = TRUE, unit = "km")
# Total
expanse(aoo_vizinho, unit = "km")

# Dica: Função adjacent também é util para encontrar outras coisas adjacentes:
# Florestas de Galeria (adjacentes a rios)
# Áreas não protegidas adjacentes a Unidades de Conservação
# Áreas adjacentes a estradas


#### Buffer ao redor dos pontos ####
# Outra maneira de estimar um AOO mais otimista, é considerando um buffer ao redos dos pontos

# Criar buffer de 20km
occ_buffer <- buffer(pts,
                     width = 20 * 1000) #Tamanho do buffer em metros
mapview(occ_buffer) + mapview(pts, cex = 3, col.regions = "black")

# Cortar o raster base
aoo_buffer <- crop(base, occ_buffer, mask = TRUE) %>%
  trim() #Para remover NAs em excesso em torno do raster
# Remover parte que cai no mar
aoo_buffer <- crop(aoo_buffer, mundo, mask = TRUE)
# Plot
mapview(aoo_buffer) + mapview(pts, cex = 3)
# AOO:
expanse(aoo_buffer, unit = "km")
expanse(aoo_vizinho, unit = "km")
expanse(aoo_occ, unit = "km")

# Métodos mais complexos para estimar AOO e EOO:
# Modelagem de Nicho Ecológico
# Considerar paisagem (ex: quantidade de vegetação natural em cada pixel)
# Assunto para próximas aulas

# Mas com esses dados, já conseguimos responder perguntas interessantes
# Por exemplo, quanto da AOO da espécie está protegida por Unidades de conservação?

# Importar Unidades de conservação

# Caso queira usar UCs do Brasil
# Fonte: https://dados.mma.gov.br/dataset/unidadesdeconservacao
# uc <- vect("Data/cnuc_2025_03.shp")
# plot(uc)

# Para obter UCs do mundo todo:
# https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
# Dados beem pesados (~ 4GB)
# Vantagem: pacote já corrige as geometrias (que são bugadas), não precisa baixar do mundo todo e por ser feito no R é mais reproduzível
# Desvantagem: processo meio lento
# Aqui o código que usei para obter somente as áreas da América do Sul usando o pacote wdpar
# library(wdpar)
#
# sa <- vect("Data/South_America.gpkg") #Importar mapa da América do Sul
# sap <- crop(mundo, sa) #Cortar mapa do mundo usando América do Sul
# plot(sap)
# paises <- sap$NAME %>% unique() #Obter paises da América do Sul
# paises
#
# # Para cada país, baixar as UCs
# ucs <- pbapply::pblapply(paises, function(i){
#   message("Downloading PAs from ", i)
#   wdpa_fetch(x = i, force_download = TRUE, wait = TRUE)
# })
# # Demora uns 20 min
#
# # Reparar e transformar em spatvector
# ucs2 <- pbapply::pblapply(ucs, function(x){
#   pa_x <- wdpa_clean(x)
#   vect(pa_x[, c("NAME", "DESIG", "DESIG_TYPE", "IUCN_CAT", "MARINE",
#                 "STATUS_YR")])
# }) # Demora uns 20min...
# class(ucs2[[1]])
#
# #Salvar PAs
# dir.create("Data/PAs")
# names(ucs2) <- paises
# lapply(paises, function(i){
#   # Reproject to global epsg:4326
#   uc_i <- project(ucs2[[i]], "epsg:4326")
#   writeVector(uc_i,
#               paste0("Data/PAs/", i, ".gpkg"), overwrite = TRUE)
# })

# Importar PAs de Brasil, Argentina, Paraguai e Uruguai
br_pa <- vect("Data/PAs/Brazil.gpkg")
arg_pa <- vect("Data/PAs/Argentina.gpkg")
uru_pa <- vect("Data/PAs/Uruguay.gpkg")
par_pa <- vect("Data/PAs/Paraguay.gpkg")

#Unir PAs (basta colocar dentro de uma lista e usar vect)
pas <- vect(list(br_pa, arg_pa, uru_pa, par_pa))
# plot(pas) #Pode demorar um pouco...
# Vamos simplificar as geometrias (deixar menos complexas)
pas_simples <- simplifyGeom(pas)
plot(pas_simples)

# Vamos usar a extensão que criamos anteriormente para criar o raster base para cortar esse vetor
plot(ext_buffer)
pas_base <- mask(pas_simples, ext_buffer)
plot(pas_base)


#Ver todas as PAs na região
pas_base %>% as.data.frame() %>% View()

# Agora, podemos quantificar a AOO (ou EOO) que está dentro de UCs
# Como exemplo, vamos usar o AOO definido com base nas células adjacentes as celulas com ocorrência
plot(aoo_vizinho)
mapview(aoo_vizinho) + mapview(pas_base)

# Cortar AOO para dentro
aoo_inside_pa <- crop(aoo_vizinho, pas_base, mask = TRUE)
plot(aoo_inside_pa)
mapview(pas_base) + mapview(aoo_inside_pa)
# Area dentro de PAs
area_dentro_pa <- expanse(aoo_inside_pa, unit = "km")[[2]]
area_dentro_pa
# Area total
area_total <- expanse(aoo_vizinho, unit = "km")[[2]]
area_total

# % do AOO dentro de PAs
(area_dentro_pa / area_total) * 100

# Para uma % mais refinada, poderíamos transformar o AOO em spatvector
# Problema: o R se mata ao tentar fazer isso :(
# aoo_poligono <- as.polygons(aoo_vizinho)
# mapview(aoo_poligono)
# # Cortar
# aoo_poligono_in_pa <- terra::crop(aoo_poligono, pas_base)
#
#
# mapview(pas_base) + mapview(aoo_poligono_in_pa)

# Quantos pontos de ocorrência estão dentro de PAs?
occs_inside_pas <- is.related(pts, pas_base, "intersects")
# Registros dentro de PAs
sum(occs_inside_pas)
# Registros fora de Pas
sum(!occs_inside_pas)
# % de registros dentro
(sum(occs_inside_pas) / length(occs_inside_pas)) * 100

# Quais PAs abrangem os pontos?
pas_com_registros <- is.related(pas_base, pts, "intersects")
sum(pas_com_registros) #Quantas PAs abrigam registros?
pas_base[pas_com_registros]$NAME #Quais PAs?
pas_base[pas_com_registros]$NAME %>%
  as.data.frame() %>% View() #Quais PAs? - Ver como dataframe

# Qual a PA que mais tem registros?
records_inside_pas <- relate(pts, pas_base, "intersects")
nrow(records_inside_pas) #Linhas são registros
ncol(records_inside_pas) #Colunas são PAs

# Podemos renomear as colunas da matriz para ficar fácil entende-la
records_inside_pas <- records_inside_pas %>% as.data.frame() #Converter para dataframe
colnames(records_inside_pas) <- pas_base$NAME

#Calcular numero de registros (TRUE) em cada coluna (PA)
n_por_pa <- apply(records_inside_pas, 2, sum)
n_por_pa <- data.frame("PA" = names(n_por_pa),
                       "n" = n_por_pa)
