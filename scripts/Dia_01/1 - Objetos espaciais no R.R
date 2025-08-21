#### OBJETOS ESPACIAIS NO R ####

#Carregar pacotes
library(terra) #Manipular dados espaciais
library(geobr) #Obter mapas do Brasil
library(mapview) #Plotar mapa interativo
library(dplyr) #Manipulação de dataframes e uso do %>%
library(florabr) #Pacote para acessar Flora do Brasil
library(sf) #Outro pacote utilizado para manipular dados espaciais

# library(geodata) #Obter dados do worldclim
# library(ggplot2) #Plotar mapas
# library(tidyterra) #Plotar mapas
# library(rnaturalearth) #Obter mapas do mundo
# library(metR) #Configurações adicionais de plots
# library(ggspatial) #Configurações adicionais de plots

#### 1 - Objetos Raster ####
#Um pouco de confusão...
# Raster: objeto compostos por uma matriz de píxels/células, cada uma contendo um valor que representa as condições da área que esta célula cobre.
# raster é também o nome de um famoso pacote para importar e manipular objetos espaciais no R, mas o autor do pacote se aposentou (e o pacote também).
# O pacote 'raster' foi substituido pelo pacote 'terra'.
# USE O TERRA, NÃO O RASTER!
# Evite usar pacotes como sf e sp,a não ser que seja mesmo necessário
# Iinfelizmente alguns pacotes ainda usam objetos sf e sp

#Importar arquivo raster
f <- system.file("ex/elev.tif", package="terra") #Obter caminho do arquivo de exemplo
f #Caminho para o arquivo tif
# Geralmente, um arquivo raster está no formato TIF (ou TIFF). Também é possível salvar e importar arquivos ASC, mas estes ocupam mais espaço. Prefira salvar os arquivos como TIF.

r <- rast(f) #Importar arquivo
class(r) #Classe do objeto
plot(r) #Plot estático
plet(r) #Plot interativo do terra (não muito bom)
mapview(r) #Plot interativo do mapview (melhor)

#Podemos transformar um raster em um dataframe
d <- as.data.frame(r, xy = TRUE, cells = TRUE)
head(d)
#E podemos transformar um dataframe com coordenadas em um raster
r_from_d <- rasterize(x = as.matrix(d[,c("x", "y")]), #Matriz de coordenadas
                      y = r, #Raster base
                      values = d$elevation) #Valores
plot(r_from_d)

#Salvar rasters
# Criar pasta para salvar arquivos temporários
dir.create("temp")
writeRaster(r, "temp/Raster.tif", overwrite = TRUE) #TIF é melhor formato para dados rasters
#Importar dados novamente
r2 <- rast("temp/Raster.tif")

#### 2 - Vetores espaciais ####
# Vetores espaciais podem ser polígonos (ex: limites de estado ou país), pontos (ex: registros de ocorrência) ou linhas.
f <- system.file("ex/lux.shp", package="terra")  #Obter caminho do arquivo de exemplo
f
# Geralmente, um arquivo vetorial está no formato shp, que deve possuir vários outros arquivos associados....
system.file("ex", package = "terra") %>%
  list.files()
#Veja que temos vários arquivos lux: dbf (guarda tabela com informações sobre o vetor), prj (guarda informações sobre o sistema de projeção), shp (arquivo para importar) e shx (informações sobre a geometria). Todos os arquivos precisam estar na mesma pasta!

v <- vect(f) #Importar arquivo
v
class(v) #Classe do objeto
plot(v) #Plot estático
plet(v) #Plot interativo do terra (não muito bom)
mapview(v) #Plot interativo do mapview (melhor)

# Também podemos transformar um vetor espacial em um data.frame
d2 <- as.data.frame(v, geom = "WKT")
View(d2)
# E podemos transformar um dataframe em um vetor espacial
v_from_d2 <- vect(d2, geom = "geometry")
plot(v_from_d2)

# Também podemos transformar coordenadas long-lat em vetores espaciais
data("occurrences", package = "florabr")
#Selecionar ocorrencias de Araucaria angustifolia
occ <- occurrences %>% filter(species == "Araucaria angustifolia")
View(occ)
#Transformar dataframe em dados espaciais
pts <- vect(occ, geom = c(x = "x", y = "y"), crs = "+init=epsg:4326")
plot(pts)
mapview(pts)
#Salvar vetores
#Formato shapefile (mais comum) - Ver tamanho e numero de arquivos
writeVector(pts, "temp/Pontos.shp",
            overwrite=TRUE)
writeVector(pts, "temp/Pontos2.gpkg", #Formato gpkg - Ver tamanho e numero de arquivos
            overwrite=TRUE)
#Importar dados novamente
pts2 <- vect("temp/Pontos2.gpkg")

#Transformando spatvector de pontos para dataframe
pts_df <- as.data.frame(pts, geom = "XY")
head(pts_df)
head(occurrences)

#### 3 - Fazendo subseleção de pontos com polígonos ####
# Imagina que você quer selecionar apenas os pontos dentro do estado do Paraná
# Vamos importar um mapa do Estado do Paraná do pacote geobr
pr <- read_state(code_state = "PR")
# Caso não funcione por problemas na internet, use esse código
# pr <- vect("Data/Parana.gpkg") %>% st_as_sf
plot(pr)
class(pr) #Classe do objeto

# sf é o nome de uma classe de objetos do pacote "sf". Alguns pacotes de análises mais antigos aceitam apenas objetos sf, e não SpatVector. Porém, podemos converter os formatos de um para outro:

#Converter sf para spatvector
pr <- vect(pr) #Sobrescrever objeto
class(pr)
plot(pr)

#Converter spatvector para sf
pr_sf <- st_as_sf(pr)
class(pr_sf)

# Plotar mapa interativo do Paraná e pontos
mapview(pr,
        col.regions = "forestgreen", #Cor de preenchimento
        lwd = 4, #Espessura da linha
        alpha.regions = 0.5, #Transparencia
        layer.name = "Paraná") + #Nome do mapa para aparecer
  mapview(pts,
          col.regions = "firebrick",
          cex = 2, #Tamanho do ponto
          layer.name = "Araucaria angustifolia")

# Para ver todos os controles de mapview(), acesse:
# https://r-spatial.github.io/mapview/articles/mapview_02-advanced.html

#Selecionar apenas pontos dentro do Paraná
# Existem várias maneiras de fazer isso
# A mais direta, é usar a função mask
pts_mask_pr <- mask(pts, pr)
plot(pr) #Plotar mapa Parana
plot(pts_mask_pr, add = TRUE) #Plotar pontos sobre o mapa

# Outra maneira, é testar quais pontos estão dentro do Paraná
# Essa opção é (MUITO!) mais rápida quando temos muitos pontos ou poligonos muito complexos
pts_in <- is.related(pts, pr, "intersects") #Quais pontos intersectam com o PR?
pts_in
sum(pts_in) #Quantos pontos estão dentro do PR?
pts_pr <- pts[pts_in, ] #Selecionar pontos TRUE
mapview(pts_pr)

#Como a sequencia dos pontos é a mesma, podemos usar o mesmo vetor para selecionar no data.frame
occ_pr <- occ[pts_in, ]
#Para plot de conferencia, podemos utilizar o dataframe
plot(pr) #Plotar mapa Parana
points(occ_pr[, c("x", "y")]) #Plotar pontos sobre o mapa


# Cortando rasters com polígonos
# Podemos baixar variáveis direto do worldclim com o pacote geodata
# O comando abaixo já foi rodado
# geodata::worldclim_global(var = "bio", res = 10, path = "Data/WorldClim10")
# Importar variaveis
lf <- list.files("Data/WorldClim10/climate/wc2.1_10m/", full.names = TRUE)
lf
# Podemos importar variavel por variavel (era assim com o pacote raster)
bio_1 <- rast("Data/WorldClim10/climate/wc2.1_10m/wc2.1_10m_bio_1.tif")
plot(bio_1)
bio_12 <- rast("Data/WorldClim10/climate/wc2.1_10m/wc2.1_10m_bio_12.tif")
plot(bio_12)

# Ou podemos apenas importar todos os arquivos de uma vez!
wc <- rast(lf)
wc
nlyr(wc) #Numero de layers
names(wc) #Nomes das variáveis
plot(wc$wc2.1_10m_bio_1)

#Cuidado ao abrir rasters grandes com mapview
# mapview(wc$wc2.1_10m_bio_1)
#Renomear variaveis
names(wc)
names(wc) <- gsub("wc2.1_10m_", "", names(wc)) #Remover wc2.1_10m_
names(wc) #Novos nomes, sem wc2.1_10m_

#Cortar variáveis para Paraná
#Sem mask: considera apenas extensões mínimas e máximas
wc_pr_ext <- crop(wc, pr)
plot(wc_pr_ext$bio_1)
plot(pr, add = T)
#Com mask: corta como uma forminha de bolacha
wc_pr <- crop(wc, pr, mask = TRUE)
plot(wc_pr$bio_1)
plot(pr, add = T)

#Converter rasters (vários) para dataframe
wc_df <- as.data.frame(wc_pr, xy = TRUE, cells = TRUE)

#Extraindo valores de rasters nos pontos
wc_pts <- extract(x = wc_pr, y = occ_pr[,c("x", "y")],
                  xy = TRUE, cells = TRUE)
View(wc_pts)
# Agora, podemos extrair algumas estatísticas dos pontos
min(wc_pts$bio_1) #Temperatura anual mínima
max(wc_pts$bio_1) #Temperatura anual máxima
mean(wc_pts$bio_1) #Média de Temperatura anual
boxplot(wc_pts$bio_1)

#Agora, vamos ver uma das principais limitações do pacote terra
plot(pr) #Plotar mapa
# Agora, reinicie o R (Session > Restart R) e aguarde o carregamento dos objetos
library(terra) #Carregar pacote novamente
plot(pr) #Plotar mapa

#O erro acontece porque, ao contrário de outros pacotes, o terra não cria uma cópia do objeto na memória RAM, mas sim cria um "endereço" que direciona para o arquivo original.
# Vantagem: podemos trabalhar com arquivos muitos grandes que excedem a quantidade de meória RAM (por exemplo, arquivos com 20GB em um computador com 16GB)
# Desvantagem: Toda vez que reiniciamos o R, esse endereço é perdido.

#Podemos "forçar" o terra a criar uma cópia do objeto na memória RAM usando a função wrap:
pr <- vect("Data/Parana.gpkg")
pr_packed <- wrap(pr)
#Veja a diferença de tamanho
object.size(pr) #Apenas "endereço" na RAM
object.size(pr_packed) #Objeto inteiro duplicado na RAM

#Reinicie o R novamente (Session > Restart R) e aguarde o carregamento dos objetos
library(terra) #Carregar pacote novamente
plot(pr) #Erro: endereço perdido
plot(pr_packed) #Erro: precisamos "desempacotar" o objeto
#Desempacotar
pr_unpacked <- unwrap(pr_packed)
plot(pr_unpacked)
