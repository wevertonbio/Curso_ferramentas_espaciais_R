#### PADRONIZAÇÃO DE VARIÁVEIS AMBIENTAIS ####

# MAIOR PROBLEMA: variáveis de diferentes fontes tem diferentes resoluções e extensões

# Resolução é o tamanho do pixel:
# - 30arc-sec = 0.00833333º = ~ 1km x 1km
# - 2.5arc-min = 0.0416667º = ~ 4.6km x 4.6km
# - 5arc-min = 0.0833333º = ~ 9km x 9km
# - 10arc-min = 0.166667º = ~ 18.5km x 18.5km

# O R costuma mostrar resolução em graus.

# Extensão corresponde aos limites máximos e mínimo de longitude e latitude

# Variáveis precisam estar EXATAMENTE na mesma resolução e extensão!

# Funções do terra para padronizar variáveis:
# - crop(mask = TRUE) para cortar variáveis
# - aggregate() para agrupar pixels e diminuir resolução (pixel maior)
# - resample() para diminuir resolução e garantir que variáveis tenham mesma extensão
# - project() para garantir que variaveis tenham mesmo sistema de coordenadas.

# Carregar pacotes
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(sf) #Manipulação de dados espaciais. Será usada função gdal_utils
library(pals) #Pacote com paleta de cores
library(scales) #Pacote para visualizar cores

# Primeiros, vamos cortar as variáveis para uma região do mundo que com certeza
# engloba o M de todas as espécies
#Nesse caso, vamos usar a região neotropical como inicial
neot <- vect("Data/Neotropical.gpkg")
mapview(neot)


#Agora, vamos padronizando as variaveis de cada fonte(WorldClim)
# A dica é escolher uma das fontes (geralmente, a com mais variáveis) para servir de base
# Nesse caso, vamos usar o Worldclim, que servira de base para padronizar todas as outras

#### Worldclim ####
wc_files <- list.files("Variaveis/Originais/WorldClim/5/climate/wc2.1_5m/",
                       full.names = TRUE) #Listar arquivos na pasta
wc_files
wc <- rast(wc_files) #Importar arquivos
wc
#Ver nomes das variáveis
names(wc)
#Renomear variáveis

# IMPORTANTE: AO PROJETAR PARA OUTROS CENÁRIOS/TEMPOS, VARIÁVEIS DEVEM TER O MESMO NOME!
names(wc) <- gsub("wc2.1_5m_", "", names(wc))
names(wc) #Novos nomes

#Cortar variáveis para o Neotropico
wc_neot <- crop(wc, neot, mask = TRUE)
plot(wc_neot$bio_1)
plot(wc_neot$bio_12)

#### SoilGrids ####
#Soil grids temos dois tipos de variáveis:
# Variáveis tif (clay e sand)
# Variavel vrt (most probable soil type)
# Vamos trabalhar com elas separadamente
soil <- list.files("Variaveis/Originais/SoilGrids//", full.names = TRUE,
                   pattern = ".tif") #Apenas variáveis .tiff
soil #Ver arquivos
#Remover arquivos aux.xml da lista
soil <- soil[!grepl("aux.xml", soil)]
soil
soil <- rast(soil)
#Compare a projeção de soil com do worldclim
crs(wc_neot) # WGS84 :)
crs(soil) # Homolosine :(
res(wc_neot) #0.083 graus :)
res(soil) #5000 metros :(

#Vamos tentar unir os rasters de solo com o do worldclim
wc_soil <- c(wc_neot, soil)

#Vamos reprojetar o raster de solo para wgs84, usando o wc_neot como base
soil <- project(soil, wc_neot$bio_1,
                method = "bilinear")
soil
#Ver projeção e resolução
crs(soil) # WGS84 :)
res(soil) #0.083 graus :)
plot(soil)

#Vamos renomear as variáveis
names(soil)
names(soil) <- c("clay", "sand") #Novos nomes

#Perceba que as variáveis de solo possuem alguns "buracos" (NA)
mapview(soil$clay)

#Podemos preencher esses buracos usando informações de pixels ao redor
new_soil <- focal(soil,
                  w = 3, #Tamanho da janela de preenchimento
                  fun = "mean", #Método para preencher NA - Aqui, média
                  na.policy="only") #Preencher somente NA
mapview(new_soil$clay)
#Agora, vamos cortar o raster
soil_neot <- crop(new_soil, neot, mask = TRUE)
plot(soil_neot)
#Tentar unir dados de solo e do worldclim
wc_soil <- c(wc_neot, soil_neot)
names(wc_soil) #Ver variáveis

#Agora, vamos padronizar a variável de tipo de solo
soiltype <- rast("Variaveis/Originais/SoilGrids/MostProbable.vrt")
soiltype
plot(soiltype) #Tentar plotar

# Variaveis vrt são variáveis virtuais: os arquivos não estão no seu computador,
# mas sim, na internet
# Vamos usar a função gdal_utils para construir essa raster virtual no seu
# computador
# Convertendo o VRT para TIFF
gdal_utils(
  util = "translate", #usar função do gdal para transformar
  source = "Variaveis/Originais/SoilGrids/MostProbable.vrt", #Caminho do arquivo vrt
  destination = "Variaveis/Originais/SoilGrids/SoilType.tif", #Caminho do arquivo de saída
  options = c(
    "-of", "GTiff",  # Define o formato de saída como GeoTIFF
    "-tr", "0.08333333", "0.08333333",  # Define a resolução para 0.08333333 graus
    "-r", "near", # Define o método de resampling como nearest neighbor
    "-co", "COMPRESS=LZW" #Define o método de compressão
  ))
#Agora, vamos importar o arquivo correto
soiltype <- rast("Variaveis/Originais/SoilGrids/SoilType.tif")
plot(soiltype)
#Ver projeção e resolução
crs(soiltype) # WGS84 :)
res(soiltype) #0.083 graus :)

#Cortar variáveil para neotropico
soiltype_neot <- crop(soiltype, neot, mask = TRUE)
plot(soiltype_neot)
mapview(soiltype_neot) #ver mapa

#Preencher buracos usando informações de pixels ao redor
new_soiltype <- focal(soiltype_neot,
                  w = 3, #Tamanho da janela de preenchimento
                  fun = "modal", #Função para preencher NA - Modal é melhor para categoricos
                  na.policy="only") #Preencher somente NA
plot(new_soiltype)

# Perdemos as categorias 😭
# Raster é uma matriz, então só consegue armazenar informações numericas
# Uma das grandes vantagens do Terra é que ele consegue associar categorias (texto) aos numeros
levels(soiltype_neot) #Ver categorias associadas aos numeros

#Porém, quando usamos a função focal, perdemos as categorias
levels(new_soiltype)

# Vamos apenas transferir as categorias de soiltype_neot para new_soiltype
levels(new_soiltype) <- levels(soiltype_neot)
plot(new_soiltype) #Agora com legenda referente as categorias :)

# Outra vantagem do terra é que podemos armazenar cores para cada categoria
# Vamos usar algumas paletas de cores do pacote pals
browseURL("https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html")
alphabet2(n = 26) #Cores discretas (categoricas)
okabe(n = 8) #Cores discretas - amigável para daltônicos (mas nem tanto)
# Podemos visualizar as cores com a função show_col do pacote scales
show_col(colours = alphabet2(n = 26))
show_col(colours = okabe(n = 8))

#No caso dos tipos de solo, vamos usar a paleta glasbey, que tem 32 cores
show_col(glasbey(n = 32))

#Definir paleta de cores para raster
coltab(new_soiltype) #Não tem paleta de cores

# Extrair valores do raster
raster_valores <- levels(new_soiltype)[[1]]
nrow(raster_valores) #Precisamos de 30 valores
raster_valores
# Criar dataframe com duas colunas: uma com valor, outra com cor
tabela_de_cores <- data.frame(value = raster_valores$VALUE,
                              col = glasbey(n = 30))
# Plotar sem tabela de cores
plot(new_soiltype)
#Adicionar tabela de cores
coltab(new_soiltype) <- tabela_de_cores
# Plotar com tabela de cores
plot(new_soiltype)

# Mapview também usa essas cores
mapview(new_soiltype)

# Ggplot também usa essas cores! (Assunto para ultima aula)

#Agora, vamos cortar o raster
soiltype_neot <- crop(new_soiltype, neot, mask = TRUE)
plot(soiltype_neot)
#Renomear variável
names(soiltype_neot)
names(soiltype_neot) <- "soilType"
#Tentar unir com dados anteriores
wc_soil_type <- c(wc_soil, soiltype_neot)

# Ver variaveis
names(wc_soil_type)

#### Topografia - EARTHENV ####
# Obter caminho das variáveis
topo_files <- list.files("Variaveis/Originais/EarthEnv/",
                         full.names = TRUE) #Caminho completo
topo_files

# Importar rasters
topo <- rast(topo_files)
plot(topo)

#Ver projeção e resolução
crs(topo) # WGS84 :)
res(topo) #0.041 graus :(  Precisamos fazer um resample
# Resample (já corta para mesma area do wc_soil)
topo_res <- resample(topo, wc_soil,
                      method = "average") #Média porque é continua
plot(topo_res)
#Renomear
names(topo_res)
names(topo_res) <- c("elevation", "slope")
#Tentar unir
var_final <- c(wc_soil_type, topo_res)
names(var_final)

#Salvar variáveis finais
writeRaster(var_final, "Variaveis/Variaveis_neotropicos.tif",
            overwrite = T)

#Testar se deu certo
rm(list = ls()) #Limpar objetos
v <- rast("Variaveis/Variaveis_neotropicos.tif")
v
names(v) #Nomes das variaveis
plot(v$soilType) #Quando salva, perde paleta de cores :(
# Mas mantém categorias! Mas para isso, precisa manter na pasta o arquivo:
# Variaveis_neotropicos.tif.aux
# É esse arquivo que mantem as categorias
