#### Download de variáveis ambientais ####
# Variaveis já foram baixadas em 'Variaveis_brutas'

#Dica: salve todas as variaveis em uma unica pasta raíz, separando apenas por fontes
# Ex: uma pasta para worldclim, outra para soilgrids, outra para earthenv
# Isso facilita processamento das variáveis

# Variáveis bioclimáticas do presente e do futuro:
# O que são variáveis bioclimáticas?
browseURL("https://worldclim.org/data/bioclim.html")
# WorldClim: https://worldclim.org/data/index.html
browseURL("https://worldclim.org/data/index.html")
# Vantagem: mais utilizado, muitas opções de resolução e muitas opções para o futuro
# Desvantagem: versão recente não tem opções para o passado. Versão recente não é tão recente assim (considera dados de 1970-2000).
# Reference: https://rmets.onlinelibrary.wiley.com/doi/epdf/10.1002/joc.5086
browseURL("https://rmets.onlinelibrary.wiley.com/doi/epdf/10.1002/joc.5086")

# CHELSA: https://chelsa-climate.org/
browseURL("https://chelsa-climate.org/")
# Vantagem: tem muitas opções para o passado (paleomodelagem) e considera clima mais recente (1979–2013)
# Desvantagem: tem menos opções para futuro e só tem em resolução de 1km x 1km (dados mais pesados para baixar)
# Reference: https://www.nature.com/articles/sdata2017122
browseURL("https://www.nature.com/articles/sdata2017122")

# DICA: EVITE USAR AS VARIÁVEIS QUE COMBINAM TEMPERATURA E PRECIPITAÇÃO
# bio_8 (temperatura do trimestre mais umido)
# bio_9 (temperatura do trimestre mais seco)
# bio_18 (precipitação do trimestre mais quente)
# bio_19 (precipitação do trimestre mais frio)
# Variáveis possuem variações bruscas que são artefatos estatísticos, por que os meses mais secos/umidos ou quentes/frios podem mudar muito em regioes proximas
# Referencia para justificar exclusão:
browseURL("https://onlinelibrary.wiley.com/doi/abs/10.1111/aec.13234")


# Variáveis bioclimáticas extendidas do ENVIREM
browseURL("https://envirem.github.io/")
# Geralmente tem grande correlação com variáveis bioclimáticas
# Reference: http://onlinelibrary.wiley.com/doi/10.1111/ecog.02880/full

# Variáveis bioclimáticas do passado
# CHELSA: https://chelsa-climate.org/chelsa-trace21k/
browseURL("https://chelsa-climate.org/chelsa-trace21k/")
# Reference: https://cp.copernicus.org/articles/19/439/2023/cp-19-439-2023.html


# PaleoClim: http://www.paleoclim.org/
browseURL("http://www.paleoclim.org/")
# Reference: https://www.nature.com/articles/sdata2018254
# Pacote rpaleoclim
browseURL("https://cran.r-project.org/web/packages/rpaleoclim/vignettes/rpaleoclim.html")

# Variáveis de solo:
# SoilGrids: https://soilgrids.org/
browseURL("https://soilgrids.org/")
# Reference: https://soil.copernicus.org/articles/7/217/2021/
# Para download em resolução de 1km ou 5km use:
browseURL("https://files.isric.org/soilgrids/latest/data_aggregated/")

# Para baixar classes de solos mais prováveis, use:
browseURL("https://files.isric.org/soilgrids/latest/data/wrb/")
# Baixe o MostProbable.vrt

# Variáveis topográficas (altitude, declive, etc)
browseURL("https://www.earthenv.org/topography")
# Reference: https://www.nature.com/articles/sdata201840

# Biome stability in the last 30k years
# Reference:
browseURL("https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12694
# Link to download: https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fgeb.12694&file=geb12694-sup-0001-suppinfo1.zip")
# Original resolution: 2.5arcmin (~4.5km x 4.5km)

# Gerar próprias variáveis de estabilidade climática
browseURL("https://github.com/hannahlowens/climateStability")
# Reference: https://journals.ku.edu/jbi/article/view/9786

# Outras variáveis interessantes
browseURL("https://www.earthenv.org/")
# Reference: https://www.nature.com/articles/sdata201840

# Mapbiomas (cobertura e uso do solo)
browseURL("https://brasil.mapbiomas.org/downloads/")

# CUIDADO COM VARIÁVEIS QUE PODEM VARIAR MUITO COM O TEMPO (ex: cobertura florestal)
# Dados bioclimáticos são derivados de medições num intervalo de ~30 anos
# Por isso, são mais seguros de associar a dados de ocorrência coletados num
# grande intervalo de tempo
# O quanto outras variáveis refletem as condições do local quando a ocorrência
# foi registrada? Ex: registro de ocorrência coletado em 1981, cobertura florestal
# medida em 2010.
# O quanto outras variáveis sem projeções para o futuro vão mudar?
# Ex: pH do solo, que está correlacionado com vegetação e precipitação, pode ser
# mantida fixa numa projeção para o futuro?


# Ao baixar variáveis ambientais, dê preferência ao download direto pelo R, se disponível
# Isso mantém a reprodutibilidade do código

#### Download de variáveis ambientais do presente e do futuro do WorldClim ####
# Para fazer o download dessas variáveis, vamos usar o pacote geodata

#Carregar pacotes
library(geodata) #Baixar variáveis do WorldClim no presente e futuro
library(pbapply) #Funções para mostrar barra de progresso em loopings
library(parallel) #Para rodas coisas em paralelo
library(fs) #Para visualizar estrutura de pastas

# Vamos definir a resolução dos dados
resolucao <- 5 #Mude para 10 para testar


#Criar diretório para salvar variáveis brutas
to_save <- file.path("Variaveis/Originais/WorldClim", resolucao)
to_save #Ver pasta que sera criada
dir.create(path = to_save, recursive = TRUE)

#Baixar variáveis do presente no WorldClim
worldclim_global(var = "bio", #Variaveis bioclimáticas
                 res = resolucao, #Resolução em arco-minutos
                 path = to_save) #Pasta para salvar

# Checar arquivos na pasta
list.files(to_save, recursive = TRUE)
# Checar arquivos de um jeito mais bonito
dir_tree(to_save)

# Importar alguma variável para plotar
caminho <- list.files(to_save, recursive = TRUE, full.names = TRUE)[1]
caminho
r <- rast(caminho)
plot(r)
# mapview(r) #Pode matar o R se não tiver RAM suficiente

#Baixar variáveis do futuro do Worldclim
# Ver variáveis no site
browseURL("https://worldclim.org/data/cmip6/cmip6_clim10m.html")
?cmip6_world #Ver função para baixar dados
#Criar diretório
to_save_future <- file.path(to_save, "Futuro")
dir.create(to_save_future)

#Como são muitas variáveis, vamos criar um grid de combinações de gcm, ssp e tempo
g <- expand.grid(model = c("ACCESS-CM2", "HadGEM3-GC31-LL", "MIROC6"),
                 ssp = c("126", "585"),
                 time = c("2041-2060","2061-2080"))
g

#Criar cluster
parallel::detectCores() #Ver numero de cores disponiveis
ncores <- 5 #Determinar numero de cores a serem usados em paralelo
cl <- parallel::makeCluster(ncores) #Criar um cluster em cada core
parallel::clusterEvalQ(cl, {library("geodata")}) #Enviar pacotes necessários para cada cluster
#Enviar objetos necessários para cada cluster
parallel::clusterExport(cl, varlist = c("g", "resolucao", "to_save_future"))

#Looping para baixar variáveis
pblapply(1:nrow(g), function(i){ #A cada iteração, i sera uma linha de g
  g_i <- g[i,] #Obter combinação i do grid
  #Baixar combinação i
  geodata::cmip6_world(model = g_i$model, #Modelo i
                       ssp = g_i$ssp, #SSP i
                       time = g_i$time, #Time i
                       var = "bioc", #Baixar variáveis bioclimáticas
                       res = resolucao, #Resolução
                       path = to_save_future) #Pasta para salvar
}, cl = cl)
parallel::stopCluster(cl) #Fechar cluster

#Outras variáveis baixadas direto do site

# Variáveis de solo (clay e sand)
# SoilGrids: https://soilgrids.org/
# Reference: https://soil.copernicus.org/articles/7/217/2021/
# Para download use: https://files.isric.org/soilgrids/latest/data_aggregated/
# Para baixar pelo R
# Criar diretorio
dir.create("Variaveis/Originais/SoilGrids")
# Baixar
# Areia
download.file(url = "https://files.isric.org/soilgrids/latest/data_aggregated/5000m/sand/sand_5-15cm_mean_5000.tif",
              destfile = "Variaveis/Originais/SoilGrids/sand_5-15cm_mean_5000.tif", method = "curl")
# Importar e plotar
areia <- rast("Variaveis/Originais/SoilGrids/sand_5-15cm_mean_5000.tif")
plot(areia)

# Argila
download.file(url = "https://files.isric.org/soilgrids/latest/data_aggregated/5000m/clay/clay_15-30cm_mean_5000.tif",
              destfile = "Variaveis/Originais/SoilGrids/clay_15-30cm_mean_5000.tif", method = "curl")
argila <- rast("Variaveis/Originais/SoilGrids/clay_15-30cm_mean_5000.tif")
plot(argila)

# Classes de solos mais prováveis:
# https://files.isric.org/soilgrids/latest/data/wrb/
# Download pelo R
download.file(url = "https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.vrt",
              destfile = "Variaveis/Originais/SoilGrids/MostProbable.vrt", method = "curl")
download.file(url = "https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.vrt.ovr",
              destfile = "Variaveis/Originais/SoilGrids/MostProbable.vrt.ovr", method = "curl")

# Variáveis topográficas (Altitude e declive)
# https://www.earthenv.org/topography
# Reference: https://www.nature.com/articles/sdata201840
# Download pelo R
# Criar diretorio
dir.create("Variaveis/Originais/EarthEnv")
download.file(url = "https://data.earthenv.org/topography/elevation_5KMmn_GMTEDmn.tif",
              destfile = "Variaveis/Originais/EarthEnv/elevation_5KMmn_GMTEDmn.tif", method = "curl")
altitude <- rast("Variaveis/Originais/EarthEnv/elevation_5KMmn_GMTEDmn.tif")
plot(altitude)

download.file(url = "https://data.earthenv.org/topography/slope_5KMmn_GMTEDmd.tif",
              destfile = "Variaveis/Originais/EarthEnv/slope_5KMmn_GMTEDmd.tif", method = "curl")
declive <- rast("Variaveis/Originais/EarthEnv/slope_5KMmn_GMTEDmd.tif")
plot(declive)
