library(rinat) #Pacote para baixar dados do iNaturalist
library(terra)
library(mapview)
library(sf)
library(data.table)
library(spatstat)
library(spatialEco)
library(pals)

# Importar mapa do Paraná
pr <- vect("Data/Parana.gpkg")
# Esse é um dos casos que precisamos converter para sf...
pr_sf <- st_as_sf(pr)

# Baixar dados de Mirtáceas no Paraná
mirtaceas <- get_inat_obs(taxon_name = "Myrtaceae", #Taxon
                          bounds = pr_sf, #No "Bounding box" do Paraná
                          geo = TRUE, #Apenas registros com coordenadas
                          meta = TRUE, #Retornar metadados
                          maxresults = 10000) #Retornar no máximo 10 mil registros (máximo)
# Demora um pouco...

# Metadados
mirtaceas$meta

#Ver ocorrências
View(mirtaceas$data)

# Alguns registros não chegaram ao nível de identificação de espécie
# Para saber qual o nivel taxônimo, precisamos consultar o
# https://www.inaturalist.org/taxa/inaturalist-taxonomy.dwca.zip
taxonomy <- fread("Data/taxa.gz")


# Unir dados
d <- left_join(mirtaceas$data, taxonomy, join_by(taxon_id == id))
View(d)

# Vamos selecionar apenas os registros identicados até o nivel espécie
table(d$taxonRank)
sp <- d %>% filter(taxonRank == "species")

#Vamos espacializar esses dados
pts <- vect(sp, geom = c(x = "longitude", y = "latitude"), crs = "epsg:4326")
plot(pts) #Plot estático
mapview(pts)

#Vamos selecionar apenas os pontos que caem no Paraná
in_pr <- is.related(pts, pr, "intersects") #Quais pontos caem no PR?
sum(in_pr) #Quantos caem no PR?
pts_pr <- pts[in_pr] #Selecionar apenas pontos que caem no PR
mapview(pts_pr)

#Selecionar apenas pontos que caem no PR (no dataframe
sp <- sp[in_pr,]


# Onde tem mais registro de Mirtaceas?
# Podemos fazer um mapa de calor para ver onde estão as "zonas quentes" de registros
# Algumas dessas análises podem ser feitas com o pacote "spatstat"

# Primeiro, vamos converter nossos pontos para o formato ppp, usado pelo pacote
pontos <- as.ppp(X = sp[,c("longitude", "latitude")], #X são as coordenadas
                 W = as.vector(ext(pr))) # W é os limites geográficos da área de estudo

# Vamos dividir nossa área de estudo em quadrantes
quadrantes <- quadratcount(pontos, #Objeto criado com as.ppp()
                           nx = 5, ny = 10) #Numero de linhas e colunas
# Plotar
plot(pontos)
plot(qcounts1, add = TRUE)
plot(pr, add = T)

# Os pontos estão mais agrupados do que esperado ao acaso?
quadrat.test(pontos,
             nx = 5, ny = 10) #Número de linhas e colunas
# Valor de p < 0.05 indica que está mais agrupado que o esperado ao acaso

# Podemos também fazer um mapa de calor
calor <- density.ppp(pontos, sigma = 0.1) #Mude o valor de sigma para ver a diferença
plot(calor)
plot(pr, add = T)

# Também podemos gerar um mapa de calor no formato SpatRaster com o pacote SpatialEco
pts_sf <- st_as_sf(pts)
# Mapa de calor (kernel density)
kde <- sp.kde(x = pts_sf, #Pontos (em formato sf)
              res = 0.045, # Resolução do raster: ~5km
              standardize = T) #Escalar de 0 a 1
# Cortar para Paraná
kde_pr <- crop(kde, pr, mask = TRUE)
plot(kde_pr)
mapview(kde_pr, alpha.regions = 0.5,
        col.regions = rev(pals::brewer.spectral(n = 10))) +
  mapview(pts_pr, cex = 2)
