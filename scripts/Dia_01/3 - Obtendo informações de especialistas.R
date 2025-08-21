#### Obtendo informações de distribuição de especialistas ####

# Carregar pacotes
library(terra)
library(florabr)
library(rWCVP)
library(rWCVPdata)
library(dplyr)
library(mapview)
library(sf)
library(data.table)


#### FLORA E FUNGA DO BRASIL ####
# Site da base de dados: https://floradobrasil.jbrj.gov.br/consulta/#CondicaoTaxonCP
# Site do pacote: https://wevertonbio.github.io/florabr/
# Site do artigo do pacote: https://bsapubs.onlinelibrary.wiley.com/doi/10.1002/aps3.11616?af=R

# Para usar o florabr, precisamos baixar os dados mais recentes
dir.create("florabr")
#RODE ESSE SCRIPT ABAIXO APENAS UMA VEZ!
# get_florabr(output_dir = "florabr", #Pasta criada para salvar dados
#             solve_discrepancy = TRUE, #Resolver discrepancias species-subspecies?
#             overwrite = TRUE) #Sobrescrever arquivo?

#Após baixar os dados, vamos carregá-lo:
fbr <- load_florabr(data_dir = "florabr")
head(fbr)


#### Selecionar lista de espécies ####

# Uma das principais utilidades do florabr é ajudar a selecionar uma lista de espécies com base em características taxonômicas, ecológicas e geográficas:
# kingdom, group, subgroup, phylum, class, order, family, lifeform, habitat, vegetation, origin, endemism, biome, states, taxonomicstatus or nomenclaturalstatus.
# Vamos ver as opções disponíveis para forma de vida e habitat
fh <- get_attributes(data = fbr, #Objeto importado com load_florabr
                     attribute = c("lifeform", "habitat")) #Atributos para obter opções
View(fh$lifeForm)
View(fh$habitat)

# Imagine que queremos estudar todas as ervas epífitas nativas com ocorrência no Paraná:
# Vamos selecionar essas espécies
?select_species
ervas_epifitas_pr <- select_species(data = fbr, #Objeto importado com load_florabr
                                    lifeForm = "Herb", habitat = "Epiphytic",
                                    state = "PR",
                                    origin = "native")
# Quantas dessas são endemicas do Brasil?
ervas_epifitas_pr %>% count(endemism)

# Perceba que a função retornou espécies que ocorrem no PR & em outros estados.
# Será que existem espécies endêmicas do Paraná?
ervas_endemicas_pr <- select_species(data = fbr, #Objeto importado com load_florabr
                                    lifeForm = "Herb", habitat = "Epiphytic",
                                    state = "PR",
                                    filter_state = "only", #APENAS no estado desejado
                                    origin = "native")
# Veja que algumas delas não são totalmente endêmicas...
ervas_endemicas_pr %>% filter(endemism == "Non-endemic") %>% View()
# Essas espécies tem ocorrência no Paraguai

# Em que tipo de vegetação essas espécies endemicas do PR ocorrem?
ervas_endemicas_pr %>% count(vegetation)


#### Obter informações de espécies ####
# Caso queira extrair informações de uma ou mais espécies, use a função:
?subset_species

# Essa função funciona apenas com nomes binomiais (Genero + especie + subspecie/variedade(opcional))
# Caso tenha um nome completo com autor, utilize a função:
?get_binomial
# Por exemplo
get_binomial("Araucaria angustifolia (Bertol.) Kuntze")
get_binomial("Conchocarpus cuneifolius var. confertus Kallunki")
get_binomial("Conchocarpus cuneifolius var. confertus Kallunki",
             include_variety = FALSE) # Não incluir variedade
get_binomial("Esenbeckia grandiflora subsp. brevipetiolata Kaastra")
get_binomial("Esenbeckia grandiflora subsp. brevipetiolata Kaastra",
             include_subspecies = FALSE) # Não incluir subspecie

# Como sempre, vamos criar um objeto com o nome da espécie para facilitar
sp <- "Araucaria angustifollia"

# Checar se a espécie existe na base de dados
sp_checked <- check_names(data = fbr, species = sp)
sp_checked
# Corrigir nome
sp <- sp_checked$Suggested_name
sp

# Vamos obter as informações dessa espécie
sp_info <- subset_species(data = fbr, species = sp)
View(sp_info)
# Nomes populares
sp_info$vernacularName
# Curi = kuri’y (pinheiro em Guarani)

#### Espacializar informações sobre distribuição ####
# Podemos obter poligonos dos Estados e Biomas que uma espécie ocorre com a função:
?get_spat_occ

# Vamos obter os poligonos com Estados e biomas onde a Araucaria ocorre
araucaria_dist <- get_spat_occ(data = fbr, species = sp)
plot(araucaria_dist$`Araucaria angustifolia`$states)
plot(araucaria_dist$`Araucaria angustifolia`$biomes)
plot(araucaria_dist$`Araucaria angustifolia`$states_biomes)
# Mapa interativo
mapview(araucaria_dist$`Araucaria angustifolia`$states, layer.name = "Estados") +
  mapview(araucaria_dist$`Araucaria angustifolia`$biomes, layer.name = "Biomas") +
  mapview(araucaria_dist$`Araucaria angustifolia`$states_biomes,
          layer.name = "Estado-Bioma")

# São informações bem grosseiras, mas que podem ser úteis...

# Para animais, temos um pacote muito parecido que acessa o Catálogo Taxonomico da Fauna do Brasil
# https://wevertonbio.github.io/faunabr/

#### WORLD CHECKLIST OF VASCULAR PLANTS (WCVP) ####
# Sobre a base de dados: https://www.nature.com/articles/s41597-021-00997-6
library(rWCVP)
library(rWCVPdata)
# Artigo sobre o pacote rWCVP: https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.18919
# Tutoriais do pacote: https://matildabrown.github.io/rWCVP/

# Obter distribuição
range_araucaria <- wcvp_distribution(taxon = sp,
                                  taxon_rank = "species")
# Demora um pouco...
View(range_araucaria)
# Para garantir apenas regiões onde é nativa
range_nativo <- wcvp_distribution(taxon = sp,
                                  taxon_rank = "species",
                                  introduced = FALSE,
                                  extinct = FALSE,
                                  location_doubtful = FALSE)
# Mapear distribuição
range_map <- wcvp_distribution_map(range_nativo)
range_map
# Para dar zoom na distribuicao
range_map_zoom <- wcvp_distribution_map(range_nativo, crop_map = TRUE)
range_map_zoom

# Um exemplo com espécie introduzida
range_eucalipto <- wcvp_distribution(taxon = "Eucalyptus",
                                     taxon_rank = "genus")
wcvp_distribution_map(range_eucalipto)

# Vamos obter o polígono da distribuição da Araucária
# O pacote rWCVP trabalha com o pacote sf ao invés do terra
araucaria_range <- st_union(range_nativo)
class(araucaria_range)
# Mas podemos transformar para SpatVector
araucaria_range <- vect(araucaria_range)
plot(araucaria_range)

# Comparar com distribuição de acordo com Flora do Brasil
mapview(araucaria_dist$`Araucaria angustifolia`$states_biomes,
        layer.name = "Flora do Brasil",
        col.regions = "forestgreen") +
  mapview(araucaria_range, layer.name = "WCVP",
          col.regions = "firebrick")

# Vamos plotar os pontos de Araucaria
occ <- fread("Ocorrencias/Araucaria angustifolia/Ocorrencias_final.gz")
# Espacializar
pts <- vect(occ, geom = c(x = "decimalLongitude",
                          y = "decimalLatitude"),
            crs = "epsg:4326")
mapview(araucaria_dist$`Araucaria angustifolia`$states_biomes,
        layer.name = "Flora do Brasil",
        col.regions = "forestgreen") +
  mapview(araucaria_range, layer.name = "WCVP",
          col.regions = "firebrick") + mapview(pts, cex = 3)

# Ponto de Araucaria no meio do Uruguai:
# https://www.inaturalist.org/observations/83641772
# Na Bahia: https://www.inaturalist.org/observations/38310632

#### IUCN ####
# IUCN também disponibiliza alguns shapefiles com informações sobre o range da espécie
# Para Araucaria não tem 🥲
# https://www.iucnredlist.org/species/32975/2829141
# Exemplo: Panthera onca
# https://www.iucnredlist.org/species/15953/123791436

#### Native range estimates for red-listed vascular plants ####
# Artigo: https://www.nature.com/articles/s41597-022-01233-5
# Range nativo obtido por meio de modelos de nicho ecológico e "validados" comparando com o range da IUCN e por especialistas (algumas espécies)
# Mapa interativo: https://plant-ranges.indecol.no/#
# Para usar no R, precisa baixar os dados (TODOS!)

# Obter metadados dos rasters
metadados <- fread("Data/native range/metadata_default.csv")
# Ver se tem mapa para Araucaria
sp %in% metadados$scientificname

# Tem Pau-brasil
"Paubrasilia echinata" %in% metadados$scientificname

# Vamos seguir com pau-brasil como exemplo
# Qual o ID do pau-brasil nos dados
pau_brasil_id <- metadados %>%
  filter(scientificname == "Paubrasilia echinata") %>%
  pull(speciesID)
pau_brasil_id

# Importar todos os rasters de native range
native_ranges <- rast("Data/native range/range_data_raw.nc",
                      subds = 3) #Apenas native region (sem predictions)
names(native_ranges) %>% head()

# Native range de pau-brasil
pau_brasil_range <- native_ranges[[pau_brasil_id]] %>%
  trim() #Remover bordas
plot(pau_brasil_range)
mapview(pau_brasil_range)

# Converter para polígono
pau_brasil_range2 <- as.polygons(pau_brasil_range)
plot(pau_brasil_range2)
mapview(pau_brasil_range2)


