# Métricas de biodiversidade a partir de matrizes de presença-ausência (PAM) #

# Carregar pacotes
library(BIEN)
library(terra)
library(mapview)
library(data.table)
library(dplyr)
library(tidyr)
library(pals)
library(biosurvey)

# O BIEN (Botanical Information and Ecology Network) possui alguns dados de levantamento florístico em plots
# Pacote do R para obter dados do BIEN: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12861
# A função BIEN_plot_country() permite baixar os plots de um país.
# Vamos baixar do Brasil:

# # Obter dados de plots de todo o Brasil
# plots_brasil <- BIEN_plot_country("Brazil",
#                                   cultivated = TRUE,
#                                   native.status = TRUE,
#                                   natives.only = FALSE,
#                                   political.boundaries = TRUE,
#                                   all.metadata = TRUE)
# # Salvar
# fwrite(plots_brasil, "Data/BIEN_plots_Brasil.gz")

# Importar dados de plots de todo o Brasil
plots_brasil <- fread("Data/BIEN_plots_Brasil.gz")
View(plots_brasil)

# Qual dataset tem mais espécies?
plots_brasil %>% count(datasource)
# Vamos pegar o team
team <- plots_brasil %>% filter(datasource == "TEAM")
# Para entender mais sobre o dataset
BIEN_metadata_citation(team)
# Fonte dos dados: Projeto TEAM (Tropical Ecology Assessment and Monitoring (TEAM) Network)
# TEAM is devoted to monitoring long-term trends in biodiversity, land cover change, climate and ecosystems in tropical forests.
# https://app.wildlifeinsights.org/initiatives/2000017/Tropical-Ecology-Assessment-and-Monitoring-(TEAM)-Network

# Remover registros sem coordenadas
team <- team %>% filter(!is.na(longitude) & !is.na(latitude))
# Manter somente colunas de interesse
colnames(team)
team <- team %>% select(plot_name, subplot, longitude, latitude,
                        cidade = dataset, #Aproveitar para renomear coluna
                        species = scrubbed_species_binomial, #Aproveitar para renomear coluna
                        date_collected, native_status_reason, recorded_by)

# Quantos registros em cada local
team %>% count(cidade)

# Quantas registros são de nativas
team %>% count(native_status_reason)
# Quantas espécies no total
team %>% distinct(species) %>% nrow()

# Plotar locais de plots
locais_plots <- team %>% select(plot_name, longitude, latitude) %>%
  distinct() #Manter apenas valores únicos
locais_plots <- vect(locais_plots, geom = c(x = "longitude", y = "latitude"),
                     crs = "epsg:4326")
# Plot
mapview(locais_plots, zcol = "plot_name", col.regions = pals::alphabet(12))

# Vamos criar uma coluna chamada ID, identificando o nome do plot e o subplot
team <- team %>%
  mutate(ID = paste(plot_name, subplot, sep = "_"),
         .before = 1) #Posicionar na primeira coluna

# Os dados se referem a levantamentos em várias datas diferentes:
unique(team$date_collected)

# Vamos criar uma coluna apenas com o ano
team$year <- format(team$date_collected, format = "%Y") %>% #Extrair ano de data
  as.numeric() #Converter para numero
table(team$year)

# Vamos salvar esses dados
fwrite(team, "Data/plots_team.gz")

# Selecionar apenas os dados de 2011
team11 <- team %>% filter(year == 2011)

# Numa matriz de presença e ausência (PAM), as linhas se referem aos locais e as colunas as espécis. A matriz é então preenchida com 0 (indicando ausencia da espécie nos locais) ou 1 (indicando presença da espécie)

# Vamos converter essa tabela de dados para o formato PAM
# Para isso, precisamos converter essa tabela do formato longo para o formato largo:

# Primeiro, vamos selecionar apenas as colunas de interesse (local e species)
# Aqui, vamos usar os plots (e não os subplots como locais)
d11 <- team11 %>% select(plot_name, longitude, latitude, species) %>%
  mutate(presence = 1) %>% #Criar coluna indicando presenca
  distinct() #Apenas valores unicos (remover repeticoes de mesmo ano)

#  Agora, usamos a função `pivot_wider()` do pacote tidyr para fazer a transformação.
# 'names_from' define a coluna que se tornará as novas colunas (as espécies).
# 'values_from' define a coluna que conterá os valores (a presença).
# 'values_fill' preenche os valores ausentes (onde não há espécie) com 0.

pam11 <- d11 %>%
  pivot_wider(names_from = species, #Define a coluna que se tornará as novas colunas
    values_from = presence, #define a coluna que conterá os valores
    values_fill = 0) # preenche os valores ausentes (onde não há espécie) com 0
View(pam11)

#Vamos salvar essa matriz
fwrite(pam11, "Data/pam_team_2011.gz")

# A PAM é um dos dados ecológicos mais valiosos (e mais difícies de obter)
# Com ele, podemos obter diversas informações
# Por exemplo, podemos ver a riqueza de espécies em cada local simplesmente somando os valores de cada linha
riqueza <- rowSums(pam11[,-c(1:3)]) #Remover 3 primeiras colunas
names(riqueza) <- pam11$plot_name
#Salvar resultado em um dataframe
riqueza <- data.frame(plot_name = pam11$plot_name, #Criar coluna com nomes dos plots
                      riqueza = riqueza, #Criar coluna com riqueza
                      row.names = NULL) #Não adicionar row.names

# Se somaros valores através das colunas, obtemos o "range" da espécie (em quantos lugares ela aparece)
species_range <- colSums(pam11[,-c(1:3)])
# Salvar em um dataframe
species_range <- data.frame(species = colnames(pam11[,-c(1:3)]), #Coluna com nome de espécies
                            range = species_range, #Coluna com range
                            row.names = NULL) #Não adicionar row.names

#Top 3 espécies com maior range
species_range %>% slice_max(range, n = 3)

# Existem muitas outras métricas que podemos obter de uma PAM:
# https://core.ac.uk/download/pdf/162636888.pdf
# Algumas dessas métricas estão implementadas no pacote biosurvey
?PAM_indices

# Antes, vamos converter nossa PAM de data.frame para matriz
pam <- as.matrix(pam11[,-c(1:3)])
# Vamos ver algumas dessas métricas
pam_ind <- PAM_indices(PAM = pam)

# Vamos ver as métricas gerais da comunidades
pam_ind$One_value_indices

# Sites_Cells: Numero de locais

# Species: Numero de espécies

# Av_dispersion_field: para cada local, temos uma média dos ranges das espécies que ocorrem nesse local. Esse valor é a média dessas médias. Valores altos indicam que locais abrigam espécies mais comuns (com ranges maiores). Interessante comparar com modelo nulo.

# AV_diversity field: Para cada espécie, temos a média de riqueza de espécies dentro do range da espécie. Esse valor é a média dessas médias. Valores maiores indicam que muitas comunidades são ricas em espécies comuns. Interessante comparar com modelo nulo.

# Av_shared_community_composition (ou campo de riqueza): Para cada espécie, temos a soma da riqueza dos locais onde a espécie ocorre. Valores mais altos indicam espécies ocorrem mais em locais com grande riqueza de espécies. Esse valor é a média desses valores.

# Additive_Beta (Lande 1996): quantidade de espécies adicionais que você encontra quando compara diferentes habitats em uma região, além daquelas que você já esperaria encontrar em um único habitat. Quanto mais alto, maior diferença de composição entre locais

# Beta_Whittaker (Whittaker 1960): razão entre a diversidade gama e a diversidade alfa média. Indica número de "comunidades efetivas" ou "quantas vezes a composição de espécies muda".

# Beta_Legendre:  soma de quadrados das dissimilaridades entre as comunidades. Quanto maior o valor, maior a dissimilaridade entre os locais.

# Schulter_cov_sites_composition: mede como a associação de espécies muda ao longo da comunidade.
  # Valores maiores que 1 indicam que espécies tendem a ocorrer juntas em mais  comunidades do que seria esperado por acaso. Pode sugerir que compartilham requisitos ambientais ou têm interações de facilitação.
  # Valores menores que 1 indicam segregação (espécies tendem a não ocorrer). Pode indicar forte competição ou que ocupam nichos ambientais distintos.
  # Valores iguais a 1 sugerem distribuições independentes, não diferentes do que seria esperado ao acaso.

# Schluter_cov_species_ranges: mede como o range das espécies muda ao longo da comunidade.
# Valores maiores que 1 indicam que ranges das espécies se sobrepõem mais do que o esperado ao acaso.
# Valores menores que 1 indicam que ranges das espécies se sobrepõem menor do que o esperado ao acaso.
# Valores iguais a 1 sugerem distribuições independentes, não diferentes do que seria esperado ao acaso.

# Wright_Reeves_nestedness: métrica de aninhamento. Indica a some do número de espécies compartilhadas por cada par de locais. Valores maiores indicam que comunidades menos diversas são subconjuntos de comunidades mais diversas.

# Stone_Roberts_Cscore (Checkerboard score): mede a tendência das espécies de formarem um padrão de coocorrência semelhante a um tabuleiro de xadrez. Valores altos indicam maior segregação (maior proximidade com um padrão de distribuição semelhante a um tabuleiro de xadrez). Pode oferecer indícios de que as espécies em uma comunidade estão "se espalhando" para evitar a competição ou se estão "se agrupando" devido a fatores ambientais.
# Precisa comparar com um modelo nulo.

# Métricas para cada site ou espécies
#Riqueza de cada site
pam_ind$Richness

# Range de cada espécie (numero de locais que especie ocorre)
pam_ind$Range

#Riqueza normalizada (0 a 1)
pam_ind$Richness_normalized

#Range normalizado (0 a 1)
pam_ind$Range_normalized

# Média dos ranges das espécies que ocorrem em cada local
pam_ind$Dispersion_field

# Soma de co-ocorrências de cada espécie
# Com quantas espécies cada espécie co-ocorre?
pam_ind$Diversity_field %>% head()

# Soma da riqueza de espécies dentro do range de cada espécie
pam_ind$Shared_community_composition

# Desvio da composição de cada local em relação à composição média da paisagem
# Valores negativos indicam comunidades mais únicas (segreação de espécies)
# Valores positivos indicam comunidades mais parecidas (agrupamento de espécies)
pam_ind$Mean_composition_covariance

# Desvio do range de cada espécies em relação  média da paisagem
# Valores negativos que espécie ocorre em locais que poucas espécies ocorrem (nicho muito específico?)
# Valores positivos indicam que range da espécie se sobrepõe ao range de muitas outras espécies (nicho mais amplo?)
pam_ind$Mean_range_covariance %>% head()

# Matriz de covariância entre composição de locais
# Valores Positivos: indicam que os dois locais tendem a ter composições de espécies semelhantes
# Valores negativos: indicam que os dois locais tendem a ter composições de espécies diferentes
View(pam_ind$Cov_mat_sites_composition)

# Matriz de covariância entre ranges de espécies
# Valores Positivos: indicam que as duas espécies tendem a ocorrer mais que o esperado ao acaso
# Valores negativos: indicam que espécies tendem a se evitar mais que o esperado ao acaso
View(pam_ind$Cov_mat_species_ranges)

# Vamos plotar os pontos dos locais, colorindo os pontos pela riqueza e pelo dispersion field
dados <- pam11 %>%
  select(plot_name, longitude, latitude) %>% #Selecionar colunas de interesse
  mutate(Riqueza = pam_ind$Richness) %>%  #Criar coluna com riqueza
  mutate(Disp_field = pam_ind$Dispersion_field) #Criar coluna com disp. field

# Podemos plotar os pontos sem converter para spatial
mapview(dados,
        xcol = "longitude", ycol = "latitude", #Nomes das colunas com long e lat
        zcol = "Riqueza", #Coluna usada para colorir
        col.regions = rev(pals::brewer.rdylbu(12)), #Paleta de cores
        crs = "epsg:4326") #Precisa especificar sistema de coordenadas

# Ou, convertemos para espacial
pts <- vect(dados, geom = c(x = "longitude", y = "latitude"), crs = "epsg:4326")
# Riqueza
mapview(pts,
        zcol = "Riqueza", #Coluna usada para colorir
        col.regions = rev(pals::brewer.spectral(10))) #Paleta de cores

# Dispersion field
# Valores menos indicam comunidades mais "únicas"
mapview(pts,
        zcol = "Disp_field", #Coluna usada para colorir
        col.regions = rev(pals::brewer.spectral(10))) #Paleta de cores

# Com esses dados podemos explorar algumas questões relacionadas ao espaço
# Por exemplo, será que dois locais mais próximos são mais semelhantes entre si que dois locais mais distantes?
# Ou, em termos mais estatísticos, será que a covariância diminui quando amentamos a distância?

# Primeiro, vamos obter uma matriz de distância dos pontos
d <- distance(pam11[,c("longitude", "latitude")], #Coordenadas
              unit = "km", #Unidade de medida
              symmetrical = T,
              lonlat = T) #Coordenadas em longitude a latitude (não UTM)

# Extrair parte inferior da matriz de covariancia entre sites
View(pam_ind$Cov_mat_sites_composition)
indices_lower <- lower.tri(pam_ind$Cov_mat_sites_composition, diag = FALSE)

# Transforma a matriz de covariância em um vetor
cov_vector <- pam_ind$Cov_mat_sites_composition[indices_lower]

# Transforma a matriz de distâncias em um vetor
dist_vector <- as.vector(d)
# Plot
# Cada circulo representa a comparação entre dois sites
plot(dist_vector, cov_vector, xlab = "Distância", ylab = "Covariância",
     pch = 21, bg = "forestgreen")
# Quanto maior a distância, menor a covariância (mais diferentes são os locais)

# Tarefa
# Agora, analise os dados de 2005 para responder as seguintes questões?
# Quem eram as espécies mais comuns (maior range) em 2005? São as mesmas de 2011?
# Quais eram os locais mais e menos ricos em espécies em 2005? São os mesmos de 2011?
# Qual espécie teve maior ganho de range entre 2005 e 2011? Qual teve maior perda?
# Qual era o local mais único (menor dispersion field) em 2005? É o mesmo que de 2011?






