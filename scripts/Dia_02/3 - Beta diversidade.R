#### Beta diversidade ####
# Artigo do pacote betapart:
# https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/j.2041-210X.2012.00224.x

# Carregar pacotes
library(terra)
library(mapview)
library(data.table)
library(dplyr)
library(pals)
library(tidyr)
library(tibble)
library(betapart)

#### Beta-diversidade par a par ####

# O pacote betapart permite calcular a betadiversidade (variação de composição entre sites) e particionar essa betadiversidade em turnover (locais são diferentes porque tem espécies diferentes - substituição de espécies) e aninhamento (locais são diferentes porque local menos rico é um subconjunto do locao mais rico).

# Pacote utiliza como métricas de dissimilaridade os indices de Jaccard e de Sorensen:
# Jaccard: dá mesmo peso para espécies compartilhadas e não compartilhadas.
# Sorensen: dá peso dobrado às espécies compartilhadas do que as espécies não compartilhadas. Mais sensível as semelhanças.

# Dados de entrada das funções são PAMs!

# Importar dados de plots do projeto TEAM disponivel no BIEN
# Dados gerados no script 6
pam11 <- fread("Data/pam_team_2011.gz")

# Vamos calcular os indices de betadiversidade par a par
beta_par <- beta.pair(x = pam11[,-c(1:3)], index.family = "sorensen")
# Betadiversidade total
beta_par$beta.sor
# Converter para dataframe para ficar mais fácil de ver
# Extrair indice
betad <- beta_par$beta.sor
# Definir os nomes das linhas do objeto com os nomes dos sites
attr(betad, "Labels") <- pam11$plot_name
# Converter para dataframe e mudar formato para longo (longer)
beta_df <- betad %>%
  as.matrix() %>% #Converte para matrix
  as.data.frame() %>% #Converte para dataframe
  rownames_to_column(var = "Site1") %>% #Adiciona row.names como coluna
  pivot_longer( #Converte formato de wider para longer
    cols = -Site1, #Não considerar coluna com nomes dos sites na hora de transformar
    names_to = "Site2", #Adiciona colunas Site2
    values_to = "Sorensen" #Cria coluna com valores de betadiversidade
  )
# Vamos adicionar os valores de turnover (substituição) e nestedness (aninhamento)
turnover <- beta_par$beta.sim %>% as.matrix() %>% as.numeric()
aninhamento <- beta_par$beta.sne %>% as.matrix() %>% as.numeric()
# Adicionar valores a tabela
beta_df <- beta_df %>%
  mutate(turnover = turnover,
         aninhamento = aninhamento)

# Remover duplicadas reversas
beta_df <- beta_df %>%
  group_by(grp = paste(pmax(Site1, Site2), pmin(Site1, Site2),
                       sep = "_")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-grp) %>%
  filter(Sorensen != 0) #Remove comparações de site com ele mesmo

# Quais comunidades são mais diferentes entre si?
# (maior valor de beta diversidade)
beta_df %>% slice_max(Sorensen, n = 3)

# Quais comunidades são mais semelhantes entre si?
# (menor valor de beta diversidade)
beta_df %>% slice_min(Sorensen, n = 3)

# Quais comunidades tem mais turnover?
beta_df %>% slice_max(turnover, n = 3)

# Quais comunidades tem maior aninhamento?
beta_df %>% slice_max(aninhamento, n = 3)

# Será que a betadiversidade aumenta quando aumentamos a distância?
# Obter distancia entre sites
d <- distance(pam11[,c("longitude", "latitude")], #Coordenadas
              unit = "km", #Unidade de medida
              lonlat = T) #Coordenadas em longitude a latitude (não UTM)
# Ajustar modelo relacionando betadiversidade ~ distancia
decay <- decay.model(y = beta_par$beta.sor,
                     x = d, y.type = "dissimilarities",
                     model.type = "exp")
#  Se retorner um erro, reinicie o R (Session > Restar R), carregue somente o betapart e tente rodar novamente:
library(betapart)

# Plotar resultado
plot.decay(decay, bg = "forestgreen", pch = 21)

# Relação é significativa?
decay$p.value

# Quanto a distância explica?
decay$pseudo.r.squared

# Podemos também avaliar apenas o turnover (beta_par$beta.sim) e apenas o aninhamento (beta_par$beta.sne), basta substituir na funcao decay.model

#### Beta-diversidade temporal ####
# O pacote betapart também permite avaliar a betadiversidade temporalmente, ou seja, o quanto (e como - se por turnover ou aninhamento) uma comunidade mudou entre o tempo 1 e o tempo 2
# Como exemplo, vamos avaliar como os plots variaram entre 2005 e 2011

# Importar dados de plots do projeto TEAM
team <- fread("Data/plots_team.gz")
unique(team$year)
# Selecionar apenas os dados de 2005 e 2011
team <- team %>% filter(year %in% c(2005, 2011))

# Selecionar colunas e converter em PAM
pam <- team %>% select(year, plot_name, longitude, latitude, species) %>%
  mutate(presence = 1) %>% #Criar coluna indicando presenca
  distinct() %>% #Apenas valores unicos (remover repeticoes de mesmo ano)
  pivot_wider(names_from = species, #Define a coluna que se tornará as novas colunas
              values_from = presence, #define a coluna que conterá os valores
              values_fill = 0) %>% # preenche os valores ausentes (onde não há espécie) com 0
  arrange(plot_name) #Organizar por plot_name

# Perceba qu agora temos 24 sites: 12 de 2005 e 12 de 2011
# Vamos separar essa PAM
pam2011 <- pam %>% filter(year == 2011)
pam2005 <- pam %>% filter(year == 2005)
#Ver parte da tabela
head(pam2011)
head(pam2005)

# Os sites precisam estar na mesma ordem!

# Agora, podemos comparar as mudanças de composição entre as datas
# Vamos usar o Jaccard agora
beta_tempo <- beta.temp(x = pam2005[, -c(1:4)], y = pam2011[, -c(1:4)],
                        index.family = "jaccard")
# Adicionar informação de local
beta_tempo <- beta_tempo %>%
  mutate(plot_name = pam11$plot_name, #Criar coluna com plot_name
         .before = 1) %>% #Posicionar como primeira coluna
  rename(Turnover = beta.jtu, Nestedness = beta.jne, Jaccard = beta.jac)
# Composição pouco mudou, mas parece haver perda/ganho de espécies (nestedness)

#### Beta-diversidade geral ####
# O pacote betapart também permite calcular e particionar a beta-diversidade considerando todos os locais, e não apenas par a par
# Para quem quiser entender melhor como isso é feito:
# https://royalsocietypublishing.org/doi/pdf/10.1098/rsbl.2007.0449

# Vamos utilizar a função beta.mult para calcular esses indices:
beta_geral <- beta.multi(pam2011[, -c(1:4)])
beta_geral

# Será que em 2005 era o mesmo padrão?
beta.multi(pam2005[, -c(1:4)])
