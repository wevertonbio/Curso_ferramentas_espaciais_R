#### Análises de correlação entre variáveis ambientais ####

# Carregar pacotes
library(terra)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(mapview)
library(rgl)
library(pals)
library(scales)
library(factoextra) #Plotar gráfico de PCA e contribuição de variáveis

# Importar variaveis
variaveis <- rast("Variaveis/Variaveis_neotropicos.tif")
names(variaveis)

# Cortar para Mata Atlântica
# Importar mapa da Mata Atlantica do GitHub
af <- vect("https://github.com/wevertonbio/spatial_files/raw/refs/heads/main/Data/AF_limite_integrador.gpkg")
# Ou...
# af <- vect("Data/Mata_Atlantica_limite_integrador.gpkg")
plot(af)
mapview(af)

# Cortar variáveis para Mata Atlântica
var_af <- crop(variaveis, af, mask = TRUE)
plot(var_af$bio_1,
     type = "interval") # Plotar intervalos
plot(var_af$bio_12,
     type = "interval") # Plotar intervalos
plot(var_af$soilType) # Plotar intervalos

# Algumas variáveis possuem alta correlação, ou seja, fornecem a mesma informação
# Por exemplo, variáveis de temperatura e de altitue
plot(var_af[[c("bio_11", "elevation")]], type = "interval", legend = FALSE)
# Em geral, lugares mais altos são também mais frios

# Em algumas análises, essa correlação pode ser problemática
# Uma abordagem comum é fazer uma análise de correlação para selecionar as variáveis
# E manter somente aquelas com baixa correlação

#### Análise de correlação ####
# Antes, vamos transformar o raster com as variáveis em um data.frame
df <- as.data.frame(var_af,
                    na.rm = TRUE) #Não retornar NAs
# Vamos remover a coluna de tipo de solo, porque ela não é numérica
df <- df %>% select(-soilType)

# Análise de correlação
correlacao <- cor(df)
correlacao

#Plotar gráfico com ggcorrplot
g_cor <- ggcorrplot(correlacao, #Matriz de correlação
                    method = "square", #Square or circle?
                    type = "lower", #"full", "lower" or "upper" display.
                    lab = TRUE, # Mostrar valor das correlações?
                    lab_size = 2.5, #Tamanho da fonte
                    hc.order = TRUE, # Reordenas variaveis de acordo com correlação?
                    outline.color = "white", # Cor em torno dos circulos
                    ggtheme = theme_minimal(), #Tema
                    legend.title = "Correlation") + #Título da legenda
  theme(legend.position = "right") #Posição da legenda
g_cor
# Salvar em alta resolução
ggsave(filename = "images/correlacao.png",
       units = "in", #Unidade de medida para definir resolução e tamanho (poleg)
       dpi = 600, #Resolução (em dots per inch)
       width = 8, height = 7, #Largura e altura do plot
       scale = 1.2, #Controla tamanho geral do texto: valores maiores deixam texto menor
       bg = "white") #Cor de fundo (NULL para transparente)

# Problema de correlação: precisa avaliar variável por variável
# Vamos determinar um threshold e escolher uma variavel por vez
# Aqui, vamos manter apenas as variaveis com correlação menor que 0.7 (ou maior que -0.7)
thr <- 0.7
# Começar escolhendo duas variáveis (escolha outras caso prefira)
var_to_keep <- c("bio_6", "bio_15")

# Identificar variaveis que não tem alta correlação (> thr) com variaveis mantidas
var_without_correlation <- rownames(abs(correlacao)[, var_to_keep])[
  apply(abs(correlacao)[, var_to_keep], 1, function(x) all(x <= 0.7))]
var_without_correlation #Escolha uma dessas e adicione a var_to_keep. Rode até aparecer 0

#Vai atualizando aqui e rode var_without_correlation de novo
# Por exemplo, vamos manter também a bio_12..
var_to_keep <- c("bio_6", "bio_15", "bio_12", "elevation", "bio_16", "bio_2",
                 "bio_5", "slope")
# Rode de novo as linhas 77 a 79...
#Vamos manter essas variáveis
var_to_keep

# Ver correlacao entre variaveis
cor(df[, var_to_keep]) %>%
  View()

# Vamos salvar os nomes dessas variáveis
saveRDS(var_to_keep, "Variaveis/Variaveis_para_manter.rds")


# Outra abordagem é sumarizar/resumir a variabilidade das variáveis em eixos de PCA
# Um pacote legal para entender a matemática por trás do PCA é o pacote
browseURL("https://bryanhanson.github.io/LearnPCA/articles/Vig_01_Start_Here.html")

# Outra dica é fazer a disciplina de Análises Multivariadas do Padial.

# Uma explicação beeeem grosseira de como PCA é util em análises espaciais
# Primeiro, vamos utilizar apenas 3 variáveis como exemplo:
ex <- df[, c("bio_5", "bio_10", "bio_12")]
# Vamos usar scale para centralizar e deixar as variáveis na mesma "unidade de medida"
ex <- scale(ex)

# Vamos plotar esses dados em 3 dimensoes
open3d()
plot3d(x = ex[,1], y = ex[,2], z = ex[,3],
       xlab = "bio_5", ylab = "bio_10", zlab = "bio_12", col = "gray50")
# Qual o eixo que tem maior variação?
# Tente posicionar o plot de maneira que ele fique mais "largo"...
# Esse é o primeiro eixo (ou primeiro componente da Análise de Componentes Principais)
# Perceba que nesse eixo, a variação ocorre principalmente na temperatura (bio_5 e bio_10)
# Ou seja, o primeiro eixo já resume muito bem duas variáveis!

# Agora, tente encontrar o eixo com a segunda maior variação
# O segundo maior eixo (ou segundo componente da Análise de Componentes Principais) parece ter mais relação com precipitação (bio_12)

# Uma análise de PCA faz isso: procura os eixos com maior variação

# Lembra que cada ponto desse é um pixel com uma coordenada geográfica
# Quando projetamos o primeiro componente espacialmente, ao invés do pixel ter 3 valoes (um para bio_5, um para bio_10 e um para bio_12), ele vai adquirir o valor em relação ao eixo 1.
# Mesma coisa para o componente 2, componente 3, etc

# O PCA vai gerar um numero de componentes igual ao numero de variáveis
# Se o PCA for feito com 20 variáveis, irá gerar 20 componentes

# Como escolher quantos componentes?
# Cada eixo explica uma % de variação total dos dados.
# Exemplo: eixo 1 explica 50%, eixo dois explica 30%, eixo 3 explica 10%...
# Geralmente, escolhemos os primeiros eixos que juntos explicam 90% ou 95% da variação
# Em geral, isso dá 4 ou 5 eixos.

# Mas vamos fazer o PCA de verdade, sem ser no olhômetro

# Podemos fazer o PCA com o data.frame que criamos, ou direto com o SpatRaster
# Vamos fazer com a tabela primeiro
pca <- prcomp(df, center = TRUE, scale = TRUE)
# Aqui, temos o desvio padrão de cada eixo
pca$sdev #Para melhor visualizar, rode:
options(scipen = 999)
# Para ver a variância, basta elevar ao quadrado:
pca$sdev^2 %>% round(digits = 2)
# Para ver quantos % da variância cada eixo explica, basta ver o quanto da variância total cada eixo corresponde
(pca$sdev^2/sum(pca$sdev^2)) * 100

# Podemos usar a função cumsum para ver a explicação cumulativa
cumsum(pca$sdev^2/sum(pca$sdev^2)) * 100

# Os 5 primeiros eixos explicam >90% da variação
# Os 6 primeiros eixos explicam >95% da variação

# Ver contribuições de cada variavel a cada eixo
View(pca$rotation)

# Vamos plotar esses eixos
# Plotar gráfico de PCA e contribuição de variáveis
# Eixos 1 e 2
pca_plot12 <- fviz_pca_var(pca, #Resultado do pca
                           axes = c(1, 2), #Eixos
                           col.var="contrib", #Colorir por contribuição
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Cores
                           repel = TRUE) # Evita sobreposição de texto
pca_plot12

# Para plotar pontos também
# Cuidado: talvez seu R morra
plot_com_pontos <- fviz_pca(pca, #Resultado do pca
                    label = "var", #Mostrar apenas legenda das variaveis (não de cada ponto)
                    col.var= "contrib", #Colorir por contribuição
                    col.ind = "coord", #Colorir por posição do ponto
                    alpha.ind = 0.5, #Transparencia dos pontos
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
plot_com_pontos
# Podemos salvar com ggplot
ggsave(filename = "images/PCA.png",
       plot = plot_com_pontos,
       units = "in", #Unidade de medida para definir resolução e tamanho (poleg)
       dpi = 600, #Resolução (em dots per inch)
       width = 8, height = 7, #Largura e altura do plot
       scale = 1, #Controla tamanho geral do texto: valores maiores deixam texto menor
       bg = "white") #Cor de fundo (NULL para transparente))

# Eixos 3 e 4
pca_plot34 <- fviz_pca_var(pca, #Resultado do pca
                           axes = c(3, 4), #Eixos
                           col.var="contrib", #Colorir por contribuição
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Cores
                           repel = TRUE) # Evita sobreposição de texto
pca_plot34

# Agora, vamos espacializar o PCA
pca_variaveis <- predict(object = var_af, model = pca, index = 1:6)
plot(pca_variaveis)

# Comparar
plot(c(pca_variaveis$PC1, var_af$bio_11))

# Também podemos fazer o PCA direto com o spatraster
#Remover soiltype do spatraster
variaveis_continuas <- setdiff(names(var_af), "soilType")
variaveis_continuas
var_af_continuo <- var_af[[variaveis_continuas]]
# PCA
pca_terra <- prcomp(var_af_continuo, scale = TRUE, center = TRUE)
# Espacializar eixos
pca_variaveis2 <- predict(object = var_af, model = pca_terra, index = 1:6)
plot(pca_variaveis2)
