# Instalar pacotes (se necessário) #
#Instalar pacman, que irá conferir os pacotes que necessitam de instalação
# Se não tiver instalado, irá instalar
if(!require("pacman")){
  install.packages("pacman") }

#Carregar pacote
library(pacman)

#Verificar se pacotes estão instalados, e instalar se necessário
p_load("dplyr", "terra", "ggplot2", "data.table", "tidyterra", "geobr",
       "geodata", "sf", "rnaturalearth", "metR", "ggspatial", "jsonlite",
       "rgbif", "BIEN", "florabr", "rWCVP", "stringi", "pbapply", "imager",
       "red", "rnaturalearth", "rnaturalearthdata", "wdpar", "pals", "scales",
       "tidyr", "betapart", "parallel", "bamm", "tidyterra", "metR", "ggspatial",
       "stringr", "CoordinateCleaner", "spThin", "rpaleoclim", "fs", "remotes",
       "ape", "factoextra", "rgl", "ggcorrplot", "spatstat", "spatialEco",
       "MetBrewer", "patchwork", "ggpubr", "spThin")

# Se não aparecer nada, significa que todos os pacotes estão instalados!

# Se aparecer algum erro dizendo que não foi possível instalar o pacote porque
# ele está em uso no R, reinicie o R e tente novamente

# Instalar pacoted direto do github
if(!require("biosurvey")){
  remotes::install_github("claununez/biosurvey")
}
if(!require("kuenm2")){
  remotes::install_github("wevertonbio/kuenm2")
}
if(!require("faunabr")){
  remotes::install_github("wevertonbio/faunabr")
}
if(!require("rWCVPdata")){
  remotes::install_github("matildabrown/rWCVPdata")
}
if(!require("moranfast")){
  remotes::install_github("mcooper/moranfast")
}
if(!require("flexsdm")){
  remotes::install_github("sjevelazco/flexsdm")
}
