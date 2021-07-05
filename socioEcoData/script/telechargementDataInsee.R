#########################################
#                                       #
#     TELECHARGEMENTS DONNEES INSEE     #
#     . Yann DAVID . TI-Biovallée .     #
#             Juillet 2021              #
#                                       #
#########################################


# 00. INSTALLATION PACKAGES -----------------------------------------------
# Initialisation dossiers
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
telechargements <- sub('/script','/data',getwd())


# Installation packages
## Packages pour le téléchargement du découpage géographique
tel = c('httr','utils','xlsx',"dplyr")
options(java.parameters = "-Xmx3000m") # Setting xls ram availability
invisible(lapply(tel,function(pack){
  if(!c(pack %in% installed.packages()[,'Package'])){
    install.packages(pack) 
  } 
  do.call("require", list(pack)) 
}))

## Package utilisation API INSEE
if(!("inseeLocalData" %in% installed.packages()[,'Package'])){
  remotes::install_github("inseefrlab/inseeLocalData")
}
library(inseeLocalData)


# 01. TELECHARGEMENT BASE COMMUNES ----------------------------------------
# Cette partie permet d'obtenir les numéros INSEE des communes de Biovallée pour utiliser l'API Insee


# Lien au 05.07.2021
url = "https://www.insee.fr/fr/statistiques/fichier/2028028/table-appartenance-geo-communes-21.zip" 

# Téléchargement si pas déjà dans le dossier
if(!("communes.zip" %in% list.files(telechargements))){
  httr::GET(
    url = url,
    httr::write_disk(file.path(telechargements,"communes.zip"))
  )
  
  # Dezippage du dossier
  unzip(file.path(telechargements,"communes.zip"),
        exdir = file.path(telechargements,'communes'))
}

url_epci = 'https://www.insee.fr/fr/statistiques/fichier/2510634/Intercommunalite_Metropole_au_01-01-2021.zip'
if(!("epci.zip" %in% list.files(telechargements))){
  httr::GET(
    url = url_epci,
    httr::write_disk(file.path(telechargements,"epci.zip"))
  )
  
  # Dezippage du dossier
  unzip(file.path(telechargements,"epci.zip"),
        exdir = file.path(telechargements,'epci'))
}


# Chargement des fichiers dans R
if(!("codeCommunesEPCI.RDS"%in%list.files(file.path(telechargements)))){
  # Chargement du fichier dans R
  fichier = list.files(file.path(telechargements,'communes'))[1]
  zonage_meta <- read.xlsx(file.path(telechargements,'communes',fichier),
                           sheetName="COM",
                           startRow = 6,
                           encoding = "UTF-8") 
  
  
  fichier_epci = list.files(file.path(telechargements,'epci'))[1]
  zonage_epci <- read.xlsx(file.path(telechargements,'epci',fichier_epci),
                           sheetName="EPCI",
                           startRow = 6,
                           encoding = "UTF-8") 
  
  zonage_final <- zonage_meta %>% 
    left_join(zonage_epci)
  
  # Sauvergarde au format RDS
  saveRDS(zonage_final, file.path(telechargements,"codeCommunesEPCI.RDS"))
} else {
  zonage_final <- readRDS(file.path(telechargements,"codeCommunesEPCI.RDS"))
}


# Restriction du fichier communes aux communes de la Biovallée
zonage_biovallee <- zonage_final %>% 
  filter(LIBEPCI %in% c("CC du Val de Drôme en Biovallée",
                        "CC du Diois",
                        "	CC du Crestois et de Pays de Saillans Cœur de Drôme"))




# 02. UTILISATION API INSEE ------------------------------------------------


