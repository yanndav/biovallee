#########################################
#                                       #
#     TELECHARGEMENTS DONNEES CARTO     #
#     . Yann DAVID . TI-Biovallée .     #
#             Juillet 2021              #
#                                       #
#########################################

# Le but de ce script est de télécharger et configurer les données cartographiques pour Biovallée



# 00. INSTALLATION PACKAGES -----------------------------------------------
# Initialisation dossiers
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
telechargements <- sub('/script','/data',getwd())


# Installation packages
## Packages pour le téléchargement du découpage géographique
tel = c('httr','utils','sf')
invisible(lapply(tel,function(pack){
  if(!c(pack %in% installed.packages()[,'Package'])){
    install.packages(pack) 
  } 
  do.call("require", list(pack)) 
}))


# 01. TELECHARGEMENT BASE COMMUNES ----------------------------------------
# Cette partie permet d'obtenir le shapefile du découpage administratif français

# Lien au 08.07.2021
url = "https://www.data.gouv.fr/fr/datasets/r/989a9b36-e12a-4d28-a9b9-8eaf3955e8d3"



# Téléchargement si pas déjà dans le dossier
if(!("admin-shapefile.zip" %in% list.files(telechargements))){
  httr::GET(
    url = url,
    httr::write_disk(file.path(telechargements,"admin-shapefile.7z"))
  )
  
  # Dezippage du dossier
  
  z7path = shQuote('C:\\Program Files\\7-Zip\\7z')
  file = file.path(telechargements,"admin-shapefile.7z")
  cmd = paste0(z7path, ' e ', file, ' -y -o', file.path(telechargements,"admin-shapefile"), 
               '/')
  shell(cmd)
  
  
  
}



# Chargement des fichiers dans R
if(!("geocommune.RDS"%in%list.files(file.path(telechargements)))){
  # Chargement du fichier dans R
  fichier = file.path(telechargements,'admin-shapefile/COMMUNE.shp')
  geocommune <- st_read(fichier) 
  
  # Sauvergarde au format RDS
  saveRDS(geocommune, file.path(telechargements,"geocommune.RDS"))
  
  geocommune_drome = geocommune %>% 
    filter(INSEE_DEP==26)
  
  
  saveRDS(geocommune_drome, file.path(telechargements,"geocommune_drome.RDS"))
  
  
  
} else {
  geocommune <- readRDS(file.path(telechargements,"geocommune.RDS"))
  geocommune_drome <- readRDS(file.path(telechargements,"geocommune_drome.RDS"))
  
  
}




# TELECHARGEMENT TOPOGRAPHIE IGN ------------------------------------------


url2 = "http://data.cquest.org/ign/bdtopo/BDTOPO_3-0_2021-03-15/BDTOPO_3-0_TOUSTHEMES_SHP_LAMB93_D026_2021-03-15.7z"



# Téléchargement si pas déjà dans le dossier
if(!("topo.7z" %in% list.files(telechargements))){
  httr::GET(
    url = url2,
    httr::write_disk(file.path(telechargements,"topo.7z"))
  )
  
  # Dezippage du dossier
  
  z7path = shQuote('C:\\Program Files\\7-Zip\\7z')
  file = file.path(telechargements,"topo.7z")
  cmd = paste0(z7path, ' e ', file, ' -y -o', file.path(telechargements,"topo"), 
               '/')
  shell(cmd)
  
}




# Importation de certaines données topographiques
topo = file.path(telechargements,"topo")

eau  = st_read(file.path(topo,"COURS_D_EAU.shp")) %>% 
  filter(str_detect(TOPONYME,"la Drôme|le Rhône")) %>% 
  mutate(courseau = case_when(str_detect(TOPONYME,"la Drôme")~"drome",
                           str_detect(TOPONYME,"le Rhône")~"rhone",
                           T ~ TOPONYME))

departement  = st_read(file.path(topo,"DEPARTEMENT.shp")) %>% 
  filter(INSEE_DEP==26)
eau_drome  = st_crop(eau, extent(departement))
saveRDS(eau_drome,file = file.path(telechargements,"eau_drome.RDS"))




# TELECHARGEMENT ALTIMETRIE IGN -------------------------------------------


url3 = "http://data.cquest.org/ign/bdalti/BDALTIV2_2-0_250M_ASC_LAMB93-IGN69_FRANCE_2018-01-15.7z"

# Téléchargement si pas déjà dans le dossier
if(!("alti250.7z" %in% list.files(telechargements))){
  httr::GET(
    url = url3,
    httr::write_disk(file.path(telechargements,"alti250.7z"))
  )
  
  # Dezippage du dossier
  
  z7path = shQuote('C:\\Program Files\\7-Zip\\7z')
  file = file.path(telechargements,"alti250.7z")
  cmd = paste0(z7path, ' e ', file, ' -y -o', file.path(telechargements,"alti250"), 
               '/')
  shell(cmd)
  
}

# Chargement du fichier français
file = "BDALTIV2_250M_FXX_0098_7150_MNT_LAMB93_IGN69.asc"
alti = raster(file.path(telechargements,'alti250',file))

# Découpage à l'échelle départementale
departement  = st_read(file.path(telechargements,"topo/DEPARTEMENT.shp")) %>% 
  filter(INSEE_DEP==26)

alti_drome <- crop(alti, extent(departement))
alti_dromepts = rasterToPoints(alti_drome, spatial = TRUE)
alti_drome_df  <- data.frame(alti_dromepts)
colnames(alti_drome_df)[1]="height"

saveRDS(alti_drome_df,file = file.path(telechargements,"alti_drome.RDS"))



# Découpage à l'échelle biovallee
biovallee_c = as_Spatial(st_union(st_read(file.path(topo,"EPCI.shp")) %>% 
                       filter(NOM %in% zonage_biovallee$LIBEPCI)))


alti_biovallee <- crop(alti, extent(biovallee_c))
alti_biovallee_df = data.frame(rasterToPoints(alti_biovallee, spatial = TRUE))
colnames(alti_biovallee_df)[1]="height"

saveRDS(alti_biovallee_df,file = file.path(telechargements,"alti_biovallee_df.RDS"))
