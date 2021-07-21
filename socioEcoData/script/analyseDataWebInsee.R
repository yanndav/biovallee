#########################################
#                                       #
#         ANALYSE DONNEES INSEE         #
#     . Yann DAVID . TI-Biovallée .     #
#             Juillet 2021              #
#                                       #
#########################################


# 00. INSTALLATION PACKAGES -----------------------------------------------
# Initialisation dossiers
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
telechargements <- sub('/script','/data',getwd())
data_clean <- sub('/script','/data/insee_aura_clean',getwd())


# Installation packages
## Packages pour l'analyse statistique et geospatiale
tel = c("tidyverse","sf","raster","rgdal","ggnewscale")
invisible(lapply(tel,function(pack){
  if(!c(pack %in% installed.packages()[,'Package'])){
    install.packages(pack) 
  } 
  do.call("require", list(pack)) 
}))



# 01. CARTOGRAPHIE DE LA BIOVALLEE ----------------------------------------
# Chargement des fonctions
source("fonctionsWebCarto.R",encoding = "UTF-8")

# Cartographie géographie département
echelle = "departement"
ggplot()+
  Altitude(echelle = echelle)+
  Departement()+
  Biovallee()+
  Epci(size=1,text=F,fill=T,alpha=0.3)+
  
  Cours_eau(cours = c("drome","rhone"),
            echelle=echelle)+
  Theme()+
  theme(legend.position="bottom",
        legend.direction = "vertical")


# Cartographie géographie biovallée

echelle = "biovallee"
ggplot()+
  Altitude(echelle = echelle)+
  # Departement()+
  Biovallee()+
  Epci(size=1,text=F,fill=T,alpha=0.3)+
  
  Cours_eau(cours = c("drome","rhone"),
            echelle=echelle)+
  Communes(echelle=echelle,
           size=0.8,
           alpha=1,
           line="dashed")+
  Theme()+
  theme(legend.position="bottom",
        legend.direction = "vertical")


# 02. CARTOGRAPHIE DONNEES INSEE ------------------------------------------

# Liste des fichiers
liste = list.files(path = data_clean )

# Chargement des données
insee_data = list()
insee_data = lapply(liste, function(x){
  list("data"=readRDS(file.path(data_clean,x)))
})
names(insee_data) = sapply(liste, function(x) str_remove_all(x,"data_|.RDS"))


names(insee_data) 
VariablesDansBase("logements")


DimensionsDispo("logements","MESURE","MEN")


nom_base = "logements"
echelle = "drome"
echelle_operateur = "sum"
facet = list("EMMENAGEMENT"="all")
mono_dim = list("MESURE"="MEN")



### Evolution temporelle pour une dimension

# Echelle spatiale désirée
if(echelle=="drome"|echelle=="departement"){
  dat = insee_data[[nom_base]][["data"]]
  communes = geocommune_reg %>% 
    filter(INSEE_DEP==26)
  base = dat %>% 
    filter(CODGEO %in% communes$INSEE_COM) %>% 
    group_by_at(setdiff(names(dat), c("CODGEO","valeur"))) %>% 
    mutate(valeur=as.numeric(valeur))
}  

# Opération de regroupement à cette échelle
if(echelle_operateur=="sum"){
  base = base %>% 
    summarise(valeur = sum(valeur, na.rm=T))
}else if(echelle_operateur=="mean"){
  base = base %>% 
    summarise(valeur = mean(valeur, na.rm=T))
}else{
  print("error no grouping operator")
  break 
}

# Conservation des dimensions de la facet
if(facet[[1]][1]!="all"){
  # Si seulement quelques modalités conservées, on les filtre
  base = base %>%  
    filter((!!as.symbol(names(facet)[1])) %in% facet[[1]])
  
}else{
  # Si toutes les modalités conservées, on enlève celle d'ensemble
  base = base %>%  
    filter((!!as.symbol(names(facet)[1])) != "ENS")

  }
 
# Application du subset
for(variable in names(mono_dim)){
    # Si toutes les modalités conservées, on enlève celle d'ensemble
  base = base %>%  
      filter((!!as.symbol(variable)) == mono_dim[[variable]])
}
  
  dim_reelle = nrow(base[base[[names(facet)[1]]]==facet[[1]][1],])
  dim_annee = length(unique(base$annee))
  
  if(dim_reelle != dim_annee){
    cat('Problème de dimension: il y a plus de dimensions ville/année/variable/mesure que possible, revoyez les dimensions dans mono_dim')
    break
  }



ggplot()+
  geom_line(data = base,
            aes_string(x="annee",
                y="valeur",
                group= names(facet)[1],
                color = names(facet)[1]))








temp = (insee_data[[nom_base]][["data"]])


















# Nom des variables
names(inseeApiData) # 24 en tout

variable = "IND_POPLEGALES"
label = inseeApiData[[variable]]$label
data = inseeApiData[[variable]]$data
data_epci = data %>% left_join(zonage_biovallee, by=c("codgeo"="CODGEO"))

# Nombre total biovallee par année et par catégorie
title = paste("Evolution des", str_to_lower(label),"dans Biovallée par année")
colnames(data)
data_cum_annee = data %>% 
  filter(nivgeo=="COM") %>%
  dplyr::select(annee, valeur, lib_mesure) %>% 
  group_by(annee,lib_mesure) %>% 
  summarise(sum = sum(as.numeric(valeur)))

ggplot(data_cum_annee) +
  geom_line(aes(x = annee,
                y= sum,
                group = lib_mesure,
                color = lib_mesure))+
  labs(x="Année",
       y=label,
       color="Catégories",
       title=title)+
  theme_light()+
  theme(legend.position = "bottom")


# Nombre total par EPCI par année et par catégorie
title = paste("Evolution des", str_to_lower(label),"par EPCI par année")
data_cum_epci_annee = data_epci %>% 
  filter(nivgeo=="COM") %>%
  dplyr::select(annee, valeur, lib_mesure,LIBEPCI) %>% 
  group_by(annee,lib_mesure,LIBEPCI) %>% 
  summarise(sum = sum(as.numeric(valeur)))

ggplot(data_cum_epci_annee) +
  geom_line(aes(x = annee,
                y= sum,
                group = lib_mesure,
                color = lib_mesure))+
  labs(x="Année",
       y=label,
       color="Catégories",
       title = title)+
  facet_grid(.~stringr::str_wrap(LIBEPCI, 30) )+
  theme_light()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=65, vjust=0.5))+
  scale_color_npg()


# Cartographie géographique sur une année
view(Bases_insee())# Obtenir le nom des variables INSEE


#### Creation fonction formattage graph

base = "INDICS_FILO_DISP_DET" 
View(Modalite_insee(base))
annee_ = 2017
facet = list("mesure"=c("PPFAM"))
mono_dim = NULL
# mono_dim = list("INDICS_FILO_DISP_DET"="1")

ggplot()+
CommunesInsee(base,annee_,facet,mono_dim)+
  Theme()+
  theme(legend.position="bottom",
        legend.direction = "horizontal")
  


 
# variable = "IND_POPLEGALES"
# label = inseeApiData[[variable]]$label
# data = inseeApiData[[variable]]$data 
# data_epci = data %>% left_join(zonage_biovallee, by=c("codgeo"="CODGEO"))
# 
# 
# #Nombre de communes
# nb_com = nrow(unique( data %>% filter(nivgeo=="COM") %>% select(libgeo)))
# 
# # Departement et région incluses?
# 
# dep = nrow(unique( data %>% filter(nivgeo=="DEP") %>% select(libgeo)))==1
# reg = nrow(unique( data %>% filter(nivgeo=="REG") %>% select(libgeo)))==1

