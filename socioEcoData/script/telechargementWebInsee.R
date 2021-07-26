#########################################
#                                       #
#     TELECHARGEMENTS DONNEES INSEE     #
#     . Yann DAVID . TI-Biovallée .     #
#             Juillet 2021              #
#                                       #
#########################################

# Le but de ce script est de télécharger les données données pour la France


# 00. INSTALLATION PACKAGES -----------------------------------------------
# Initialisation dossiers
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- sub('/script','/data',getwd())
if(!file.exists(paste0(data,"/insee"))){
  dir.create(paste0(data,"/insee"))
}
data_insee = paste0(data,"/insee")

# Installation packages
## Packages pour le téléchargement du découpage géographique
tel = c('httr','utils','sf','rvest','tidyverse','readxl')
invisible(lapply(tel,function(pack){
  if(!c(pack %in% installed.packages()[,'Package'])){
    install.packages(pack) 
  } 
  do.call("require", list(pack)) 
}))


# 01. INDEXATION DES DIFFERENTS FICHIERS DISPONIBLES ----------------------------------------
# Cette partie permet de charger les liens des différents bases disponibles


# Lien au 12.07.2021
url = "https://www.insee.fr/fr/information/2880845" # Adresse de départ
racine = "https://www.insee.fr"

# Récupération des liens pour chaque année
liens_annees =  read_html(url) %>% 
  html_elements(css = "#consulter > div > ul > li > a") %>% 
  html_attr("href")

# Ajout 2018 manuel
l2018 = "/fr/information/5369871"

liens_annees = c(liens_annees,l2018)

interet = c("Mobilités professionnelles des individus : déplacements commune de résidence / commune de travail",
            "Migrations résidentielles : localisation à la commune de résidence et à la commune de résidence antérieure")


interet_general = c("Évolution et structure de la population - Migrations résidentielles",
                    "Évolution et structure de la population"   ,
                    "Logements - Migrations résidentielles"  ,               
                    "Couples - Familles - Ménages",
                    "Logements",
                    "Diplômes - Formation - Mobilités scolaires",
                    "Diplôme - Formation - Mobilités scolaires",
                    "Population active - Emploi - Chômage",
                    "Caractéristiques de l'emploi - Mobilités professionnelles",
                    "Caractéristique de l'emploi - Mobilités professionnelles")


# 02. TELECHARGEMENT DES FICHIERS -----------------------------------------

## Fonction de téléchargement et d'import/export du fichier dans R
lien = paste0(racine,link)
telechargementBase <- function(lien,nom_element){
  extension = str_extract(lien, "\\.[A-Za-z]+$")
  
  
  if(!(paste0(nom_element,extension) %in% list.files(data_insee))){
    httr::GET(
      url = lien,
      httr::write_disk(file.path(data_insee,paste0(nom_element,extension)))
    )
    
    if(extension==".zip"){
      unzip(file.path(data_insee,paste0(nom_element,".zip")),
            exdir = file.path(data_insee,nom_element))
      
      files  = list.files(file.path(data_insee,nom_element),full.names = T)
      
      size = order(sapply(files, file.size),
                   decreasing = T)
      
      
      # Sauvergarde au format R pour chargement futur plus rapide
      to_open = files[size==1]
    }else{
      to_open = file.path(data_insee,paste0(nom_element,extension))
    }
    
    if(str_detect(str_to_lower(to_open),"\\.csv$")){
      data_base = read.csv2(to_open) 
    }else if (str_detect(str_to_lower(to_open),"\\.xls$")){
      print(to_open)
      temp = read_xls(to_open,
                           sheet = 1)
      
      # Détection de la ligne de début
      start = which(sapply(temp[,1],function(vect) str_detect(vect,"^[A-Z]+$")))[1]
      data_base = temp[start+1:nrow(temp),]
      colnames(data_base) = temp[start,]
      
      
    }else if (str_detect(str_to_lower(to_open),"\\.txt$")){
      print(to_open)
      data_base = read_delim(to_open, delim=";")
    }else{
        print("problem extension file")
      }
    
    
    saveRDS(data_base,file = file.path(data_insee,paste0(nom_element,'.RDS')))
    
  }
  
}


# Téléchargement automatisé des différents fichiers:
# Accès page de données :

for(an in 1:length(liens_annees)){
  print(liens_annees[an])
  
  
  page_annee = read_html(paste0(racine,liens_annees[an]))
  annee = str_extract(page_annee %>% 
                        html_elements(css = " title") %>% 
                        html_text(),"\\d+")
  
  # Téléchargement des données détaillées (flux)
  
  sections = page_annee %>% 
    html_elements(css="#consulter > div > div") 
  
  lien_detailles = sections[which(sections %>% html_text2() =="Pour accéder à l'ensemble des fichiers détail, cliquez ici.")] %>% 
    html_children() %>% 
    html_element("a") %>% 
    html_attr("href")
  
  
  
  page_detailles = read_html(paste0(racine,lien_detailles))
  
  fichiers_dispos = str_replace_all(str_replace_all(page_detailles %>%
    html_elements("#consulter-sommaire > nav > ul > li > ul > li > a") %>% 
    html_text(),"\n",""),"  +"," ")
  
  select = which(fichiers_dispos %in% interet)
  
  for(page in select){
    lien =  page_detailles %>%
      html_elements("#consulter-sommaire > nav > ul > li > ul > li > a") %>% 
      .[[page]] %>% 
      html_attr("href")
    
    page_temp = read_html(paste0(racine,lien)) 
    
    categ = str_replace_all(str_replace_all(page_temp %>% 
      html_elements("#consulter > div > div:nth-child(4) > div:nth-child(1) > div > span:nth-child(2)") %>% 
      html_text(),"\n|:|/",""),"  +"," ")
    
    print(categ)
    
    liens = page_temp %>% 
      html_elements("#consulter > div > div:nth-child(4) > a") 
    
    link = liens[which(str_detect(liens %>% html_text2(),"csv"))] %>% 
      html_attr("href")
    
    if(identical(link,character(0))){
      link = liens[which(str_detect(liens %>% html_text2(),"txt"))] %>% 
        html_attr("href")
          }
    
    nom_dossier = str_replace_all(paste(annee,str_to_lower(categ),sep = ".")," ","_")
    
    telechargementBase(paste0(racine,link),nom_dossier)
    
    
    
  }
  
  
  # Téléchargement des données communales, principaux indicateurs
  
  tableau = page_annee %>% 
    html_element(css="#produit-tableau-Feuil3 tbody")
  
  # Sélection des variables d'intérêt
  
  selection = which(tableau %>% html_elements("th") %>% html_text() %in% interet_general)
  
  for(k in selection){
    
    
    nom_categ = tableau %>% 
      html_children() %>% 
      .[[k]] %>%
      html_elements(css="th") %>% 
      html_text()
    
    print(nom_categ)
    
    
    lien_categ = tableau %>% 
      html_children() %>% 
      .[[k]] %>%
      html_elements(css="td") %>%
      .[[1]] %>% 
      html_element(css = "a") %>%
      html_attr("href")
    
    
    
    ## Accès données catégorie
    page_categ = read_html(paste0(racine,lien_categ))
    lien_fichier = page_categ %>% 
      html_elements("#consulter > div > div:nth-child(4) > a")%>%
      html_attr("href") 
    
    
    links = page_categ %>% 
      html_elements("#consulter > div > div:nth-child(4) > a ") 
    
    link = links[which(str_detect(links %>% html_text2(),"csv"))] %>% 
      html_attr("href")
    
    if(identical(link,character(0))){
      link = links[which(str_detect(links %>% html_text2(),"xls"))] %>% 
        html_attr("href")
    }
    if(identical(link,character(0))){
      link = links[which(str_detect(links %>% html_text2(),"zip"))] %>% 
        html_attr("href")
    }
    
    nom_dossier = str_replace_all(paste(annee,str_to_lower(nom_categ),sep = ".")," ","_")
    
    
    telechargementBase(paste0(racine,link),nom_dossier)
    
  }
}
  

# Probleme de conversion avec read_csv2 -> donc patch
# fichiers_source = list.files(data_insee)
# fichiers_source = fichiers_source[!(fichiers_source %>% str_detect(.,".RDS|.zip"))]
# 
# 
# fichiers_csv = sapply(fichiers_source, function(dossier){
#   tempo = list.files(file.path(data_insee,dossier),
#                      full.names = T)
#   if(TRUE %in% (tempo %>% str_detect(.,".CSV|.csv"))){
#     return(tempo[which(tempo %>% str_detect(.,".CSV|.csv"))])
#   }
# })

# 
# for (dossier in names(fichiers_csv)) {
#   if(!is.null(fichiers_csv[[dossier]])){
#     files = fichiers_csv[[dossier]]
#     
#     size = order(sapply(files, file.size),
#                  decreasing = T)
#     
#     
#     # Sauvergarde au format R pour chargement futur plus rapide
#     to_open = files[size==1]
#     
#     # Ouverture 
#     
#      data_base = read.csv2(to_open) 
# 
#      saveRDS(data_base,file = file.path(data_insee,paste0(dossier,'.RDS')))
#      
#     
#   }
#   
#   
# }
# 
# 
# 
# 
# 
