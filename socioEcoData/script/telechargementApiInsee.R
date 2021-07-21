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
tel = c('httr','utils','xlsx',"tidyverse","progress")
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
                        "CC du Crestois et de Pays de Saillans Cœur de Drôme"))




# 02. UTILISATION API INSEE ------------------------------------------------

# Token sauvegardé dans un fichier local
token = readLines(file("tokenInsee.txt", open="r"),n=1)


# Nom des bases de données utilisées:
db_insee <- data.frame("annee"=c(2019:2006),
                       "recensement"=c(rep(NA,2),
                                       "GEO2020RP2017",	
                                       "GEO2019RP2016",	
                                       "RP2015",
                                       "RP2014",
                                       "RP2013",
                                       "RP2012",
                                       "RP2011",
                                       "RP2010",
                                       "RP2009",
                                       "RP2008",
                                       "RP2007",
                                       "RP2006"
                                       ),
                       "population" = c(NA,
                                        "POPLEG2018",	
                                        "POPLEG2017",
                                        "POPLEG2016",
                                        "POPLEG2015",
                                        "POPLEG2014",
                                        "POPLEG2013",
                                        "POPLEG2012",
                                        "POPLEG2011",
                                        "POPLEG2010",
                                        "POPLEG2009",	
                                        "POPLEG2008",	
                                        "POPLEG2007",
                                        "POPLEG2006"),
                       "filosofi" = c(NA,
                                      "GEO2020FILO2018",
                                      "GEO2020FILO2017",
                                      rep(NA,11)),
                       "flores"=c(NA,NA,
                                  "GEO2020FLORES2017",
                                  rep(NA,11)),
                       "entreprises"=c("GEO2020REE2019",
                                       "GEO2019REE2018",
                                       "GEO2017REE2017",
                                       "REE2016",
                                       "REE2015",
                                       "REE2014",
                                       "REE2013",
                                       "REE2012",
                                       "REE2011",
                                       "REE2010",
                                       'REE2009',
                                       rep(NA,3)
                                       )
                       )


# Nom des zones géographiques d'intérêt
geo_insee = data.frame("codegeo"=c(zonage_biovallee$CODGEO,26,84),
                       "nivgeo"=c(rep("COM",length(zonage_biovallee$CODGEO)),"DEP","REG"))



# Nom des variables d'intérêt:
variables = setNames(
  as.data.frame(rbind(
    c("LOG G2","Ancienneté d'emménagement des ménages","recensement"),
    c("POP T4","Lieu de résidence 1 an auparavant","recensement"),
    c("IND_POPLEGALES","Populations légales","population"),
    c("SEXE-AGE15_15_90","Population par sexe et âge","recensement"),
    c("INDICS_FILO_DISP",	"Ménages fiscaux","filosofi"),
    c("INDICS_FILO_DISP_DET-TRAGERF", "Taux de pauvreté par tranche d'âge du référent fiscal","filosofi"),
    c("INDICS_FILO_DISP_DET-OCCTYPR",	"Taux de pauvreté par statut d'occupation du logement du référent fiscal","filosofi"),
    c("INDICS_FILO_DEC_DET","Décomposition des revenus disponibles","filosofi"),
    c("INDICS_FILO_DISP_DET","Distribution des revenus disponibles","filosofi"),
    c("EFFECSAL5T_0_50P","Établissements actifs employeurs par secteur d'activité agrégé et taille","flores"),
    c("EFFECSAL5T_1_100P","Postes salariés par secteur d'activité agrégé et taille d'établissement","flores"),
    c("SPHEREECO-DOMPUB","Établissements actifs employeurs selon les sphères de l'économie","flores"),
    c("NA17", "Établissements actifs employeurs et postes salariés par secteur d'activité détaillé (A17)","flores"),
    c("INDICS_FLORES_AUTRES","Particuliers employeurs","flores"),
    c("CS1_6",	"Emplois par catégorie socioprofessionnelle","recensement"),
    c("IMMI-CS1_8-SEXE", "Population par sexe, situation quant à l'immigration et catégorie socioprofessionnelle",'recensement'),
    c("STATR-AGEQ20_65P-TP-SEXE", "Emplois au lieu de travail par sexe, temps de travail, âge quinquennal et statut regroupé","recensement"),
    c("TACTR_2-SEXE-AGE3", "Activité et emploi de la population de 15 à 64 ans par sexe et âge","recensement"),
    c("TP-EMPL-SEXE", "Emplois au lieu de travail par sexe, condition d'emploi et temps de travail","recensement"),
    c("STATR-NA17-SEXE","Emplois selon le secteur d'activité","recensement"),
    c("NA10_HORS_AZ",	"Nombre d'entreprises par secteur d'activité au 31 décembre","entreprises"),
    c("TYPLR-CATL",	"Catégories et types de logements","recensement"),
    c("ANEMR2", "Ancienneté d'emménagement des ménages","recensement"),
    c("STOCD",	"Résidences Principales selon le statut d'occupation","recensement"),
    c("VOIT",	"Équipement automobile des ménages","recensement"),
    c("TRANS_19",	"Part des moyens de transport utilisés pour se rendre au travail","recensement")
    )),
  c("code","label","source"))
  





# Fonction de téléchargement des données INSEE

# Chargement, création ou utilisation de la liste de résultats
if(!exists('inseeApiData')){
  if("inseeApiData.RData" %in% list.files(file.path(telechargements))){
    load(file.path(telechargements,"inseeApiData.RData"))
  }else{
    inseeApiData = list()
    save(inseeApiData, file = file.path(telechargements,"inseeApiData.RData"))
  }
  
} 




error = c()


for(v in 1:nrow(variables)){
  iter = Sys.time()
  # --- Initialisation des paramètres de départ --- #
  
  
  # Assignation des valeurs de départ
  croisement = variables$code[v] # Définition du croisement
  modalite <- paste(rep("all",str_count(variables$code[v],"-")+1),collapse = ".")
  print(croisement)
  
  # Dimensions de la variable
  bases = db_insee[[variables$source[v]]]
  annees = db_insee$annee[!is.na(bases)]
  localisations = geo_insee$codegeo
  nivsgeo = geo_insee$nivgeo
  
  # Dimensions potentielles
  dim_explore = data.frame("annee"=rep(as.character(annees),length(localisations)),
                       "codgeo"=rep(localisations,each=length(annees)),
                       "nivgeo"=rep(nivsgeo,each=length(annees))
                       )
  
  
  # --- Comparaison des paramètres possibles et existants --- #
  if(!(croisement %in% names(inseeApiData))){ # Si pas encore téléchargé
    
    # Initialisation de la liste
    inseeApiData[[croisement]] = list()
    
    
  } else { # Sinon on va aller chercher les infos pas encore chargées
    
    # Dimensions déja chargées
    dim_charge =  as.data.frame(inseeApiData[[croisement]][["data"]]) %>% 
      select(annee, codgeo, nivgeo)
    
    # Dimensions à explorer:
    
    dim_explore = anti_join(dim_explore,dim_charge)
    
  }
  
  # --- Initialisation de la barre de progression --- #
  n_iter =  nrow(dim_explore)
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Temps écoulé: :elapsedfull || Temps restant estimé: :eta]",
                         total = n_iter,
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)      # Width of the progress bar
  
  
  
  # --- Téléchargement des données non encore téléchargés --- #
  # A présent on va se balader à travers les dimensions à explorer
    
    for(year in unique(dim_explore$annee)){
      # Initialisation vecteur localisations pour l'année
      explore = which(dim_explore$annee==year)
      
      # Initialisation du jeu de données de l'année
      jeu_donnees <- db_insee[[variables$source[v]]][db_insee$annee==year]
      
      
      # Si l'année est bien dispo dans l'API:
      if(!is.na(jeu_donnees)){
        # print(year)
        
        # Pour chaque niveau géographique désiré:
        for(g in explore){
          pb$tick()
          
          # Initialisation des niveaux géographiques
          nivgeo <- dim_explore$nivgeo[g]
          codgeo <-  dim_explore$codgeo[g]
          
          # print(codgeo)
          
          # Tant que sous les deux secondes, attendre
          # (pas plus de 30 requêtes par minute sur l'API)
          while (Sys.time()- iter <2) {
            Sys.sleep(1)
          }
          
          # Sauvegarde de l'horaire de nouvelle requête
          iter = Sys.time()
          
          # Résultats de la requête
       
          
           res <- tryCatch({ quietly(get_dataset)(token,
                                       jeu_donnees, 
                                       croisement, 
                                       modalite, 
                                       nivgeo, 
                                       codgeo)$result
             
             },
                          error = function(e) {
                            return('Erreur - Aucune cellule ne correspond à la requête')
                          },
                          warning = function(w){
                            return(as.character(w))
                          },
                         message = function(m){
                         
                         }
                          )
          
          # Vérification pas d'erreur
          if(!(res %in% c("Erreur - Paramètre(s) de la requête incorrect(s)",
                    "Erreur - Aucune cellule ne correspond à la requête")) ){
            
            # Création de la base de données temporaire
            data_temp = res$donnees %>% # pour accéder aux données
              mutate(annee = res$source$millesime_donnees,# pour accéder à la source
                     libgeo = res$info_zone$libelle_sans_article, # pour accéder aux données géographiques
                     anneegeo = res$info_zone$millesime_geo) 
            
            
        
                # Si première itération, alors assignation des valeurs de métadonnées
                if(length(inseeApiData[[croisement]])==0){
                  inseeApiData[[croisement]]["meta"] = list(res$liste_code)
                  inseeApiData[[croisement]]["label"] = variables$label[v]
                  inseeApiData[[croisement]]["data"] = list(data_temp)
                  
                } else {
                  # Insertion verticale à la base de données de la variable
                  inseeApiData[[croisement]]["data"] <-list(rbind(
                    as.data.frame(inseeApiData[[croisement]][["data"]]),
                    data_temp
                  )
                  )
                }
            
              
            } else { # S'il y a une erreur
              # Sauvegarde des paramètres causant une erreur
              
              error = c(error,
                        paste(jeu_donnees,
                              croisement,
                              modalite,
                              nivgeo,
                              codgeo))
              
            }
            
          }
         
          
        
        # Sauvegarde à chaque année réalisée
        save(inseeApiData, file = file.path(telechargements,"inseeApiData.RData"))
      }
      
    }
    
  }


