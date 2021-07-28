#########################################
#                                       #
#      MISE EN FORME DONNEES INSEE      #
#     . Yann DAVID . TI-Biovallée .     #
#             Juillet 2021              #
#                                       #
#########################################

# Le but de ce script est de filtrer les données INSEE pour la France aux échelles pertinentes pour Biovallée
# L'idée est également de créer des bases exploitables pour l'analyse de données

# 01. INITIALISATION ------------------------------------------------------


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- sub('/script','/data',getwd())
if(!file.exists(paste0(data,"/insee"))){
  dir.create(paste0(data,"/insee"))
}
data_insee = paste0(data,"/insee")
if(!file.path(data,"insee_aura")){
  dir.create(file.path(data,"insee_aura"))
}
data_aura = file.path(data,"insee_aura")
if(!file.path(data,"insee_aura_clean")){
  dir.create(file.path(data,"insee_aura_clean"))
}
data_clean = file.path(data,"insee_aura_clean")
# Installation packages
## Packages pour le téléchargement du découpage géographique
tel = c('tidyverse',"sf")
invisible(lapply(tel,function(pack){
  if(!c(pack %in% installed.packages()[,'Package'])){
    install.packages(pack) 
  } 
  do.call("require", list(pack)) 
}))



# 02. CHARGEMENT DONNEES --------------------------------------------------

# Chargement du cadrage géographique:
geocommune_reg <- readRDS(file.path(data,"geocommune.RDS")) %>% 
  filter(INSEE_REG==84)

saveRDS(geocommune_reg,file.path(telechargements,"geocommune_reg.RDS"))


nrow(geocommune_reg) # On garde les 4030 communes de l'AURA pour les stats desc

com_AURA = geocommune_reg$INSEE_COM
candidates = c("CODGEO","COMMUNE") # Nom potentiel des colonnes de code insee


# Chargement du nom des fichiers INSEE

all_files = list.files(data_insee)
to_load = all_files[str_detect(all_files,"RDS$")]
length(to_load)
# Vérification des fichiers dispos par année
df_files = data.frame("annee"=character(),
                      "fichier"=character(),
                      "extension"=character())

for(i in 1:length(to_load)){
  df_files = df_files %>% 
    rbind(unlist(str_split(to_load[i],"\\.")))
}
colnames(df_files) <- c("annee","fichier","extension")

table(df_files$annee,df_files$fichier)




## Utilisation des zones administratives et statistiques pour filtrer le jeu entier de données
## Création de fichiers au niveau AURA

for(i in 1:length(to_load)){
  if(!file.exists(file.path(data_aura,paste0("aura.",to_load[i])))){
    # Chargement du fichier
    temp = readRDS(file.path(data_insee,
                             to_load[i]))
    
    # Récupération du bon nom de colonne
    column = candidates[which(candidates %in% colnames(temp))]
    
    # Filtrage
    if(column=="COMMUNE"){
      temp = temp %>% 
        filter(!!as.symbol(column) %in% as.numeric(com_AURA) | 
              "DCLT" %in% as.numeric(com_AURA))
      
    }else{
      temp = temp %>% 
        filter(!!as.symbol(column) %in% as.numeric(com_AURA))
      
    }
    
    
    # Sauvegarde de la version filtrée
    saveRDS(object = temp, 
            file = file.path(data_aura,paste0("aura.",to_load[i])))
  }
}

####### CHARGEMENT DES BASES NON FLUX -------------------------------------
# 01 . CARACTERISTIQUES DE L'EMPLOI ET MOBILITE PROFESSIONNELLE -----------


var_statut_emploi = c("ACTOCC",
               "SAL",
               "NSAL")

conditions = c("INDEP",
               "EMPLOY",
               "AIDFAM",
               "EMPAID",
               "INTERIM",
               "CDD",
               "CDI")

bases_emplois  = c("caractéristiques_de_l'emploi_-_mobilités_professionnelles",
                   "caractéristique_de_l'emploi_-_mobilités_professionnelles")

data_emploi = data.frame("CODGEO"=character(),
                         "valeur"=character(), 
                         "annee"=character(),
                         "SEXE"=character(),
                         "AGE"=character(),
                         "TEMPS"=character(),
                         "CONDITION"=character(),
                         "STATUT" = character(),
                         "LIEU"=character())
i=1
for(i in 1:length(to_load)){
  file_name = paste0("aura.",to_load[i])
  split_name = unlist(str_split(file_name,"\\."))
  
  if(split_name[3] %in% bases_emplois){
    print(split_name[2])
    
    
    temp = readRDS(file = file.path(data_aura,file_name))
    
    
    c_start = max(which(str_detect(colnames(temp),"^[A-Z]+$"))) +1
    

    
    
    temp1 = temp %>% 
      # mutate_if(is.numeric,as.character, is.factor, as.character,is.double,as.character) %>% 
      
      pivot_longer(
        cols = c(c_start[1]:ncol(temp)),
        values_to= "valeur",
        names_to="variable",
        values_ptypes = list(valeur=character())) %>% 
      mutate(annee = ifelse(str_detect(variable,
                                       paste0("P",str_extract(split_name[2],"\\d{2}$"))),
                            split_name[2],
                            "else")) %>% 
      filter(annee!="else") %>% 
      mutate(variable_clean = str_remove(variable,
                                         paste0("P",str_extract(split_name[2],"\\d{2}$"),"_")),
             SEXE = case_when(str_detect(variable_clean,"^F|^H") ~
                                str_extract(variable_clean,"^[A-Z]"),
                              T ~ "ENS"),
             AGE = str_extract(variable_clean,"[0-9]+(?:[A-Z]*)"),
             TEMPS = ifelse(str_detect(variable_clean,"_TP$"),"TP","ENS"),
             STATUT  = ifelse(str_detect(variable_clean,paste(var_statut_emploi, collapse = "|")),
                                 str_extract(variable_clean,paste(var_statut_emploi, collapse = "|")),
                                 "ENS"),
             CONDITION  = ifelse(str_detect(variable_clean,paste(paste0("_",conditions,"$"), collapse = "|")),
                                 str_extract(variable_clean,paste(paste0(conditions,"$"), collapse = "|")),
                                 "ENS"),
             LIEU = ifelse(str_detect(variable_clean,"_ILT.+"),
                           str_extract(variable_clean,"ILT.+(?!_)"),
                           "ENS")) %>% 
      mutate(valeur = as.numeric(valeur)) %>% 
      dplyr::select(CODGEO, valeur, annee,STATUT, SEXE, AGE, TEMPS, CONDITION, LIEU)
    
    
    data_emploi <-  data_emploi %>% 
      rbind(temp1)
    
  }
  
}

meta_emploi = list(
  
  "annee" = list("nom"="année",
                 "legende"=list("2006"="2006", 
                                 "2007"="2007",
                                 "2008"="2008",
                                 "2009"="2009",
                                 "2010"="2010",
                                 "2011"="2011",
                                 "2012"="2012",
                                 "2013"="2013",
                                 "2014"="2014",
                                 "2015"="2015",
                                 "2016"="2016",
                                 "2017"="2017",
                                 "2018"="2018")),
  "STATUT"=list("nom"="statut professionel",
                "legende"=list("ACTOCC"="actif occupé",
                               "SAL"="salarié",
                               "NSAL"="non salarié" )),
  "SEXE" = list("nom"="sexe",
                "legende"=list("ENS"="tous sexes confondus",
                               "H"="hommes",
                               "F"="femmes")),
  "AGE" = list("nom"="groupe d'âge",
               "legende"=list("15P"="15 ans et plus", 
                              "1564"="15 à 64 ans",
                              "1524"="15 à 24 ans",
                              "2554"="25 à 54 ans",
                              "5564"="55 à 64 ans")),
  "TEMPS" = list("nom"="temps de travail",
                 "legende"=list("ENS"="tous temps de travail confondus",
                                "TP"="temps partiel")),
  "CONDITION"=list("nom"="catégorie d'actif",
                   "legende"=list("ENS"="toutes catégories d'actif confondus",
                                  "CDI"="actifs en CDI",    
                                  "CDD"="actifs en CDD",    
                                  "INTERIM"="actifs en interim",
                                  "EMPAID"="actifs en emploi aidé", 
                                  "INDEP"="actifs indépendants",  
                                  "EMPLOY"="actifs employés",
                                  "AIDFAM"="actifs percevants les aides familiales")),
  "LIEU"= list("nom"="lieu de travail",
               "legende"=list("ENS"="tous lieux de travail confondus",  
                              "ILT1"="travail dans commune de résidence",  
                              "ILT2P"="travail dans autre commune que celle de résidence", 
                              "ILT2"="travail dans autre commune que celle de résidence, même département",
                              "ILT3"="travail dans autre département, même région",  
                              "ILT4"="travail dans autre région de métropole",  
                              "ILT5"="travail hors métropole",  
                              "ILT45D"="travail hors de la région de résidence"))
  )

base_emploi = list("data"=data_emploi,
                  "meta"=meta_emploi)
save(base_emploi,file = file.path(data_clean,"caracteristique_emploi_mobilite_professionnelle.RData"))


# 02. COUPLES - FAMILLES - MENAGES ----------------------------------------


bases_menage = c("couples_-_familles_-_ménages")

composition_menage  = c("HSEUL",
                        "FSEUL",
                        "PSEUL",
                        "FAMMONO",
                        "SFAM",
                        "FAM",
                        "COUPSENF",
                        "COUPAENF")

statut_indiv = c("PSEUL",
                 "COUPLE",
                 "MARIE",
                 "CELIB",
                 "VEUF",
                 "DIVOR")

composition_famille = c("COUPAENF",
                        "FAMMONO",
                        "HMONO",
                        "FMONO",
                        "COUPSENF",
                        "NE24F0",
                        "NE24F1",
                        "NE24F2",
                        "NE24F3",
                        "NE24F4P")


data_menage = data.frame("CODGEO"=character(),
                         "valeur"=character(), 
                         "annee"=character(),
                         "MESURE"=character(),
                         "AGE"=character(),
                         "CSP"=character(),
                         "MEN_COMPOSITION"=character(),
                         "POP_STATUT"=character(),
                         "FAM_COMPOSITION"=character())
i=2
for(i in 1:length(to_load)){
  file_name = paste0("aura.",to_load[i])
  split_name = unlist(str_split(file_name,"\\."))
  
  if(split_name[3] %in% bases_menage){
    print(split_name[2])
    
    
    temp = readRDS(file = file.path(data_aura,file_name))
    
    
    c_start = max(which(str_detect(colnames(temp),"^[A-Z]+$"))) +1
    
    temp1 = temp%>% 
      mutate_if(is.numeric,as.character, is.factor, as.character,is.double,as.character) %>% 
      
      pivot_longer(
        cols = c(c_start[1]:ncol(temp)),
        values_to= "valeur",
        names_to="variable",
        values_ptypes = list(valeur=character()))  %>% 
      mutate(annee = ifelse(str_detect(variable,
                                       paste0("(P|C)",str_extract(split_name[2],"\\d{2}$"))),
                            split_name[2],
                            "else")) %>% 
      filter(annee!="else") %>%  
      mutate(variable_clean = str_remove(variable,"(P|C)[0-9]{2}_"),
             EXPLOITATION = str_extract(variable,"^(P|C)"),
             MESURE = case_when(str_detect(variable_clean,"^MEN")~"MEN",
                                str_detect(variable_clean,"^POPMEN|^PMEN")~"PMEN",
                                str_detect(variable_clean,"^POP[0-9]")~"POP",
                                str_detect(variable_clean,"^(F|C|H|N)")~"FAM",
                                T~"ERREUR"),
             AGE = ifelse(str_detect(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          str_extract(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          "ENS"),
             CSP = ifelse(str_detect(variable_clean,"CS[0-9]"),
                          str_extract(variable_clean,"CS[0-9]"),
                          'ENS'),
             MEN_COMPOSITION = case_when((MESURE == "MEN" | MESURE == "PMEN") & str_detect(variable_clean,
                                                                                           paste(composition_menage,
                                                                                                 collapse = "|")) ~
                                           str_extract(variable_clean,
                                                       paste(composition_menage,
                                                             collapse = "|")),
                                         (MESURE == "MEN" | MESURE == "PMEN") & 
                                           !str_detect(variable_clean,
                                                       paste(composition_menage,
                                                             collapse = "|")) ~ "ENS",
                                         !(MESURE == "MEN" & MESURE == "PMEN") ~ NA_character_,
                                         T ~"ERREUR"),
             POP_STATUT =  case_when(MESURE == "POP" & str_detect(variable_clean,
                                                                  paste(statut_indiv,
                                                                        collapse = "|")) ~
                                       str_extract(variable_clean,
                                                   paste(statut_indiv,
                                                         collapse = "|")),
                                     MESURE == "POP" & 
                                       !str_detect(variable_clean,
                                                   paste(statut_indiv,
                                                         collapse = "|")) ~ "ENS",
                                     MESURE != "POP" ~ NA_character_,
                                     T ~"ERREUR"),
             FAM_COMPOSITION =  case_when(MESURE == "FAM" & str_detect(variable_clean,
                                                                       paste(composition_famille,
                                                                             collapse = "|")) ~
                                            str_extract(variable_clean,
                                                        paste(composition_famille,
                                                              collapse = "|")),
                                          MESURE == "FAM" & 
                                            !str_detect(variable_clean,
                                                        paste(composition_famille,
                                                              collapse = "|")) ~ "ENS",
                                          MESURE != "FAM" ~ NA_character_,
                                          T ~"ERREUR")
      )%>% 
      dplyr::select(CODGEO, valeur, annee, EXPLOITATION,
                    MESURE, AGE, CSP, MEN_COMPOSITION, POP_STATUT, FAM_COMPOSITION)
    
    data_menage <- data_menage %>% 
      rbind(temp1)
    
  }
  
}

data_menage = readRDS(file.path(data_clean,"couples_familles_menages.RDS"))

meta_menage = list(
  
  "annee" = list("nom"="année",
                 "legende"=list("2006"="2006", 
                                "2007"="2007",
                                "2008"="2008",
                                "2009"="2009",
                                "2010"="2010",
                                "2011"="2011",
                                "2012"="2012",
                                "2013"="2013",
                                "2014"="2014",
                                "2015"="2015",
                                "2016"="2016",
                                "2017"="2017",
                                "2018"="2018")),
  "EXPLOITATION"=list("nom"="exploitation",
                      "legende"=list("C"="complémentaire",
                                     "P"="principale")),
  "MESURE"= list("nom"="mesure",
                 "legende"=list("MEN"="ménages", 
                                "PMEN"="population des ménages",
                                "POP"="population",
                                "FAM"="familles" )),
  "AGE"=list('nom'="groupe d'âge",
             "legende"=list("ENS"= "tous groupes d'âge confondus",
                            "15P"= "15 ans et plus",
                            "1519"="15 à 19 ans",
                            "2024"="20 à 24 ans",
                            "2539"="25 à 39 ans",
                            "4054"="40 à 54 ans",
                            "5564"="55 à 64 ans",
                            "6579"="65 à 79 ans",
                            "80P"="80 ans et plus" )),
  "CSP"=list("nom"="catégorie socio-professionelle",
             "legende"=list("ENS"="toutes catégories socio-professionnelles confondus",
                            "CS1"="agriculteur exploitant",
                            "CS2"="artisan, commerçant, chef d'entreprise",
                            "CS3"="cadre, profession intellectuelle supérieure",
                            "CS4"="profession intermediaire",
                            "CS5"="employé",
                            "CS6"="ouvrier",
                            "CS7"="retraité",
                            "CS8"="autre sans activité professionnelle")
             ),
  "MEN_COMPOSITION"=list("nom"="composition du ménage",
                         "legende"=list(
                           "ENS"="tous types de ménage",     
                           "PSEUL"="personne seule",   
                           "HSEUL"="homme seul",   
                           "FSEUL"="femme seule",   
                           "SFAM"="sans famille",    
                           "FAM"="avec famille",     
                           "COUPSENF"="couple sans enfant(s)",
                           "COUPAENF"="couple avec enfant(s)",
                           "FAMMONO"="famille monoparentale"
                         )),
  "POP_STATUT"=list("nom"="composition de la population",
                    "legende"=list(
                      "ENS"="toutes compositions de population confondues",   
                      "PSEUL"="vivant seule", 
                      "COUPLE"="vivant en couple",
                      "MARIE"="marié", 
                      "CELIB"="célibataire", 
                      "VEUF"="veuf",  
                      "DIVOR"="divorcé" 
                    )),
  "FAM_COMPOSITION"=list("nom"="composition familiale",
                         "legende"=list(
                           "ENS"="toutes compositions familiales confondues",      
                           "COUPAENF"="couples avec enfants", 
                           "FAMMONO"="famille monoparentale",  
                           "HMONO"="famille monoparentale formée d'un homme seul",    
                           "FMONO"="famille monoparentale formée d'une femme seule",    
                           "COUPSENF"="couple sans enfant", 
                           "NE24F0"="famille sans enfant de moins de 25 ans",   
                           "NE24F1"="famille avec un enfant de moins de 25 ans",   
                           "NE24F2"="famille avec deux enfants de moins de 25 ans",  
                           "NE24F3"="famille avec trois enfants de moins de 25 ans",  
                           "NE24F4P"="famille avec 4 enfants ou plus de moins de 25 ans" 
                         ))
)

base_menage = list("data"=data_menage,
                   "meta"=meta_menage)



save(base_menage,file = file.path(data_clean,"couples_familles_ménages.RData"))




# 03. DIPLOME - FORMATION - MOBILITE SCOLAIRE -----------------------------
bases_diplome = c("diplômes_-_formation_-_mobilités_scolaires",
                  "diplôme_-_formation_-_mobilités_scolaires")
etude = c("DIPL0",
          "CEP",
          "BEPC",
          "CAPBEP",
          "BACP2",
          "BAC",
          "SUP2",
          "SUP34",
          "SUP5",
          "DIPLMIN",
          "SUP"
)



data_diplome = data.frame("CODGEO"=character(),
                          "valeur"=character(), 
                          "annee"=character(),
                          "SEXE"=character(),
                          "SCOLARITE"=character(),
                          "AGE"=character(),
                          "ETUDE"=character())

i=3

for(i in 1:length(to_load)){
  file_name = paste0("aura.",to_load[i])
  split_name = unlist(str_split(file_name,"\\."))
  
  if(split_name[3] %in% bases_diplome){
    print(split_name[2])
    
    
    temp = readRDS(file = file.path(data_aura,file_name))
    
    
    c_start = max(which(str_detect(colnames(temp),"^[A-Z]+$"))) +1
    
    temp1 = temp%>% 
      mutate_if(is.numeric,as.character, is.factor, as.character,is.double,as.character) %>% 
      
      pivot_longer(
        cols = c(c_start[1]:ncol(temp)),
        values_to= "valeur",
        names_to="variable",
        values_ptypes = list(valeur=character()))  %>% 
      mutate(annee = ifelse(str_detect(variable,
                                       paste0("(P|C)",str_extract(split_name[2],"\\d{2}$"))),
                            split_name[2],
                            "else")) %>% 
      filter(annee!="else") %>%  
      mutate(variable_clean = str_remove(variable,"(P|C)[0-9]{2}_"),
             EXPLOITATION  =  str_extract(variable,"^(P|C)"),
             SEXE = ifelse(str_detect(variable_clean,'^(F|H)'),
                           str_extract(variable_clean,'^(F|H)'),
                           "ENS"),
             SCOLARITE = ifelse(str_detect(variable_clean,"SCOL|NSCOL"),
                                str_extract(variable_clean,"SCOL|NSCOL"),
                                "ENS"),
             AGE = ifelse(str_detect(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          str_extract(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          "ENS"),
             ETUDE = ifelse(str_detect(variable_clean,paste(etude,collapse = "|")),
                            str_extract(variable_clean,paste(etude,collapse = "|")),
                            "ENS"))%>% 
      dplyr::select(CODGEO, valeur, annee,EXPLOITATION,SEXE,SCOLARITE,AGE,ETUDE)
    
    data_diplome = data_diplome %>% 
      rbind(temp1)
    
  }
}


meta_diplome = list(
  
  "annee" = list("nom"="année",
                 "legende"=list("2006"="2006", 
                                "2007"="2007",
                                "2008"="2008",
                                "2009"="2009",
                                "2010"="2010",
                                "2011"="2011",
                                "2012"="2012",
                                "2013"="2013",
                                "2014"="2014",
                                "2015"="2015",
                                "2016"="2016",
                                "2017"="2017",
                                "2018"="2018")),
  "EXPLOITATION"=list("nom"="exploitation",
                      "legende"=list("C"="complémentaire",
                                     "P"="principale")),
  "MESURE"=list("nom"="population"),
  "SEXE" = list("nom"="sexe",
                "legende"=list("ENS"="tous sexes confondus",
                               "H"="hommes",
                               "F"="femmes")),
  "SCOLARITE"=list("nom"="statut de scolarisation",
                   "legende"=list(
                     "ENS"="tous statuts de scolarisation confondus",  
                     "SCOL"="scolarisé", 
                     "NSCOL"="non scolarisé"
                   )),
  "AGE"= list("nom"="age",
              "legende"=list(
                "0205"="2 à 5 ans",
                "0610"="6 à 10 ans",
                "1114"="11 à 14 ans",
                "1517"="15 à 17 ans",
                "1824"="18 à 24 ans",
                "2529"="25 à 29 ans",
                "30P"="30 ans et plus",
                "15P"="15 ans et plus")),
  "ETUDE"=list("nom"="niveau de diplôme",
               "legende"=list(
                 "ENS"="tous niveaux de diplôme confondus",    
                 "DIPL0"="sans diplôme",  
                 "CEP"="diplômée du certificat d'études primaires",    
                 "BEPC"="diplômée du BEPC ou brevet des collèges",   
                 "CAPBEP"="diplômée du CAP ou BEP", 
                 "BAC"="diplômée du baccalauréat ou brevet professionnel",    
                 "BACP2"="diplômée de l'enseignement supérieur court",  
                 "SUP"="diplômeée de l'enseignement supérieur long",    
                 "DIPLMIN"="sans diplôme ou au plus un CEP",
                 "SUP2"="diplômée BAC+2"  ,
                 "SUP34"="diplômée BAC+3 ou BAC+4", 
                 "SUP5"="diplômée BAC+5 ou plus"  
               ))
)

base_diplome = list("data"=data_diplome,
                   "meta"=meta_diplome)



save(base_diplome,file = file.path(data_clean,"diplomes_formation.RData"))




# 04. EVOLUTION STRUCTURE POPULATION - LOGEMENT - MIGRATIONS --------------------------------------
# Ici, on a deux bases qui changent de nom à compter de 2017, mais pas d'organisation

bases_evolution = c("évolution_et_structure_de_la_population",
                    "évolution_et_structure_de_la_population_-_migrations_résidentielles")



data_structure = data.frame("CODGEO"=character(),
                            "valeur"=character(), 
                            "annee"=character(),
                            "SEXE"=character(),
                            "AGE"=character(),
                            "CSP"=character(),
                            "MOBILITE"=character())



i=4
for(i in 1:length(to_load)){
  file_name = paste0("aura.",to_load[i])
  split_name = unlist(str_split(file_name,"\\."))
  
  if(split_name[3] %in% bases_evolution){
    print(split_name[2])
    
    temp = readRDS(file = file.path(data_aura,file_name))

    c_start = max(which(str_detect(colnames(temp),"^[A-Z]+$"))) +1
    
    temp1 = temp%>% 
      mutate_if(is.numeric,as.character, is.factor, as.character,is.double,as.character) %>% 
      
      pivot_longer(
        cols = c(c_start[1]:ncol(temp)),
        values_to= "valeur",
        names_to="variable",
        values_ptypes = list(valeur=character()))  %>% 
      mutate(annee = ifelse(str_detect(variable,
                                       paste0("(P|C)",str_extract(split_name[2],"\\d{2}$"))),
                            split_name[2],
                            "else")) %>% 
      filter(annee!="else") %>%  
      mutate(variable_clean = str_remove(variable,"(P|C)[0-9]{2}_"),
             EXPLOITATION = str_extract(variable, '^(P|C)' ),
             SEXE = ifelse(str_detect(variable_clean,'^(F|H)|(POPH|POPF)'),
                           str_extract(variable_clean, "(?<=POP)(H|F)|(H|F)"),
                           "ENS"),
             AGE = ifelse(str_detect(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          str_extract(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          "ENS"),
             CSP = ifelse(str_detect(variable_clean,"CS[0-9]"),
                          str_extract(variable_clean,"CS[0-9]"),
                          'ENS'),
             MOBILITE = ifelse(str_detect(variable_clean,"IRAN[0-9](?:P|)"),
                               str_extract(variable_clean,"IRAN[0-9](?:P|)"),
                               'ENS'))%>% 
      dplyr::select(CODGEO, valeur,annee,EXPLOITATION,SEXE,AGE,CSP,MOBILITE)
    
    
    data_structure = data_structure %>% 
      rbind(temp1)
  }
}


data_structure = readRDS(file = file.path(data_clean,"evol_structure_pop.RDS"))
meta_structure= list(
  "annee" = list("nom"="année",
                 "legende"=list("2006"="2006", 
                                "2007"="2007",
                                "2008"="2008",
                                "2009"="2009",
                                "2010"="2010",
                                "2011"="2011",
                                "2012"="2012",
                                "2013"="2013",
                                "2014"="2014",
                                "2015"="2015",
                                "2016"="2016",
                                "2017"="2017",
                                "2018"="2018")),
  "EXPLOITATION"=list("nom"="exploitation",
                      "legende"=list("C"="complémentaire",
                                     "P"="principale")),
  "SEXE" = list("nom"="sexe",
                "legende"=list("ENS"="tous sexes confondus",
                               "H"="hommes",
                               "F"="femmes")),
  "AGE"=list("nom"="groupe d'âges",
             "legende"=list(
             "ENS"="tous groupes d'âges confondus",  
             "0014"="0 à 14 ans", 
             "1529"="15 à 29 ans", 
             "3044"="30 à 44 ans", 
             "4559"="45 à 59 ans", 
             "6074"="60 à 74 ans", 
             "75P"="75 ans et plus",  
             "7589"="75 à 89 ans", 
             "90P"="90 ans et plus",  
              "0019"="0 à 19 ans", 
              "2064"="20 à 64 ans", 
              "65P"="65 ans et plus",  
              "05P"="5 ans et plus",  
              "0514"="5 à 14 ans", 
              "1524"="15 à 24 ans", 
              "2554"="25 à 54 ans", 
              "55P"="55 ans et plus",  
              "15P"="15 ans et plus",  
              "01P"="1 an et plus",  
              "0114"="1 à 14 ans")),
  
  "CSP"=list("nom"="catégorie socio-professionelle",
             "legende"=list("ENS"="toutes catégories socio-professionnelles confondus",
                            "CS1"="agriculteur exploitant",
                            "CS2"="artisan, commerçant, chef d'entreprise",
                            "CS3"="cadre, profession intellectuelle supérieure",
                            "CS4"="profession intermediaire",
                            "CS5"="employé",
                            "CS6"="ouvrier",
                            "CS7"="retraité",
                            "CS8"="autre sans activité professionnelle")
  ),
  "MOBILITE"=list("nom"="mobilité résidentielle depuis 5 ans",
                  "legende"=list(
                    "ENS"="toutes mobilités résidentielles confondues",     
                    "IRAN1"="habitant auparavant dans le même logement",   
                    "IRAN2"="habitant auparavant dans un autre logement de la même commune",   
                    "IRAN3"="habitant auparavant dans une autre commune du même département",   
                    "IRAN4"="habitant auparavant dans un autre département de la même région",   
                    "IRAN5"="habitant auparavant dans une autre région de métropole",   
                    "IRAN6"="habitant auparavant dans un département d'outre-mer" ,  
                    "IRAN7"="habitant auparavant hors de métropole ou d'outre-mer",   
                    "IRAN3P"="habitant auparavant une autre commune",  
                     "IRAN2P"=  "habitant auparavant un autre logement"
                  ))
)

base_structure = list("data"=data_structure,
                      "meta"=meta_structure)



save(base_structure,file = file.path(data_clean,"evol_structure_pop.RData"))



# 05. LOGEMENT ------------------------------------------------------------

bases_logement = c("logements_-_migrations_résidentielles",
                   "logements")

var_voitures = c("GARL",
                 "VOIT1P",
                 "VOIT1",
                 "VOIT2P")

var_equipements = c("SDB",
                    "CCCOLL",
                    "CCIND",
                    "CINDELEC",
                    "ELEC",
                    "EAUCH",
                    "BDWC",
                    "CHOS",
                    "CLIM",
                    "TTEGOU","HABFOR","CASE","MIBOIS","MIDUR")

data_logements = data.frame("CODGEO"=character(),
                            "valeur"=character(), 
                            "annee"=character(),
                            "OCCUPATION"=character(),
                            "CONSTRUCTION"=character(),
                            "BAIL"=character(),
                            "MESURE"=character(),
                            "VOITURE"=character(),
                            "EQUIPEMENT"=character(),
                            "EMMENAGEMENT"=character(),
                            "DATE_CONSTRUCTION"=character(),
                            "NB_PIECE"=character())



i=5
for(i in 1:length(to_load)){
  file_name = paste0("aura.",to_load[i])
  split_name = unlist(str_split(file_name,"\\."))
  
  if(split_name[3] %in% bases_logement){
    print(split_name[2])


temp = readRDS(file = file.path(data_aura,file_name))


c_start = max(which(str_detect(colnames(temp),"^[A-Z]+$"))) +1

temp1 = temp%>% 
  mutate_if(is.numeric,as.character, is.factor, as.character,is.double,as.character) %>% 
  
  pivot_longer(
    cols = c(c_start[1]:ncol(temp)),
    values_to= "valeur",
    names_to="variable",
    values_ptypes = list(valeur=character()))  %>% 
  mutate(annee = ifelse(str_detect(variable,
                                   paste0("(P|C)",str_extract(split_name[2],"\\d{2}$"))),
                        split_name[2],
                        "else")) %>% 
  filter(annee!="else") %>%  
  mutate(variable_clean = str_remove(variable,"(P|C)[0-9]{2}_"),
         EXPLOITATION = str_extract(variable, '^(P|C)'),
         OCCUPATION = case_when(str_detect(variable_clean,"RP")~"RP",
                                str_detect(variable_clean,"RSECOCC")~"RSECOCC",
                                str_detect(variable_clean,"LOGVAC")~"LOGVAC",
                                T ~ "ENS"),
         CONSTRUCTION = case_when(str_detect(variable_clean,"APPART")~"APPART",
                                  str_detect(variable_clean,"MAISON")~"MAISON",
                                  T ~ "ENS"),
         BAIL = case_when(str_detect(variable_clean,"PROP") ~ "PROP",
                          str_detect(variable_clean,"LOCHLMV") ~ "LOCHLMV",
                          str_detect(variable_clean,"LOC") ~ "LOC",
                          str_detect(variable_clean,"GRAT") ~ "GRAT",
                          T ~ "ENS"),
         MESURE = case_when(str_detect(variable_clean,"PMEN")~"PMEN",
                            str_detect(variable_clean,"MEN")~"MEN",
                            str_detect(variable_clean,"NPER_RP")~"NPER_RP",
                            str_detect(variable_clean,"ANEM_RP")~"ANEM_RP",
                            str_detect(variable_clean,"NBPI")~"NBPI",
                            str_detect(variable_clean,paste(var_voitures,collapse = "|"))~"MEN",
                                       T ~"LOG"),
         VOITURE = ifelse(str_detect(variable_clean,paste(var_voitures,collapse = "|")),
                          str_extract(variable_clean,paste(var_voitures,collapse = "|")),
                          "ENS"),
         EQUIPEMENT = ifelse(str_detect(variable_clean,paste(var_equipements,collapse = "|")),
                          str_extract(variable_clean,paste(var_equipements,collapse = "|")),
                          "ENS"),
         EMMENAGEMENT =  ifelse(str_detect(variable_clean,"ANEM[0-9].+"),
                              str_extract(variable_clean,"ANEM[0-9].+"),
                              "ENS"),
         DATE_CONSTRUCTION =  ifelse(str_detect(variable_clean,"ACHT([0-9]|T)"),
                                str_extract(variable_clean,"ACHT([0-9]|T)"),
                                "ENS"),
         NB_PIECE =  ifelse(str_detect(variable_clean,"[0-9](P|PP)"),
                                     str_extract(variable_clean,"[0-9](P|PP)"),
                                     "ENS"))%>% 
  dplyr::select(CODGEO, valeur, annee,EXPLOITATION,OCCUPATION,CONSTRUCTION,BAIL,MESURE,
         VOITURE,EQUIPEMENT,EMMENAGEMENT,DATE_CONSTRUCTION,NB_PIECE)

data_logements <- data_logements %>% 
  rbind(temp1)

  }
}

data_logements = readRDS(file = file.path(data_clean,"data_logements.RDS"))
meta_logements = list(
  "annee" = list("nom"="année",
                 "legende"=list("2006"="2006", 
                                "2007"="2007",
                                "2008"="2008",
                                "2009"="2009",
                                "2010"="2010",
                                "2011"="2011",
                                "2012"="2012",
                                "2013"="2013",
                                "2014"="2014",
                                "2015"="2015",
                                "2016"="2016",
                                "2017"="2017",
                                "2018"="2018")),
  "EXPLOITATION"=list("nom"="exploitation",
                      "legende"=list("C"="complémentaire",
                                     "P"="principale")),
  "OCCUPATION"=list("nom"="occupation du logement",
                    "legende"=list(
                      "ENS"="toutes occupations du logement confonfues",
                      "RP"="résidence principale",
                      "RSECOCC"="résidence secondaire et logement occasionel",   
                      "LOGVAC"="logement vacant"    
                    )),
  "CONSTRUCTION"=list("nom"="type de construction",
                      "legende"=list("ENS"="tous types de construction",
                                     "MAISON"="maison",
                                     "APPART"="appartement")),
  "BAIL"=list("nom"="statut d'occupation",
              "legende"=list(
                "ENS"="tous status d'occupation confondus",    
                "PROP"  ="occupé par propriétaire", 
                "LOC"="occupé par locataire",    
                "LOCHLMV"="HLM loué vide",
                "GRAT"="occupé gratuitement"   
              )), 
  "MESURE"=list("nom"="mesure",
                "legende"=list(
                  "LOG"="nombre de logemen(t)",
                  "NBPI"="nombre de pièce(s)", 
                  "MEN"="nombre de ménage(s)",    
                  "PMEN"="population des ménages",   
                  "NPER_RP"="nombre de personnes en résidences principales",
                  "ANEM_RP"="ancienneté totale d'emménagement"
                )),
  "VOITURE"=list("nom"="possession automobile",
                 "legende"=list(
                   "ENS"="toutes possessions automobiles confondues",   
                   "GARL"="au moins un parking" ,
                   "VOIT1P"="une voiture ou plus",
                   "VOIT1"="une voiture", 
                   "VOIT2P"="deux voitures ou plus"
                 )),
  "EQUIPEMENT"=list(
    "nom"="équipement des résidences principales",
    "legende"=list(
      "ENS"="tous équipements confondus",     
      "SDB"="salle de bain",     
      "CCCOLL"="chauffage central collectif",  
      "CCIND"="chauffage central individuel",
      "CINDELEC"="chauffage individuel electrique",
      "ELEC"="électricité",    
      "EAUCH"="eau chaude",   
      "BDWC"="bain/douche wc",    
      "CHOS"="chaufe-eau solaire",    
      "CLIM"="pièce climatisée",   
      "TTEGOU"="tout à l'égoût", 
      "HABFOR"="habitation de fortune", 
      "CASE"="case traditionnelle",   
      "MIBOIS"="logement en bois", 
      "MIDUR"="logement en dur" )),
  
  "EMMENAGEMENT"=list("nom"="ancienneté d'emménagement",
                      "legende"=list(
                        "ENS"="toutes anciennetés d'emménagement",     
                        "ANEM0002"="emménagement depuis moins de 2 ans",
                        "ANEM0204"="emménagement entre 2 et 4 ans",
                        "ANEM0509"="emménagement entre 5 et 9 ans",
                        "ANEM10P"="emménagement depuis 10 ans ou plus", 
                        "ANEM1019"="emménagement entre 10 et 19 ans",
                        "ANEM2029"="emménagement entre 20 et 29 ans",
                        "ANEM30P"="emmménagement depuis 30 ans ou plus" 
                      )),
  "DATE_CONSTRUCTION"=list(
    "nom"="date de construction",
    "legende"=list(
      "ENS"="toutes dates de construction",  
      "ACHTT"="avant 2004",
      "ACHT1"="avant 1949",
      "ACHT2"="entre 1949 et 1974",
      "ACHT3"="entre 1975 et 1989",
      "ACHT4"="entre 1989 et 3 ans avant sondage"
    )
  ),
  "NB_PIECE" = list("nom"="nombre de pièces",
                    "legende"=list(
                      "ENS"="tous nombres de pièces confondus",
                      "1P"="une pièce", 
                      "2P"="deux pièces", 
                      "3P"="trois pièces", 
                      "4P"="quatre pièces", 
                      "5P"="cinq pièces ou plus", 
                      "0P"="erreur" 
                    ))
 
  
)


base_logements = list('data' = data_logements,
                      "meta"= meta_logements )


save(base_logements,file = file.path(data_clean,"logements_migrations_residentielles.Rdata"))

# 06. POPULATION ACTIVE CHOMAGE -------------------------------------------

var_statut = c("ACTOCC",
               "ACT",
               'CHOM',
               "INACT",
               "ETUD",
               "RETR",
               "AINACT")
var_secteur =c("AGRI",
               "INDUS",
               "CONST",
               "CTS",
               "APESAS")

data_chomage = data.frame("CODGEO"=character(), 
                          "valeur"=character(), 
                          "SEXE"=character(),
                          "AGE"=character(),
                          "CSP"=character(),
                          "MESURE"=character(),
                          "STATUT"=character(),
                          "TEMPSTR"=character(),
                          "SALARIE"=character(),
                          "SECTEUR"=character())


bases_chomage = c("population_active_-_emploi_-_chômage")

for(i in 1:length(to_load)){
  file_name = paste0("aura.",to_load[i])
  split_name = unlist(str_split(file_name,"\\."))
  
  if(split_name[3] %in% bases_chomage){
    print(split_name[2])
    print(i)
    

    
    temp = readRDS(file = file.path(data_aura,file_name))
    
    
    c_start = max(which(str_detect(colnames(temp),"^[A-Z]+$"))) +1
    
    temp1 = temp%>% 
      mutate_if(is.numeric,as.character, is.factor, as.character,is.double,as.character) %>% 
      
      pivot_longer(
        cols = c(c_start[1]:ncol(temp)),
        values_to= "valeur",
        names_to="variable",
        values_ptypes = list(valeur=character()))  %>% 
      mutate(valeur = as.numeric(valeur),
             annee = ifelse(str_detect(variable,
                                       paste0("(P|C)",str_extract(split_name[2],"\\d{2}$"))),
                            split_name[2],
                            "else")) %>% 
      filter(annee!="else") %>%  
      mutate(variable_clean = str_remove(variable,"(P|C)[0-9]{2}_"),
             EXPLOITATION = str_extract(variable, '^(P|C)'),
             SEXE = ifelse(str_detect(variable,'(_F|_H)'),
                           str_remove(str_extract(variable,'(_F|_H)'),'_'),
                           "ENS"),
             AGE = ifelse(str_detect(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          str_extract(variable_clean,"[0-9]{4}|[0-9]{2}P"),
                          "ENS"),
             CSP = ifelse(str_detect(variable_clean,"CS[0-9]"),
                          str_extract(variable_clean,"CS[0-9]"),
                          'ENS'),
             MESURE =  ifelse(str_detect(variable_clean,"LT"),
                              "EMPLT",
                              'POP'),
             STATUT = ifelse(str_detect(variable_clean,paste(var_statut,collapse = "|")),
                             str_extract(variable_clean,paste(var_statut,collapse = "|")),
                             "ENS"),
             TEMPSTR = ifelse(str_detect(variable_clean,"TP"),'TP',"ENS"),
             SALARIE = ifelse(str_detect(variable_clean,"NSAL|SAL"),
                              str_extract(variable_clean,"NSAL|SAL"),
                              "ENS"
             ),
             SECTEUR = ifelse(str_detect(variable_clean,paste(var_secteur,collapse = "|")),
                              str_extract(variable_clean,paste(var_secteur,collapse = "|")),
                              "ENS"))%>% 
      dplyr::select(CODGEO, valeur,annee,EXPLOITATION, SEXE,AGE,CSP,MESURE,STATUT,TEMPSTR,SALARIE,SECTEUR)
    
    data_chomage <- data_chomage %>% 
      rbind(temp1)
  }
}

data_chomage = readRDS(file = file.path(data_clean,"population_active_emploi_chômage.RDS"))

meta_chomage = list(
  "annee" = list("nom"="année",
                 "legende"=list("2006"="2006", 
                                "2007"="2007",
                                "2008"="2008",
                                "2009"="2009",
                                "2010"="2010",
                                "2011"="2011",
                                "2012"="2012",
                                "2013"="2013",
                                "2014"="2014",
                                "2015"="2015",
                                "2016"="2016",
                                "2017"="2017",
                                "2018"="2018")),
  "EXPLOITATION"=list("nom"="exploitation",
                      "legende"=list("C"="complémentaire",
                                     "P"="principale")),
  
  "SEXE" = list("nom"="sexe",
                "legende"=list("ENS"="tous sexes confondus",
                               "H"="hommes",
                               "F"="femmes")),
  "AGE" = list("nom"="groupe d'âge",
               "legende"=list("15P"="15 ans et plus", 
                              "1564"="15 à 64 ans",
                              "1524"="15 à 24 ans",
                              "2554"="25 à 54 ans",
                              "5564"="55 à 64 ans",
                              "ENS"="tout groupe d'âge confondu")),
  "CSP"=list("nom"="catégorie socio-professionelle",
             "legende"=list("ENS"="toutes catégories socio-professionnelles confondus",
                            "CS1"="agriculteur exploitant",
                            "CS2"="artisan, commerçant, chef d'entreprise",
                            "CS3"="cadre, profession intellectuelle supérieure",
                            "CS4"="profession intermediaire",
                            "CS5"="employé",
                            "CS6"="ouvrier",
                            "CS7"="retraité",
                            "CS8"="autre sans activité professionnelle")
  ),
  "MESURE"=list(
    "nom"="mesure",
    "legende"=list(
      "POP"="nombre de personnes",
      "EMPLT"="nombre d'emplois au lieu de travail"
    )
  ),
  "STATUT"=list("nom"="statut professionel",
                "legende"=list(
                  "ENS"="tous statuts professionel confondus",
                  "ACTOCC"="actif occupé",
                  "ACT"="actif",
                  "CHOM"="chômeur",
                  "INACT"="inactif",
                  "ETUD"="étudiant",
                  "RETR"="retraité",
                  "AINACT"="autre inactif")),
  "TEMPSTR"=list(
    "nom"="temps de travail",
    "legende"=list(
      "ENS"="tous temps de travail confondus",
      "TP"="temps plein"
    )
  ),
  "SALARIE" = list(
    "nom"="statut salarial",
    "legende"=list("ENS"="salarié et non salarié",
                   "SAL"="salarié",
                   "NSAL"="non salarié")
  ),
  "SECTEUR"=list(
    "nom"="secteur d'activité",
    "legende"=list(
      "ENS"="tous secteurs d'activité confondus",    
      "AGRI"="agriculture",   
      "INDUS"="industrie",  
      "CONST"="construction",  
      "CTS"="commerce, transports, services",    
      "APESAS"="administration publique, enseignement, santé, action sociale"
    )
  )
)


base_chomage = list(
  "data"=data_chomage,
  "meta"=meta_chomage
)

save(base_chomage, file = file.path(data_clean,"population_active_emploi_chômage.Rdata"))



# XX. SAUVERGARDE DU FICHIER ----------------------------------------------

# Liste des fichiers
liste = list.files(path = data_clean ) 

# Chargement des données
insee_data = list()
insee_data = lapply(liste, function(x){
  get(load(file.path(data_clean,x)))
})
names(insee_data) = sapply(liste, function(x) str_remove_all(x,".RData|.Rdata"))


View(insee_data$evol_structure_pop )
data_census = insee_data$evol_structure_pop$data %>% 
  filter_at(vars("AGE","CSP","MOBILITE"), all_vars(.=="ENS")) %>% 
  rename(population=valeur) %>% 
  dplyr::select(CODGEO,SEXE,population,annee)


data_actifs = insee_data$population_active_emploi_chômage$data %>% 
  filter(STATUT=="ACT",EXPLOITATION=="P",MESURE=="POP",AGE=="1564") %>% 
  rename(actif = valeur) %>% 
  dplyr::select(CODGEO,SEXE,actif,annee)

insee_data[['denominateur']][['data']] = data_actifs %>% left_join(data_census)

save(insee_data,file=file.path(telechargements,"insee_data.Rdata"))

