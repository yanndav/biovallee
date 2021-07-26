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


saveRDS(data_emploi,file = file.path(data_clean,"caracteristique_emploi_mobilite_professionnelle.RDS"))



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



saveRDS(data_menage,file = file.path(data_clean,"couples_familles_menages.RDS"))




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


saveRDS(data_diplome,file = file.path(data_clean,"diplomes_formation.RDS"))


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


saveRDS(data_structure,file = file.path(data_clean,"evol_structure_pop.RDS"))

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

saveRDS(data_logements,file = file.path(data_clean,"data_logements.RDS"))



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
saveRDS(data_chomage,file = file.path(data_clean,"population_active_emploi_chômage.RDS"))


# XX. SAUVERGARDE DU FICHIER ----------------------------------------------

# Liste des fichiers
liste = list.files(path = data_clean ) 

# Chargement des données
insee_data = list()
insee_data = lapply(liste, function(x){
  list("data"=readRDS(file.path(data_clean,x)))
})
names(insee_data) = sapply(liste, function(x) str_remove_all(x,"data_|.RDS"))


View(insee_data$evol_structure_pop$data )
data_census = insee_data$evol_structure_pop$data %>% 
  filter_at(vars("AGE","CSP","MOBILITE"), all_vars(.=="ENS")) %>% 
  rename(population=valeur) %>% 
  dplyr::select(CODGEO,SEXE,pop,annee)


data_actifs = insee_data$population_active_emploi_chômage$data %>% 
  filter(STATUT=="ACT",EXPLOITATION=="P",MESURE=="POP",AGE=="1564") %>% 
  rename(actif = valeur) %>% 
  dplyr::select(CODGEO,SEXE,actif,annee)

insee_data[['denominateur']][['data']] = data_actifs %>% left_join(data_census)

save(insee_data,file=file.path(telechargements,"insee_data.Rdata"))

