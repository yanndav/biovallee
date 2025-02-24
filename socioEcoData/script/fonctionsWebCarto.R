# 00. INSTALLATION PACKAGES -----------------------------------------------
# Initialisation dossiers
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
telechargements <- sub('/script','/data',getwd())

topo = file.path(telechargements,"topo")

# Installation packages
## Packages pour le téléchargement du découpage géographique
tel = c('httr','utils','sf',"ggsci","ggnewscale","wesanderson")
invisible(lapply(tel,function(pack){
  if(!c(pack %in% installed.packages()[,'Package'])){
    install.packages(pack) 
  } 
  do.call("require", list(pack)) 
}))

zonage_biovallee <- readRDS(file.path(telechargements,"codeCommunesEPCI.RDS")) %>% 
  filter(LIBEPCI %in% c("CC du Val de Drôme en Biovallée",
                        "CC du Diois",
                        "CC du Crestois et de Pays de Saillans Cœur de Drôme"))




# 01. FONCTIONS TOPOGRAPHIE -----------------------------------------------
# Eau
Cours_eau <- function(cours,echelle="drome") {
  if(!exists("eau_drome")){
    
    assign("eau_drome", 
           readRDS(file.path(telechargements,"eau_drome.RDS")),
           envir = globalenv())
  }
  
  if(echelle=="biovallee"){
    if(!exists("eau_biovallee")){
      if(!exists("biovallee")){
        
        assign("biovallee",
               st_union(st_read(file.path(topo,"EPCI.shp")) %>% 
                          filter(NOM %in% zonage_biovallee$LIBEPCI)),
               envir = globalenv()
        )
      }
      assign("eau_biovallee",
             st_crop(eau_drome, extent(as_Spatial(biovallee))),
             envir = globalenv()
             ) 
      
    }
    
    p = list(geom_sf(data = eau_biovallee%>% 
                       filter(courseau %in% str_to_lower(cours)),
                     color="deepskyblue4", size=1.2))
    
    
    r = geom_sf_text(data = eau_biovallee[eau_biovallee$courseau=="rhone",],
                     aes(label = TOPONYME),
                     hjust = -3.5,
                     vjust= 2.5,
                     angle =90,
                     family="serif",
                     color = "deepskyblue4")
    
    d = geom_sf_text(data = eau_biovallee[eau_biovallee$courseau=="drome",],
                     aes(label = TOPONYME),
                     hjust = -1.5,
                     vjust= -1.2,
                     angle=-45,
                     family="serif",
                     color = "deepskyblue4")
    
    if("rhone" %in% str_to_lower(cours) ){
      p = p %>% append(r)
    }
    
    if("drome" %in% str_to_lower(cours)){
      p = p %>% append(d)
    }
    
  }else{
    p = list(geom_sf(data = eau_drome%>% 
                       filter(courseau %in% str_to_lower(cours)),
                     color="deepskyblue4", size=1.2))
    
    
    r = geom_sf_text(data = eau_drome[eau_drome$courseau=="rhone",],
                     aes(label = TOPONYME),
                     hjust = -3.5,
                     vjust= 2.5,
                     angle =90,
                     family="serif",
                     color = "deepskyblue4")
    
    d = geom_sf_text(data = eau_drome[eau_drome$courseau=="drome",],
                     aes(label = TOPONYME),
                     hjust = -1.5,
                     vjust= -1.2,
                     angle=-45,
                     family="serif",
                     color = "deepskyblue4")
    
    if("rhone" %in% str_to_lower(cours) ){
      p = p %>% append(r)
    }
    
    if("drome" %in% str_to_lower(cours)){
      p = p %>% append(d)
    }
  }
  
  

  p

  
}

  
# Altitude
Altitude <- function(echelle="departement"){
  if(!exists("alti_drome")){
    assign("alti_drome", 
           readRDS(file.path(telechargements,"alti_drome.RDS")),
           envir = globalenv())
  }
  
  if(echelle=="departement"|echelle=="drome"){
    list(new_scale("fill"),
         geom_raster(data = alti_drome, 
                     aes(x = x, y = y, fill = height)),
         scale_fill_gradientn(colours = alpha(terrain.colors(100),
                                              alpha=0.4)),
         labs(fill="Elevation")
    )
    
  }else{
    if(!exists("alti_biovallee")){
      assign("alti_biovallee",
             readRDS(file.path(telechargements,"alti_biovallee_df.RDS")),
             envir = globalenv())
      
    }
    list(new_scale("fill"),
         geom_raster(data = alti_biovallee, 
                     aes(x = x, y = y, fill = height)),
         scale_fill_gradientn(colours = alpha(terrain.colors(100),
                                              alpha=0.4)),
         labs(fill="Elevation")
    )
    
  }
  
}

# 02. DECOUPAGE ADMINISTRATIF ---------------------------------------------

Departement <- function(){
    if(!exists("departement")){
      
      assign("departement", st_read(file.path(topo,"DEPARTEMENT.shp")) %>% 
               filter(INSEE_DEP==26), envir = globalenv())
      
    }
    geom_sf(data = departement, fill="transparent")

  }

Epci<- function(size=1.1,fill=F,
                alpha=1,text=F,line="dotted"){
  if(!exists("epci")){
    assign("epci", 
           st_read(file.path(topo,"EPCI.shp")) %>% 
             filter(NOM %in% zonage_biovallee$LIBEPCI)%>% 
             mutate(NOM = str_remove(NOM,"CC du ")),
           envir = globalenv())
  }
  
  if(fill==F){
    l = list(geom_sf(data = epci, size=size,alpha=0,
                     linetype = line))
  }else{
    l = list(new_scale("fill"),
             geom_sf(data = epci, 
                     aes(fill=NOM),
                     size=size,
                     linetype = line),
             scale_fill_npg(alpha = alpha),
             labs(fill="Communauté de commune"))
  }
  
  
  if(text==T){
    l = l %>%  append(
      geom_sf_text(data = epci,
                   aes(label =stringr::str_wrap(NOM, 15) ),
                   fontface="bold",
                   size=3.5,
                   vjust = c(1,0,-0.2),
                   hjust = c(0.5,0.5,0.5))
    )
    
  }
  

  
  
  l
  
}



Biovallee <- function(size=1.1,
                      alpha=1,line="dotted",
                      fill="transparent",
                      facet = NULL){
  if(!exists("biovallee_map")){
    assign("biovallee_map",
           st_union(st_read(file.path(topo,"EPCI.shp")) %>% 
             filter(NOM %in% zonage_biovallee$LIBEPCI)),
           envir = globalenv()
           )
  }
  
    
  geom_sf(data = biovallee_map,size=size,alpha=0,
          linetype = line,fill=fill)
  
  
}


Communes <- function(echelle="departement",
                     size=1.1,
                     alpha=1,line="dashed",
                     fill=F){
  if(!exists("geocommune_drome")){
    assign("geocommune_drome",
           readRDS(file.path(telechargements,"geocommune_drome.RDS")) %>% 
             filter(INSEE_COM %in% zonage_biovallee$CODGEO),
           envir = globalenv()
    )
  }
  
  if(echelle=="biovallee"){
    communes = geocommune_drome %>% 
      filter(CODE_EPCI %in% zonage_biovallee$EPCI)
    
  }else{
    communes = geocommune_drome
  }
  
  
    geom_sf(data = communes,size=size,alpha=0,
            linetype = line)
    
  
  
  
  
}



# 03. STATISTIQUES INSEE --------------------------------------------------
VariablesDansBase <- function(nom_base){
  temp = insee_data[[nom_base]][["data"]] %>% 
    dplyr::select(-CODGEO,-valeur,annee) %>% 
    distinct()
  for (i in 1:ncol(temp)) {
    cat(paste("Nom de la variable: ",names(temp[,i]),"\n"))
    cat(paste("Label de la variable: ",insee_data[[nom_base]][["meta"]][[names(temp[,i])]]$nom,"\n\n"))
    
    print(data.frame("identifiant label" = pull(unique(temp[,i])),
                     "label"= sapply(pull(unique(temp[,i])), function(item){insee_data[[nom_base]][["meta"]][[names(temp[,i])]]$legende[[item]]})))
    cat("\n\n-------------------\n\n")
  }
  
}

graphsPossibles <- function(nom_base,silent=F){
  if(!exists(paste0("listegraphs_",nom_base))){
    base_temp = insee_data[[nom_base]][["data"]] %>% 
    dplyr::select(-CODGEO,-valeur,-annee) %>% 
    distinct()
  
  variables = names(base_temp)
  
  liste_graphs = list()
  
  doc_graphs = c()
  for(i in 1:nrow(base_temp)){
    mono_dim=list()
    for(var in variables){
      t =base_temp[[var]][i]
      if(!is.na(t)){
        mono_dim[[var]] = t
      }
    }
    
    mesures = mesure(base_temp, nom_base , insee_data, mono_dim)
      
      
      dimensions = paste(sapply(names(mono_dim)[which(!(names(mono_dim) %in% c("EXPLOITATION","MESURE")))], function(name){
        if(!is.null(mono_dim[[name]])){
         paste0(insee_data[[nom_base]][["meta"]][[name]]$legende[[mono_dim[[name]]]],", ")
        }else{
          ""
        }
      } )
      , collapse = "")
      
      
      annees = suppressMessages((base_temp[i,] %>% inner_join(
        insee_data[[nom_base]][["data"]] %>% 
          dplyr::select(-CODGEO,-valeur)%>% 
          distinct() ))[["annee"]])
      
      legende_annee = paste(", entre", min(annees), "et", max(annees))
      
      
      graph_desc = paste0(i,':',str_to_sentence(paste0(mesures," ",dimensions,legende_annee)),
       "\n",
       paste(sapply(colnames(base_temp[i,]),function(name){paste0(name,": ", base_temp[[name]][i],"  -  ")}),collapse = ""),
        '\n--\n'
      )
        
        doc_graphs = c(doc_graphs,graph_desc)
   
      
     
      
      liste_graphs[[i]] = mono_dim
     
  }
  assign(paste0("graphs_",nom_base),liste_graphs, envir = globalenv())
  assign(paste0("listegraphs_",nom_base),doc_graphs, envir = globalenv())
  
  }
  if(!silent){
    cat(get(paste0("listegraphs_",nom_base)))
    
  }

}

obtenirGraph <- function(nom_base,numero_graph,echelle="drome",group=NULL,echelle_operateur="sum",denominateur=1){
 if(!exists(paste0('graphs_',nom_base))){
   print('Je génère les possibilités de graphs')
   graphsPossibles(nom_base,silent=T)
 }
  
  mono_dim = get(paste0('graphs_',nom_base))[[numero_graph]]
  
  EvolutionInsee(nom_base = nom_base,
                 echelle = echelle,
                 echelle_operateur = echelle_operateur,
                 group = group,
                 denominateur = denominateur,
                 mono_dim = mono_dim)
  
  
}




DimensionsDispo <- function(nom_base,variable, modalite){
  insee_data[[nom_base]][["data"]] %>% 
    dplyr::select(-CODGEO,-valeur,-annee) %>% 
    filter(!!as.symbol(variable)==modalite) %>% 
    distinct() 

  
}

EvolutionInsee <- function(
  nom_base ,
  echelle = "drome",
  echelle_operateur = "sum",
  denominateur=1,
  group,  mono_dim){
  
  if(!exists("insee_data")){
    load(file = file.path(telechargements,"insee_data.RData"),
         envir = globalenv())
  }
  if(!exists("geocommune_reg")){
    geocommune_reg = readRDS(file.path(telechargements,"geocommune_reg.RDS"))
    
  }
  
   dat = insee_data[[nom_base]][["data"]]
  
   if(is.character(denominateur)){
     denom = insee_data$denominateur$data %>% 
       dplyr::select(!! denominateur,CODGEO,annee,SEXE) %>% 
       rename(denom = !!denominateur)
     
     dat <- dat %>% left_join(denom) %>% 
       mutate(valeur = (as.numeric(valeur)/as.numeric(denom))) %>% 
       dplyr::select(-denom)
     
     
   }else{
     dat <- dat %>%
       mutate(valeur = (as.numeric(valeur)/denominateur))
   }
  
  # Echelle spatiale désirée
  if(echelle=="drome"|echelle=="departement"){
    communes = geocommune_reg %>% 
      filter(INSEE_DEP==26)
  }else if(echelle=="biovallee"){
    communes = geocommune_reg %>% 
      filter(CODE_EPCI %in% unique(zonage_biovallee$EPCI))
  }
  
  
 
    base = dat %>%
      filter(CODGEO %in% communes$INSEE_COM) %>% 
      group_by_at(setdiff(names(dat), c("CODGEO","valeur"))) %>% 
      mutate(valeur=as.numeric(valeur),
             n= n()) %>% 
      group_by_at(setdiff(names(.), c("CODGEO","valeur"))) 
    
    
    
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
  
  if(!is.null(group)){
    if(names(group)[1]%in% names(mono_dim)){
      mono_dim = within(mono_dim, rm(list=names(group)[1]))
    }
    
    # Conservation des dimensions de la variable de groupement
    if(group[[1]][1]!="all"){
      print("not all")
      # Si seulement quelques modalités conservées, on les filtre
      base = base %>%  
        filter((!!as.symbol(names(group)[1])) %in% group[[1]])
      dimensions = group[[1]]
    }else{
      print('all')
      # Si toutes les modalités conservées, on enlève celle d'ensemble
      base = base %>%  
        filter((!!as.symbol(names(group)[1])) != "ENS")
      
    }
  }

  
  # Application du subset
  for(variable in names(mono_dim)){
    # Si toutes les modalités conservées, on enlève celle d'ensemble
    base = base %>%  
      filter((!!as.symbol(variable)) == mono_dim[[variable]])
    
    if(!is.null(group)){
      dimensions = unique(base[[names(group)[1]]])
      
    } 
  }
  
    if(is.null(group)){
      dim_annee = length(unique(base$annee))
      dim_reelle = nrow(base)
      
    }else{
      dim_reelle = nrow(base[base[[names(group)[1]]] %in% dimensions,])
      dim_annee = length(unique(base$annee)) * length(dimensions)
    }
  
  
  if(dim_reelle > dim_annee){
    print('Problème de dimension: il y a plus de dimensions ville/année/variable/mesure que possible, revoyez les dimensions dans mono_dim')
    
    variables = names(base)[!(names(base) %in% c(names(group)[1],"annee","valeur"))]
    for(var in variables){
      print(paste(var, ":",paste(unique(base[[var]]),collapse =" ")))
      valeurs = unique(base[[var]])
      if(length(valeurs)>1){
        print(paste("Problème de dimension sur la variable", var))
        if("ENS"%in% valeurs){
          print("Réduction à la valeur ENS")
          base = base %>% 
            filter((!!as.symbol(var)) == "ENS")
          
        }else{
          print(paste("Pas de variable ENS trouvée, je fais l'opération",echelle_operateur,"sur la variable"))
          # Opération de regroupement à cette échelle
          if(echelle_operateur=="sum"){
            base = base %>% 
              dplyr::select(-(!!as.symbol(var))) %>% 
              group_by_at(setdiff(names(base), c(var,"valeur"))) %>% 
              
              summarise(valeur = sum(valeur, na.rm=T)) %>% 
              mutate(!!var := "ENS_genere")
          }else if(echelle_operateur=="mean"){
            base = base %>% 
              dplyr::select(-(!!as.symbol(var))) %>% 
              group_by_at(setdiff(names(base), c(var,"valeur"))) %>% 
              
              summarise(valeur = mean(valeur, na.rm=T))%>% 
              mutate(!!var := "ENS_genere")
          }else{
            print("error no grouping operator")
            break 
          }
          
        }
      }
    }
    
    
  }
  

  
  
  
  base_temp = base %>% 
    mutate(annee_fact =as.factor(paste0(annee,"\n(n=",n,")")))
  

  if(is.null(group)){
    ggplot(data = base_temp,
                  aes_string(x="annee_fact",
                             y="valeur",
                             group =1))+
      geom_line()+
      geom_point()+
      theme_light()+
      labs(title = paste(strwrap(titre(base_temp,nom_base, mono_dim,echelle, group, insee_data,denominateur), width = 80), collapse = "\n"),
           y=str_to_sentence(mesure(base_temp, nom_base , insee_data, mono_dim, denominateur)),
           x="Année")
  }else{
    ggplot(data = base_temp,
                  aes_string(x="annee_fact",
                             y="valeur",
                             group= names(group)[1],
                             color = names(group)[1]))+
      geom_line()+
      geom_point()+
      theme_light()+
      scale_color_npg(labels=labels(insee_data,group))+
      labs(title = paste(strwrap(titre(base_temp,nom_base, mono_dim,echelle, group, insee_data,denominateur), width = 80), collapse = "\n"),
           y=str_to_sentence(mesure(base_temp, nom_base , insee_data, mono_dim, denominateur)),
           x="Année",
           colour=legende(insee_data,group))
  }
  
  
  
  
}
labels = function(insee_data,group){
  sapply(insee_data[[nom_base]]$meta[[names(group)[1]]]$legende,str_to_sentence)
}
legende = function(insee_data,group){
  str_to_sentence(insee_data[[nom_base]]$meta[[names(group)[1]]]$nom)
}

mesure <-  function(base_temp,nom_base,insee_data, mono_dim, denominateur=NULL){
  
  # Obtention du nom de la mesure
  if("MESURE" %in% names(base_temp) & length(unique(base_temp[["MESURE"]]))==1){
    # Si incluse dans la base par le jeu du subset
    valeur = unique(base_temp[["MESURE"]])
    mesure =  insee_data[[nom_base]][["meta"]]$MESURE$legende[[valeur]]
  }else if("MESURE" %in% names(mono_dim)){
    valeur = mono_dim[["MESURE"]]
    mesure =  insee_data[[nom_base]][["meta"]]$MESURE$legende[[valeur]]
  }else{
    # Si pas de variable de mesure, alors récupère la première légende de mesure
    # dans méta
    mesure = insee_data[[nom_base]][["meta"]]$MESURE$nom
  }
  
  if(!is.null(denominateur)){
    # Intégration de l'effet de dénominateur
    if(is.character(denominateur)){
      mesure = paste(mesure,"par",denominateur)
    }else if(is.numeric(denominateur) & denominateur>1){
      mesure = paste(mesure,"divisé par",denominateur)
      
    }
    
  }

  if("valeur"%in%names(base_temp)){
    # Conversion en pourcentage
    if(min(base_temp$valeur)>=0 & max(base_temp$valeur)<=1){
      mesure = str_replace(mesure, "nombre de","pourcentage de")
    }
    
    
  }
  

  if(mesure == "population par population"){
    mesure = "pourcentage de la population"
  }
  
  
  return(mesure)
}

titre <- function(base_temp,nom_base, mono_dim,echelle, group, insee_data,denominateur){

  mesures = str_to_sentence(mesure(base_temp, nom_base, insee_data, mono_dim, denominateur))
  
  
  dimensions = paste(sapply(names(mono_dim)[which(!(names(mono_dim) %in% c("MESURE","EXPLOITATION")))], function(name){
    if(mono_dim[[name]]!="ENS"){
      paste0(insee_data[[nom_base]][["meta"]][[name]]$legende[[mono_dim[[name]]]],", ")
    }else{
      ""
    }
  } )
  , collapse = "")
  
  if(!is.null(group)){
    nom_groupe = paste("par", insee_data[[nom_base]][["meta"]][[names(group)[1]]]$nom,", ")
  }else{
    nom_groupe=""
  }
  
  zone = paste0(" de la ",str_to_sentence(echelle),", ")
  annees = as.numeric(base_temp$annee)
  legende_annee = paste("entre", min(annees), "et", max(annees))
  
  return(paste0(mesures,zone,dimensions,nom_groupe,legende_annee))
  
}

CommunesInsee <- function(base, annee_,group,mono_dim=NULL){
  if(!exists("inseeApiData")){
    load(file = file.path(telechargements,"inseeApiData.RData"),
         envir = globalenv())
  }
  if(!exists("geocommune_biovallee")){
    if(!exists("geocommune_drome")){
      assign("geocommune_drome",
             readRDS(file.path(telechargements,"geocommune_drome.RDS")) %>% 
               filter(INSEE_COM %in% zonage_biovallee$CODGEO),
             envir = globalenv()
      )
    }
    
    assign("geocommune_biovallee",
           geocommune_drome %>% 
             filter(CODE_EPCI %in% zonage_biovallee$EPCI),
           envir=globalenv()
    )
  }
  
  
  ### Création des variables temporaires
  data_temp = inseeApiData[[base]]$data %>% 
    filter(!is.na(valeur))
  meta_temp = inseeApiData[[base]]$meta
  label = inseeApiData[[base]]$label 
  
  
  ## Sélection de la bonne année
  while(!(annee_ %in% data_temp$annee)){
    cat(paste0("L'année renseignée n'est pas contenue dans la base, ecrivez parmi: ",paste(unique(data_temp$annee), collapse = " ")))
    annee_ = readline("Renseignez une année:")
  }
  title = paste(label, "par commune en", annee_)
  
  data_temp = data_temp %>% 
    filter(nivgeo =="COM") %>% 
    left_join(geocommune_biovallee, by=c("codgeo"="INSEE_COM")) %>% 
    filter(annee %in% annee_,
           !is.na(valeur)) %>% 
    unique()
  
  # Conservation des dimensions de la facet
  if(facet[[1]]!="all"){
    # Si seulement quelques modalités conservées, on les filtre
    data_temp = data_temp %>%  
      filter((!!as.symbol(names(facet)[1])) %in% facet[[1]])
    dim_facet = length(facet[[1]])
  }else{
    # Si toutes les modalités conservées, on enlève celle d'ensemble
    data_temp = data_temp %>%  
      filter((!!as.symbol(names(facet)[1])) != "ENS")
    dim_facet = length(unique(data_temp[[names(facet)[1]]]))
  }
  
  # Creation variable label
  if(names(facet)[1] %in% meta_temp$variable){
    meta_temp = meta_temp %>% 
      filter(variable==names(facet)[1]) %>% 
      dplyr::select(modalite, lib_modalite)
    
    names(meta_temp) <- c(names(facet)[1], paste0('lib_',names(facet)[1]) )
    
    
    data_temp = data_temp %>% left_join(meta_temp)
  }
  
  # Application du subset
  if(is.null(mono_dim)){ # Si pas de subset vérification du nombre de dimension suffisant
    exp = data_temp$codgeo[1]
    dim_city = length(data_temp$codgeo[data_temp$codgeo==exp])
    
    if(dim_city != dim_facet){
      cat('Problème de dimension: il y a plus de dimensions ville/année/variable/mesure que possible, revoyez les dimensions dans mono_dim')
      break
    }
    
  }else{
    for(variable in names(mono_dim)){
      # Si toutes les modalités conservées, on enlève celle d'ensemble
      data_temp = data_temp %>%  
        filter((!!as.symbol(variable)) == mono_dim[[variable]])
    }
    
    exp = data_temp$codgeo[1]
    dim_city = length(data_temp$codgeo[data_temp$codgeo==exp])
    
    if(dim_city != dim_facet){
      cat('Problème de dimension: il y a plus de dimensions ville/année/variable/mesure que possible, revoyez les dimensions dans mono_dim')
      break
    }
  }
  
  # Creation du graph
  pal <- wes_palette("Zissou1", 100, type = "continuous")
 
  
  list(new_scale('fill'),
       geom_sf(data = data_temp,
               aes(geometry=geometry,
                   fill = as.numeric(valeur))),
       facet_wrap(. ~ data_temp[[paste0('lib_',names(facet)[1])]], 
                  labeller = label_wrap_gen(width=40),
                  ncol =3),
       scale_fill_gradientn(colours = pal),
       labs(fill=stringr::str_wrap(label,20),
            title=stringr::str_wrap(title,70))
    
  )
    
  
}


 # Cartographie évolution entre deux années


# Communes_insee_evolution <- function(variable,annee0,annee1,
#                                      categorie = NULL){
#   if(!exists("inseeApiData")){
#     load(file = file.path(telechargements,"inseeApiData.RData"),
#          envir = globalenv())
#   }
#   if(!exists("geocommune_biovallee")){
#     if(!exists("geocommune_drome")){
#       assign("geocommune_drome",
#              readRDS(file.path(telechargements,"geocommune_drome.RDS")) %>% 
#                filter(INSEE_COM %in% zonage_biovallee$CODGEO),
#              envir = globalenv()
#       )
#     }
#     
#     assign("geocommune_biovallee",
#            geocommune_drome %>% 
#              filter(CODE_EPCI %in% zonage_biovallee$EPCI),
#            envir=globalenv()
#     )
#   }
#   
#   
#   
#   label = inseeApiData[[variable]]$label
#   title = paste("Evolution des",str_to_lower(label), "par commune entre", annee0, "et", annee1)
#   data = inseeApiData[[variable]]$data %>%
#     left_join(geocommune_biovallee, by = c("codgeo"="INSEE_COM")) %>% 
#     mutate(valeur = as.numeric(valeur)) %>% 
#     filter(annee %in% c(annee0,annee1)) %>% 
#     select(annee,valeur,geometry,lib_mesure,libgeo) %>% 
#     pivot_wider(id_cols =c(geometry,lib_mesure,libgeo),
#                 names_from = annee,
#                 values_from = valeur)
#   
#   
#   data$diff = data[[as.character(annee1)]]-data[[as.character(annee0)]]
#     
#   categorie = ifelse(is.null(categorie),data$lib_mesure,categorie)
#                      
#   pal <- wes_palette("Zissou1", 100, type = "continuous")
#   
#   
#   list(new_scale("fill"),
#        geom_sf(data=data %>% filter(lib_mesure %in% categorie),
#                aes(fill=diff,
#                    geometry=geometry)),
#        facet_wrap(~lib_mesure),
#        scale_fill_gradientn(colours = pal),
#        labs(title=title,
#             fill=label))
#   
#   
# }

# 04. MISE EN FORME -------------------------------------------------------

Theme <- function(){
  list(
    theme_minimal(),
    
    theme(legend.position = "right",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(margin=margin(0,10,30,5))),
    
    labs(x=NULL,y=NULL)
  )
  
}

