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

# tempo = as.data.frame(biovallee_map) 
# geometry = unique(tempo$geometry)
# df = data.frame("facet"=)
# 
# geom_sf(data = tempo,size=size,alpha=0,
#         linetype = line,fill=fill,aes(geometry=geometry))
# 
# 
# ggplot()+
#   geom_sf(data = tempo,size=size,alpha=0,
#           linetype = line,fill=fill, aes(geometry=geometry))+
#   facet_wrap(~.,ncol=3)
# 
# size=1.1
# alpha=1
# line="dotted"
# fill="transparent"
# facet = 1


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
Bases_insee <- function(){
  if(!exists("inseeApiData")){
    load(file = file.path(telechargements,"inseeApiData.RData"),
         envir = globalenv())
  }
  data.frame("id"= names(inseeApiData),
             "label" = sapply(inseeApiData, function(k){k[['label']]}),
             row.names = 1:length(names(inseeApiData))
             )
}

formattageData <- function(variable){
  meta = inseeApiData[[variable]]$meta
  inseeApiData[[variable]]$data %>% 
    pivot_longer(cols = unlist(str_split(variable,"-")),
                 names_to = "variable",
                 values_to="modalite") %>% 
    left_join(meta)

}




Modalite_insee <- function(variable){
  if(!exists("inseeApiData")){
    load(file = file.path(telechargements,"inseeApiData.RData"),
         envir = globalenv())
  }
  formattageData(variable) %>% 
    dplyr::select(mesure,lib_mesure,
           variable, lib_varible,
           modalite,lib_modalite) %>% 
    unique()
  
}

CommunesInsee <- function(base, annee_,facet,mono_dim=NULL){
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


