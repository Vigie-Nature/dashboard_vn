# fonction pour dash board
library(leaflet)
library(sf)
library(tmap)
library(dplyr)
library(tidyr)
library(ggplot2)




# Calculs ----

# stats_globales
# all = toutes les valeurs incluses dans le calcul
# table = renvoie un tableau selon la variable
# valeur = filtrer
# compter classes, profs, etablissements
# Nombre de
# all = toutes les valeurs incluses dans le calcul
# table = renvoie un tableau selon la variable
# valeur = filtrer
# compter classes, profs, etablissements
stats_globales <- function(df, compter = "participants", select_observatoire = "all", select_annee = "all", select_protocole = "all", select_type_etablissement = "all"){
  if (!compter %in% c("participants", "observations")) stop("L'argument compter doit prendre l'une des valeurs suivantes : 'participants', 'observations'")
  variable_a_compter <- switch (compter,
                                participants = "user_id",
                                observations = "participation_id"
  )
  
  # choose the variable that will make the table
  if (any(select_observatoire == "table", 
          select_annee == "table", 
          select_protocole == "table",
          select_type_etablissement == "table")){
    group = c()
    if (select_observatoire == "table") group = c(group, "observatoire")
    if (select_annee == "table") group = c(group, "annee_participation")
    if (select_protocole == "table") group = c(group, "protocole")
    if (select_type_etablissement == "table") group = c(group, "type_etablissement")
  }
  
  # choose the variables that are fixed
  if (!select_observatoire %in% c("all", "table")) df = dplyr::filter(df, observatoire %in% select_observatoire)
  if (!select_annee %in% c("all", "table")){
    select_annee <- as.numeric(select_annee)
    df = dplyr::filter(df, annee_participation %in% select_annee)
  } 
  if (!select_protocole %in% c("all", "table")) df = dplyr::filter(df, protocole %in% select_protocole)
  if (!select_type_etablissement %in% c("all", "table")) df = dplyr::filter(df, type_etablissement %in% select_type_etablissement)
  
  # if no table function without group_by
  if (!"table" %in% c(select_annee, select_protocole, select_observatoire, select_type_etablissement)){
    result <- df %>%
      select_at(all_of(variable_a_compter)) %>%
      distinct()%>%
      drop_na() %>%
      summarise(nombre = n())
    names(result) = compter
    result
  } else if (compter == "eleves") {
    result <- df %>%
      dplyr::select(c(variable_a_compter, group, "effectifs")) %>%
      distinct() %>%
      drop_na() %>%
      group_by_at(c(group)) %>%
      summarise(nombre = sum(effectifs))
    result <- as.data.frame(result)
    names(result)[names(result) == 'nombre'] <- compter
    result
  } else {
    result <- df %>%
      group_by_at(all_of(group)) %>%
      dplyr::select(all_of(variable_a_compter)) %>%
      distinct()%>%
      drop_na() %>%
      summarise(nombre = n())
    result <- as.data.frame(result)
    names(result)[names(result) == 'nombre'] <- compter
    result
  }
}

#' Extraire des statistiques pour une variable
#'
#' @param df un data.frame
#' @param calc_var le nom d'une variable sous forme d'une chaine caractère
#' @param group_var un vecteur des variables à utiliser pour faire des groupes
#'
#' @return Un data.frame contenant des résumés de données
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' summarise_data(participation, calc_var = "nombre_individus", group_var = FALSE)
#' summarise_data(participation, calc_var = "nombre_individus", group_var = c("protocole", "niveau"))
summarise_data <- function (df, calc_var, group_var = FALSE){
  colnames_index <- c("moyenne", "ecartType", "min", "quartile1",
                      "mediane", "quartile2", "quantile98", "max",
                      "nombre", "manquantes")
  
  if (group_var[1] != FALSE){
    
    summarised_data <- df %>%
      group_by_at(group_var) %>%
      summarise_at(vars(calc_var), list(~ mean(., na.rm = TRUE),
                                        ~ sd(., na.rm = TRUE),
                                        ~ quantile(.,probs = 0, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.25, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.5, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.75, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.98, na.rm = TRUE),
                                        ~ quantile(.,probs = 1, na.rm = TRUE),
                                        ~ length(.),
                                        ~ sum(is.na(.))))
    
    colnames(summarised_data) <- c(group_var, colnames_index)
  } else {
    summarised_data <- df %>%
      summarise_at(vars(calc_var), list(~ mean(., na.rm = TRUE),
                                        ~ sd(., na.rm = TRUE),
                                        ~ quantile(.,probs = 0, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.25, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.5, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.75, na.rm = TRUE),
                                        ~ quantile(.,probs = 0.98, na.rm = TRUE),
                                        ~ quantile(.,probs = 1, na.rm = TRUE),
                                        ~ length(.),
                                        ~ sum(is.na(.))))
    colnames(summarised_data) <- colnames_index
  }
  summarised_data
}

# add a value for the "annee scolaire"
# style
label_years <- function (x, retour = FALSE){
  year_moins_un <- as.numeric(x) - 1
  if (retour) paste0(year_moins_un, "\n", x) else paste(year_moins_un, " - ", x)
}

# add a value to order mounth according to the "annee scolaire"
# decrepated
convert_mounth <- function(x){
  x <- ifelse(x > 12 | x < 1, NA, x)
  mois_ordered = ifelse(x < 9, x + 4, x - 8)
  mois_ordered
}

# label each mounth with the name in French
label_mounth <- function(x, short = FALSE){
  if (short) {
    label = hutils::Switch(as.character(x), DEFAULT = "Non Déterminé",
                           "1" = "Jan", 
                           "2" = "Fév", 
                           "3" = "Mar", 
                           "4" = "Avr", 
                           "5" = "Mai", 
                           "6" = "Juin", 
                           "7" = "Juil", 
                           "8" = "Août", 
                           "9" = "Sept", 
                           "10" = "Oct",
                           "11" = "Nov", 
                           "12" = "Déc"
    )
  } else {
    label = hutils::Switch(as.character(x), DEFAULT = "Non Déterminé",
                           "1" = "Janvier", 
                           "2" = "Février", 
                           "3" = "Mars", 
                           "4" = "Avril", 
                           "5" = "Mai", 
                           "6" = "Juin", 
                           "7" = "Juillet", 
                           "8" = "Août", 
                           "9" = "Septembre", 
                           "10" = "Octobre",
                           "11" = "Novembre", 
                           "12" = "Décembre"
    )
  }
  label
}

# Graph ----

#graph_participation_par_mois(participation, annee_focale = 2016)
# graph participation
graph_participation_par_mois <- function (df, annee_focale = FALSE, select_protocole = "all"){
  
  if (select_protocole != "all") df <- filter(df, protocole == select_protocole)
  
  obs <- df %>%
    filter(annee_participation != annee_focale)%>%
    dplyr::select(mois_participation, annee_participation, participation_id)%>%
    distinct()%>%
    group_by(mois_participation, annee_participation)%>%
    summarise(NombreObservations = n())
  
  obsNormalisee <- summarise_data(obs, calc_var = "NombreObservations", group_var = "mois_participation")
  
  # reorder mounth
  obsNormalisee$mois_participation_label <- label_mounth(obsNormalisee$mois_participation, short = TRUE)
  
  if (annee_focale != FALSE){
    
    obsAnnee <- df %>%
      filter(annee_participation == annee_focale) %>%
      dplyr::select(mois_participation, annee_participation, participation_id)%>%
      distinct()%>%
      group_by(mois_participation) %>%
      summarise(NombreObservations = n())
    
    # reorder mounth
    obsAnnee$mois_participation_label <- label_mounth(obsAnnee$mois_participation, short = TRUE)
  }
  
  couleursMinMax <- VNE_colors$vertclairVNE
  couleursQuartile <- VNE_colors$grisUrbain
  couleursMediane <- VNE_colors$texteVNE
  couleursAnnee <- VNE_colors$vertVNE
  
  graph <- ggplot(obsNormalisee, aes(x = mois_participation_label, group = 1)) +
    geom_ribbon(aes(ymin = quartile1, ymax = max), fill = couleursMinMax, alpha = 1) +
    #geom_ribbon(aes(ymin = quantile..2, ymax = quantile..4), fill = couleursQuartile, alpha = .6) +
    geom_line(aes(y = mediane), color = couleursMediane)
  
  if (annee_focale != FALSE){
    graph <- graph +
      geom_point(data = obsAnnee, aes(y = NombreObservations), color = couleursAnnee) +
      geom_line(data = obsAnnee, aes(y = NombreObservations), color = couleursAnnee)
  }
  
  graph <- graph +    
    scale_x_discrete(limits = month_order_reg_short)+
    ylab("Nombre de sessions d'observation")+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_text(size = 10),
          axis.text.x=element_text(angle = 0, size = 6),
          axis.text.y=element_text(angle = 0, size = 7, hjust = .5),
          text = element_text(colour = "#314C4C"),
          panel.grid.minor = element_line(linewidth = 0.1), 
          panel.grid.major = element_line(linewidth = .2))
  
  graph
}

graph_new_users <- function(data_participation) {
  
  # get table of users per period
  new_users <- data_participation %>%
    dplyr::select(user_id, annee_participation) %>%
    distinct() %>%
    arrange(annee_participation) %>%
    data.table::setDF()
  
  # get all years
  years <- unique(new_users$annee_participation)
  # all first users are new
  new_users$new <- new_users[, "annee_participation"] == years[1]
  # calculate next years 
  for (i in years[2:length(years)]){
    former <- new_users[new_users[, "annee_participation"] < i, "user_id"]
    current <- new_users[new_users[, "annee_participation"] == i, "user_id"]
    new_users[new_users[ , "annee_participation"] == i, "new"] <- !current %in% former
  }
  
  # get count per categories
  new_users_summary <- new_users %>%
    group_by(annee_participation, new) %>%
    summarise(nombre_users = n())
  
  # rename categories
  new_users_summary$type_user <- NA
  new_users_summary$type_user[new_users_summary$new] <- "Nouveaux utilisateurs"
  new_users_summary$type_user[!new_users_summary$new] <- "Utilisateurs ayant déjà participé"
  
  
  # plot results
  ggplot(new_users_summary, aes(x = annee_participation, y = nombre_users)) +
    geom_col(aes(fill = type_user)) +
    labs(fill="Type d'utilisateurs", 
         x = "Année scolaire", 
         y = paste(strwrap("Nombre d'utilisateurs ayant envoyé des données", width = 35), collapse = "\n")) +
    theme_minimal() +
    theme(legend.position="bottom") +
    theme_text_size +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
}

graph_evolution <- function(data_participation, variable_focale) {
  if(!variable_focale %in% c("participants", "observations")) stop("L'argument compter doit prendre l'une des valeurs suivantes : \"participants\", \"observations\"") 
  nombre_participants_an <- stats_globales(data_participation, compter = variable_focale, select_annee = "table")
  nombre_participants_an$annee_participation <- as.character(nombre_participants_an$annee_participation)
  
  label_graph <- switch (variable_focale,
                         participants = "Nombre de participants",
                         observations = "Nombre d'observations"
  )
  nombre_participants_an$annee_participation <- as.numeric(nombre_participants_an$annee_participation)
  ggplot(nombre_participants_an, aes(x = annee_participation, y = get(variable_focale), group = 1)) +
    geom_line(color = "#00CC33", linewidth= 1) +
    geom_point(color = "#00CC33", size = 2) +
    ylab(label_graph) +
    xlab("Années") +
    theme_minimal() +
    expand_limits(y=0) +
    theme_text_size 
}

# Carto ----



carte_dynamique <- function (geo_participation) {
  # Make dynamic map
  observation_map <- leaflet(geo_participation) %>%
    addTiles()%>%
    addMarkers(clusterOptions = markerClusterOptions())
  observation_map
}


carte_statique <- function (geo_participation) {
  # count each line within the shape layer
  participation_layer <- count_values_within(geo_participation, carte_grille_10k, remove_empty = TRUE, value_name = "nombre_observations")
  
  # start with background and layout
  static_map_bg <- tm_shape(carte_france) + tm_borders(col = "#70cc08") + tm_layout(frame = FALSE, legend.outside = TRUE)
  # shapefile operations
  static_map_shape <- static_map_bg + tm_shape(participation_layer)
  # add chloropeth
  static_map_chlo <- static_map_shape + tm_fill(col = "nombre_observations", legend.hist = FALSE, n = 10, style = "fisher", palette = "-viridis")
  
  static_map_chlo
}


# import maps
carte_france <- read_sf("data/maps/metropole-version-simplifiee.geojson")
carte_academie <- read_sf("data/maps/academies-version-simplifiee.geojson")
carte_region <- read_sf("data/maps/regions-version-simplifiee.geojson")
carte_departement <- read_sf("data/maps/departements-version-simplifiee.geojson")
carte_grille_10k <- sf::read_sf("data/maps/mailles10km.geojson")



#' Count values within
#'
#' @param geo_data sf object each line with geometry will be counted
#' @param geo_shape shape
#' @param remove_empty remove the empty geometries
#' @param value_name name the value counted
#'
#' @return a vector layer
#' @export
#'
#' @examples
count_values_within <- function(geo_data, 
                                geo_shape, 
                                remove_empty = FALSE, 
                                value_name = "ocurrence",
                                max_size = 30000) {
  
  if (nrow(geo_data) > max_size){
    for (i in 1:floor(nrow(geo_data)/max_size)){
      cat(i,'/',floor(nrow(geo_data)/max_size), "\n")
      subset <- geo_data[(1 + (i-1)*max_size):(max_size*i), ]
      
      # get the nomber of lines that are within each shape
      result_within <- st_within(subset, geo_shape, sparse = FALSE)
      subset_layer <- geo_shape %>%
        mutate(occurences = apply(result_within, 2, sum))
      
      if(i > 1){
        
        full_layer <- bind_rows(subset_layer, full_layer) %>%
          group_by(geometry) %>%
          summarise(occurences = sum(occurences))
      } else {
        full_layer <- subset_layer
      }
      gc()
    }
    
    if (nrow(geo_data) %% max_size > 0){
      subset <- geo_data[(1 + max_size*i):(max_size*i + nrow(geo_data) %% max_size), ]
      # get the nomber of lines that are within each shape
      result_within <- st_within(subset, geo_shape, sparse = FALSE)
      subset_layer <- geo_shape %>%
        mutate(occurences = apply(result_within, 2, sum))
      full_layer <- bind_rows(subset_layer, full_layer) %>%
        group_by(geometry) %>%
        summarise(occurences = sum(occurences))
    }
    
  } else {
    # get the nomber of lines that are within each shape
    result_within <- st_within(geo_data, geo_shape, sparse = FALSE)
    full_layer <- geo_shape %>%
      mutate(occurences = apply(result_within, 2, sum))
  }
  
  if (remove_empty) full_layer <- filter(full_layer, occurences > 0)
  colnames(full_layer)[colnames(full_layer) == "occurences"] <- value_name 

  
  return(full_layer)
}

# Graph help ----

# ggplot size 
theme_text_size <- theme( 
  axis.title = element_text(size = 15), 
  axis.text = element_text(size = 13), 
  legend.text = element_text(size = 12), 
  legend.title = element_text(size = 14))

# couleurs ----

# liste permettant un accès facile aux couleurs VNE
VNE_colors <- list(
  orangeVNE = "#fb883b",
  rougeVNE = "#ff6666",
  vertVNE = "#66cc33",
  violetVNE = "#ba6dba",
  bleubiolitVNE = "#23d2dd",
  jauneVNE = "#c2c62a",
  bleulichenVNE = "#5487ed",
  marronVNE = "#895f13",
  texteVNE = "#314C4C",
  vertclairVNE = "#ddeddd",
  grisUrbain = "#707176ff"
)

# order and labels ----

# vector containing the order for a year
month_order_short = c(
  "Sept", 
  "Oct",
  "Nov", 
  "Déc",
  "Jan", 
  "Fév", 
  "Mar", 
  "Avr", 
  "Mai", 
  "Juin", 
  "Juil", 
  "Août"
)

month_order_reg_short = c(
  "Jan", 
  "Fév", 
  "Mar", 
  "Avr", 
  "Mai", 
  "Juin", 
  "Juil", 
  "Août",
  "Sept", 
  "Oct",
  "Nov", 
  "Déc"
)

month_order = c(
  "Septembre", 
  "Octobre",
  "Novembre", 
  "Décembre",
  "Janvier", 
  "Février", 
  "Mars", 
  "Avril", 
  "Mai", 
  "Juin", 
  "Juillet", 
  "Août"
)
