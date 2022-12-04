# FONCTION CALCUL ----

library(dplyr)
library(tidyr)
library(ggplot2)

#indicateur
#' Title
#'
#' @param x a vector 
#' @param indicateur choose between "diversite" or "abondance"
#'
#' @return a single value index 
#' @export
#'
#' @examples
calcIndex <- function (x, indicateur){
  switch (indicateur,
          "diversite" = length(x[x>0]),
          "abondance" = sum(x, na.rm = TRUE)
  )
}


calc_school_year <- function (observation_date){
  observation_year <- ifelse(month(observation_date) %in% 1:8,
                             year(observation_date),
                             year(observation_date) + 1)
  observation_year
}

# test
# calc_school_year(c("2022/08/21","2022/09/21"))
# res = c(2022, 2023)


IC_VNE <- function(x) {1.96 * sd(x) / sqrt(length(x))}

# count the number of observation with values above 0
length0 <- function (x) length(x[x>0])

# remove 01_ from factors but keeps order
removeBeginingCategories <- function (input, Column){
  input <- data.frame(input)
  if (Column != "None"){
    Column <- as.numeric(Column)
    if(sapply(input[Column], class) == "factor" | sapply(input[Column], class) == "character"){
      if(is.character(sapply(input[Column], class))){
        input[ , Column] <- as.factor(input[ , Column])
      }
      levelsColumn <- levels(input[ , Column])
      if(any(grepl(pattern = "^[0-9][0-9]_", levelsColumn))){
        # lock order
        LevelToChange = grep(pattern = "^[0-9][0-9]_", levelsColumn)
        levelsColumn[LevelToChange] = substr(levelsColumn[LevelToChange], 4, nchar(levelsColumn[LevelToChange]))
        levels(input[ , Column]) <- levelsColumn
      }
    }
  }
  input
}




# add a value for the "annee scolaire"
# style
label_years <- function (x, retour = FALSE){
  yearMoinsUn <- as.numeric(x) - 1
  if (retour) paste0(yearMoinsUn, "\n", x) else paste(yearMoinsUn, " - ", x)
}

# add a value to order mounth according to the "annee scolaire"
# decrepated
convert_mounth <- function(x){
  x <- ifelse(x > 12 | x < 1, NA, x)
  moisOrdered = ifelse(x < 9, x + 4, x - 8)
  moisOrdered
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

# vector containing the order for a year
monthOrderShort = c(
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

monthOrder = c(
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

# filter date or not
# select year using all for the whole data base or a range in the following format "2012-2014" or "2019-2020"
# selects from the first of August to the 31st of July
# add test for date format
# strange behavior in some functions
selectYearRangeObs <- function (ImportResult, year = "all"){
  if (year != "all") {
    yearBegin = substr(year, 1, 4)
    yearEnd = substr(year, nchar(year)-3, nchar(year))
    ImportResult <- ImportResult %>%
      dplyr::filter(date_observation >= paste0(yearBegin, "-08-01") & date_observation <= paste0(yearEnd, "-07-31"))
  } else {
    ImportResult
  }
}

# filter date or not
# select year using all for the whole data base or a range in the following format "2012-2014" or "2019-2020"
# selects from the first of August to the 31st of July
# add test for date format
selectYearRange <- function (ImportResult, year = "all"){
  if (year != "all") {
    ImportResult <- ImportResult %>%
      dplyr::filter(annee_scolaire == year)
  } else {
    ImportResult
  }
}


getFrequence <- function(ImportResult, maxSpecies = 30, year = "all"){
  
  # select the date range
  if (year != "all")
    ImportResult <- filter(ImportResult, annee_scolaire == year)
  
  #calculate observation number
  ObservationNumber <- length(unique(ImportResult$num_observation))
  
  # Remove species without names
  # Maybe change to undetermined ???
  cleanedFromEmptySpecies <- ImportResult %>%
    dplyr::filter(!is.na(espece))
  
  #if number of individiual is indicated remove species without individuals
  if ("nombre_individus" %in% colnames(cleanedFromEmptySpecies)){
    cleanedFromEmptySpecies <- dplyr::filter(cleanedFromEmptySpecies, nombre_individus > 0)
  }
  
  #Calculate frequency
  SpeciesFreq <- cleanedFromEmptySpecies %>%
    dplyr::select(num_observation, espece, speciepk) %>%
    dplyr::distinct() %>%
    dplyr::group_by(espece, speciepk) %>%
    dplyr::summarise(Nombre = dplyr::n()) %>%
    dplyr::arrange(Nombre) %>%
    dplyr::mutate(Frequence = Nombre / ObservationNumber)
  
  #Remove species if too numerous (option) useful for "sauvages de ma rue"
  if (nrow(SpeciesFreq) > maxSpecies) SpeciesFreq <- SpeciesFreq[(nrow(SpeciesFreq)-(maxSpecies-1)):nrow(SpeciesFreq), ]
  
  #lock order in graph (descending frequency)
  SpeciesFreq$espece <- factor(SpeciesFreq$espece, levels = SpeciesFreq$espece)
  
  return(SpeciesFreq)
}


# Plot number of individuals
getIndivNumber <- function(ImportResult, maxSpecies = "all", year = "all"){
  
  # select the date range
  if (year != "all") ImportResult <- filter(ImportResult, annee_scolaire == year)
  
  # Remove species without names
  # Maybe change to undetermined ???
  cleanedFromEmptySpecies <- ImportResult %>%
    dplyr::filter(!is.na(espece)) %>%
    dplyr::filter(!is.na(nombre_individus)) 
  
  # calculate total number of individuals per species
  SpeciesNumber <- cleanedFromEmptySpecies %>%
    dplyr::group_by(espece, speciepk) %>%
    dplyr::summarise(Nombre = sum(nombre_individus)) %>%
    dplyr::arrange(Nombre)
  if (maxSpecies != "all"){
    #Remove species if too numerous (option) useful for "sauvages de ma rue"
    if (nrow(SpeciesNumber) > maxSpecies) SpeciesNumber <- SpeciesNumber[(nrow(SpeciesNumber)-(maxSpecies-1)):nrow(SpeciesNumber), ]
  }
  #lock order
  SpeciesNumber$espece <- factor(SpeciesNumber$espece, levels = SpeciesNumber$espece)
  return(SpeciesNumber)
}





# Nombre de
# all = toutes les valeurs incluses dans le calcul
# table = renvoie un tableau selon la variable
# valeur = filtrer
# compter classes, profs, etablissements
# Nombre de
# all = toutes les valeurs incluses dans le calcul
# table = renvoie un tableau selon la variable
# valeur = filtrer
# compter classes, profs, etablissements
stats_globales <- function(df, compter = "classes", selectAcademie = "all", selectAnnee = "all", selectProtocole = "all", selectTypeEtablissement = "all"){
  if (!compter %in% c("classes", "profs", "etablissements", "observations", "eleves")) stop("L'argument compter doit prendre l'une des valeurs suivantes : 'classes', 'profs', 'etablissements', 'observations'")
  variableACompter <- switch (compter,
                              classes = "groupepk",
                              profs = "userpk",
                              etablissements = "structurepk",
                              observations = "num_observation",
                              eleves = "groupepk"
  )
  
  # affect Ecole maternelle et élémentaire to Ecole élémentaire
  
  df$type_etablissement[df$type_etablissement == "Ecole maternelle et élémentaire"] <- "Ecole élémentaire"
  
  
  # choose the variable that will make the table
  if (any(selectAcademie == "table", 
          selectAnnee == "table", 
          selectProtocole == "table",
          selectTypeEtablissement == "table")){
    group = c()
    if (selectAcademie == "table") group = c(group, "academie")
    if (selectAnnee == "table") group = c(group, "annee_scolaire")
    if (selectProtocole == "table") group = c(group, "protocole")
    if (selectTypeEtablissement == "table") group = c(group, "type_etablissement")
  }
  
  # choose the variables that are fixed
  if (!selectAcademie %in% c("all", "table")) df = dplyr::filter(df, academie %in% selectAcademie)
  if (!selectAnnee %in% c("all", "table")){
    selectAnnee <- as.numeric(selectAnnee)
    df = dplyr::filter(df, annee_scolaire %in% selectAnnee)
  } 
  if (!selectProtocole %in% c("all", "table")) df = dplyr::filter(df, protocole %in% selectProtocole)
  if (!selectTypeEtablissement %in% c("all", "table")) df = dplyr::filter(df, type_etablissement %in% selectTypeEtablissement)
  
  # if no table function without group_by
  if (!"table" %in% c(selectAnnee, selectProtocole, selectAcademie, selectTypeEtablissement)){
    result <- df %>%
      select_at(all_of(variableACompter)) %>%
      distinct()%>%
      drop_na() %>%
      summarise(nombre = n())
    names(result) = compter
    result
  } else if (compter == "eleves") {
    result <- df %>%
      select(c(variableACompter, group, "effectifs")) %>%
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
      select(all_of(variableACompter)) %>%
      distinct()%>%
      drop_na() %>%
      summarise(nombre = n())
    result <- as.data.frame(result)
    names(result)[names(result) == 'nombre'] <- compter
    result
  }
}



# calcul de la moyenne, mediane, max, min de la diversite ou de l'abondance pour les protocoles
# ajouter possibilité de filtrer WIP type etablissement dans les requetes
# ajouter choix du format de sortie (liste par défaut)
# adaptation en fonction du protocole (warning aussi)
calculIndiceProtocole <- function(df, variable = FALSE, selectAcademie = "all", selectAnnee = "all", selectTypeEtablissement = "all", selectProtocole, tableau = FALSE){
  if (!selectAnnee =="all"){
    selectAnnee <- as.numeric(selectAnnee)
    df = dplyr::filter(df, annee_scolaire %in% selectAnnee)
  }
  
  if (!selectAcademie =="all"){
    df = dplyr::filter(df, academie %in% selectAcademie)
  }
  
  if (!selectTypeEtablissement =="all"){
    df = dplyr::filter(df, type_etablissement %in% selectTypeEtablissement)
  }
  
  
  if(selectProtocole %in% "Observatoire des Vers de Terre"){
    indexTable <- df %>%
      group_by(num_observation, num_quadrat) %>%
      summarise(diversite = length(nombre_individus[nombre_individus>0]),
                abondance = sum(nombre_individus, na.rm = TRUE))%>%
      ungroup()%>%
      group_by(num_observation) %>%
      summarise(diversite = mean(diversite),
                abondance = mean(abondance, na.rm = TRUE))
    
    
  } else if (selectProtocole == "Sauvages de ma rue") {
    indexTable <- df %>%
      group_by(num_observation) %>%
      summarise(diversite = n())
    indexTable$abondance <- 0
  } else if (selectProtocole %in% c("Lichen", "ALAMER")) {
    indexTable <- df %>%
      group_by(num_observation, speciepk) %>%
      summarise(abondance = sum(nombre_individus, na.rm = TRUE))%>%
      ungroup()%>%
      group_by(num_observation) %>%
      summarise(diversite = length(abondance[abondance > 0]),
                abondance = sum(abondance, na.rm = TRUE))
  } else {
    indexTable <- df %>%
      group_by(num_observation) %>%
      summarise(diversite = length(nombre_individus[nombre_individus>0]),
                abondance = sum(nombre_individus, na.rm = TRUE))
  }
  
  
  references <- indexTable %>%
    summarize_at(vars(diversite, abondance), list(~ quantile(.,probs = 0),
                                                  ~ quantile(.,probs = 0.25),
                                                  ~ quantile(.,probs = 0.5),
                                                  ~ quantile(.,probs = 0.75),
                                                  ~ quantile(.,probs = 0.98),
                                                  ~ quantile(.,probs = 1),
                                                  ~ mean(.)))  %>%
    gather("index_statistique", "Value", 1:14) %>%
    separate(index_statistique, into = c("index", "statistique"), sep = "_")
  references$label <- rep(c("min", "Q25", "medianne", "Q75", "Q98", "max", "moyenne"), each = 2)
  
  if (!tableau){
    references <- lapply(split(references, references$index), function(i) setNames(i$Value, i$label))
  }
  references
}



# print data.frame for JSON file and print graph
graphToJSONAndUpload <- function(graph, graphName, graphFolder, dossier_sorties = "sorties/",titre, commentaire, width = 3.5, height = 1.5){
  url_graph = paste0(graphFolder, graphName)
  print(url_graph)
  ggsave(file = paste0(dossier_sorties, graphName), plot = graph, width = width, height = height, bg = "transparent")
  
  graph_data <- data.frame(
    url_graph,
    titre,
    commentaire
  )
  graph_data
}

# make JSON from list of objects
# file name of the file to create do not forget .json (test)
# list list of object to transform in JSON
makeJSON <- function(file, list, dossier_sorties){
  json <- jsonlite::toJSON(list, auto_unbox = TRUE, pretty = TRUE)
  fileConn<-file(paste0(dossier_sorties, file))
  writeLines(json, fileConn)
  close(fileConn)
}

extract_json <- function(x) {
  extract <- jsonlite::fromJSON(x)
  extract_df <- do.call(rbind.data.frame, extract)
  tidyr::spread(extract_df, "label", "value")
}


# calcul le nombre de protocole effectué par les profs en tout !
nombreProtocoleEffectuesParProf <- function(df) {
  df <- na.omit(df)
  df <- df %>%
    select(id_prof, protocole) %>%
    distinct() %>%
    group_by(id_prof) %>%
    summarise(nb = n()) 
  table(df$nb)
}


# order, cut and add column with rank for in order to make a top with n values
# limit if tie values at the limit, not all will be taken...
makeTop <- function(df, varToRank, n = "all") {
  df <- data.frame(df)
  df [, varToRank] <- as.numeric(df[, varToRank])
  topDf <- df %>%
    arrange_at(vars(varToRank), list(desc))
  topDf$rang <- rank(desc(data.frame(topDf)[ , varToRank]), ties.method = "min")
  
  if (!n == "all"){
    topDf <- head(topDf, n)
  }
  topDf
}

#' Extraire des statistiques pour une variable
#'
#' @param df un data.frame
#' @param calcVar le nom d'une variable sous forme d'une chaine caractère
#' @param groupVar un vecteur des variables à utiliser pour faire des groupes
#'
#' @return Un data.frame contenant des résumés de données
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' summariseData(participation, calcVar = "nombre_individus", groupVar = FALSE)
#' summariseData(participation, calcVar = "nombre_individus", groupVar = c("protocole", "niveau"))
summariseData <- function (df, calcVar, groupVar = FALSE){
  colnamesIndex <- c("moyenne", "ecartType", "min", "quartile1",
                     "mediane", "quartile2", "quantile98", "max",
                     "nombre", "manquantes")
  
  if (groupVar[1] != FALSE){
    
    summarisedData <- df %>%
      group_by_at(groupVar) %>%
      summarise_at(vars(calcVar), list(~ mean(., na.rm = TRUE),
                                       ~ sd(., na.rm = TRUE),
                                       ~ quantile(.,probs = 0, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.25, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.5, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.75, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.98, na.rm = TRUE),
                                       ~ quantile(.,probs = 1, na.rm = TRUE),
                                       ~ length(.),
                                       ~ sum(is.na(.))))
    
    colnames(summarisedData) <- c(groupVar, colnamesIndex)
  } else {
    summarisedData <- df %>%
      summarise_at(vars(calcVar), list(~ mean(., na.rm = TRUE),
                                       ~ sd(., na.rm = TRUE),
                                       ~ quantile(.,probs = 0, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.25, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.5, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.75, na.rm = TRUE),
                                       ~ quantile(.,probs = 0.98, na.rm = TRUE),
                                       ~ quantile(.,probs = 1, na.rm = TRUE),
                                       ~ length(.),
                                       ~ sum(is.na(.))))
    colnames(summarisedData) <- colnamesIndex
  }
  summarisedData
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# FONCTIONS GRAPHIQUES ----
# La fonction serait plus propre avec 2 étapes
# La fonction serait plus propre avec 2 étapes
graphVariableFocale <- function (df, variableFocale, indice = "diversite", selectProtocole = "Non specifique"){
  
  if (variableFocale %in% c("distance_bois", "distance_prairie", "distance_champ")){
    
    df[ , variableFocale] <- ifelse(df[ , variableFocale] == "01_moins de 50 m", "01_moins de 500 m",df[ , variableFocale])
    df[ , variableFocale] <- ifelse(df[ , variableFocale] == "02_50 à 500 m", "01_moins de 500 m",df[ , variableFocale])
    df[ , variableFocale] <- ifelse(df[ , variableFocale] == "03_501 à 1000 m", "02_de 5001 à 2000 m",df[ , variableFocale])
    
    df[ , variableFocale] <- ifelse(df[ , variableFocale] ==  "04_1001 m à 2000 m", "02_de 5001 à 2000 m",df[ , variableFocale])
    df[ , variableFocale] <- ifelse(df[ , variableFocale] == "05_au-delà de 2 km", "03_au-delà de 2 km",df[ , variableFocale])
    df[ , variableFocale] <- ifelse(df[ , variableFocale] == "06_Non renseigné", NA,df[ , variableFocale])
    paletteGraphique = "gris"
    if (variableFocale == "distance_bois"){
      etiquetteAxeX <- "Distance au bois le plus proche"
    } else if (variableFocale == "distance_prairie"){
      etiquetteAxeX <- "Distance à la prairie la plus proche"
    } else if (variableFocale == "distance_champ"){
      etiquetteAxeX <- "Distance au champ le plus proche"
    } else {
      etiquetteAxeX = variableFocale
    }
  } else if (variableFocale == "environnement"){
    paletteGraphique = "environnement"
    etiquetteAxeX <- "Milieu d'observation"
  } else {
    etiquetteAxeX = variableFocale
    paletteGraphique = "environnement"
  }
  
  df <- df[!is.na(df[ , variableFocale]), ]
  
  
  
  if(selectProtocole == "Observatoire des Vers de Terre"){
    indexData <- df %>%
      group_by_at(c("num_observation", "num_quadrat", variableFocale)) %>%
      summarise(diversite = length(nombre_individus[nombre_individus>0]),
                abondance = sum(nombre_individus, na.rm = TRUE))%>%
      ungroup()%>%
      group_by_at(c("num_observation", variableFocale)) %>%
      summarise(diversite = mean(diversite),
                abondance = mean(abondance, na.rm = TRUE))
    
    
  } else {
    indexData <- df %>%
      group_by_at(c("num_observation", variableFocale)) %>%
      summarise(diversite = length(nombre_individus[nombre_individus>0]),
                abondance = sum(nombre_individus, na.rm = TRUE))
  }
  
  
  
  summaryData <- indexData %>%
    group_by_at(variableFocale) %>%
    summarise_at(vars(indice), list(moyenne = mean, intervalleDeConfiance = IC_VNE))
  
  summaryData <- removeBeginingCategories(input = summaryData, Column = 1)
  
  if (indice == "diversite"){
    etiquetteAxeY <- "Diversité"
  } else {
    etiquetteAxeY <- "Abondance"
  }
  
  
  
  
  ggplot(summaryData, aes_string(x = variableFocale, y = "moyenne"))+
    geom_col(aes_string(fill = variableFocale))+
    geom_errorbar(aes(ymin = moyenne + intervalleDeConfiance, ymax = moyenne - intervalleDeConfiance), width = .15)+
    ylab(etiquetteAxeY)+
    xlab(etiquetteAxeX)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_text(size = 10),
          axis.text.x=element_text(angle = 0, size = 7),
          axis.text.y=element_text(angle = 0, size = 7, hjust = .5),
          text = element_text(colour = "#314C4C"),
          panel.grid.minor = element_line(linewidth = 0.1), 
          panel.grid.major = element_line(linewidth = .2),
          legend.position = "none")+
    scale_fill_VNE(palette = paletteGraphique)
}

# Plot frequency of each species in observations
frequencePlot <- function(getFrequence){
  
  #plot result
  ggplot2::ggplot(getFrequence, ggplot2::aes(x = espece, y = Frequence)) +
    ggplot2::geom_col()+
    ggplot2::coord_flip()+
    ggplot2::xlab("Espèces")+
    ggplot2::ylab("Fréquence d'observation")
}

plotIndivNumber <- function(df){
  ggplot2::ggplot(df, ggplot2::aes(x = espece, y = Nombre)) +
    ggplot2::geom_col()+
    ggplot2::coord_flip()+
    ggplot2::xlab("Espèces")+
    ggplot2::ylab("Nombre d'individus")
}


# graphique de la participation par niveau
graphObservationsParNiveau <- function(donneesParticipation, selectProtocole = "all", selectAnnee = "all", selectAcademie = "all"){
  observationParNiveau <- stats_globales(df = donneesParticipation, selectAnnee = selectAnnee, selectProtocole = selectProtocole, selectTypeEtablissement = "table", selectAcademie = selectAcademie, compter = "observations")
  observationParNiveau$type_etablissement <- factor(observationParNiveau$type_etablissement, c("Ecole maternelle", "Ecole élémentaire", "Collège", "Lycée", "structure", "Autre"))
  graph_participation <- ggplot(observationParNiveau, aes(x = type_etablissement, y = observations)) +
    geom_col(aes(fill = type_etablissement))+
    theme_minimal() +
    scale_fill_manual("Legend", values = colorsType) +
    ylab("Nombre \n d'observations") +
    theme(legend.title=element_blank(), 
          axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(colour = "#314C4C"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  graph_participation
}

# localIndexResult = resultats locaux 
# references = donnees de reference
# selectProtocole = choix du protocol à représenter
# index = choix des indices / facteurs pas encore implémenté

referencePlot <- function (localIndexResult, references, selectProtocole, index = c("Diversite", "Abondance")) {
  
  # faire le graph des références
  protocoleLocalResult <- localIndexResult %>%
    dplyr::filter(protocole == selectProtocole)
  protocoleReference <- references %>%
    filter(protocole == selectProtocole)
  
  # ajouter un test pour les valeurs supérieures à 98%
  
  ggplot(protocoleLocalResult, aes(x = Diversite, y = Abondance)) +
    geom_vline(data = protocoleReference, aes(xintercept = Diversite, color = label), size = 1) +
    geom_hline(data = protocoleReference, aes(yintercept = Abondance, color = label), size = 1)+
    geom_point(alpha = .7) +
    ylim(NA, protocoleReference$Abondance[protocoleReference$label == "98%"])+
    xlim(NA, protocoleReference$Diversite[protocoleReference$label == "98%"])
}

# graph de restitution Oiseaux
# A transformer en function propre
graphRefMois <- function (df, indice = "diversite"){
  # quartile par mois
  indexTable <- df %>%
    group_by(num_observation, moisObs) %>%
    summarise(diversite = length(nombre_individus[nombre_individus>0]),
              abondance = sum(nombre_individus, na.rm = TRUE))
  
  # longueur de la variable focale pour les etiquettes
  longueurVariableFocale = length(unique(indexTable$moisObs))
  
  # table contenant les indicateurs
  references <- indexTable %>%
    group_by(moisObs) %>%
    summarize_at(vars(diversite, abondance), list(~ quantile(.,probs = 0),
                                                  ~ quantile(.,probs = 0.25),
                                                  ~ quantile(.,probs = 0.5),
                                                  ~ quantile(.,probs = 0.75),
                                                  ~ quantile(.,probs = 0.8),
                                                  ~ quantile(.,probs = 1),
                                                  ~ mean(.)))  %>%
    tidyr::gather("index_statistique", "Value", 2:15) %>%
    tidyr::separate(index_statistique, into = c("index", "statistique"), sep = "_")
  
  # ajout des étiquettes et ordre des mois
  references$label <- rep(c("min", "Q25", "medianne", "Q75", "Q98", "max", "moyenne"), each = 2*longueurVariableFocale)
  references$labelMounth <- label_mounth(references$moisObs, short = TRUE)
  references$orderMounth <- convert_mounth(references$moisObs)
  
  # sélection des étiquettes d'intéret
  references <- filter(references, label %in% c("Q25", "medianne", "Q75", "Q98"))
  
  # choix du style des lignes
  labelCol = rep(c(VNE_colors$vertVNE, VNE_colors$texteVNE, VNE_colors$bleubiolitVNE,"#FFFFFF"), each = longueurVariableFocale)
  typeLine = rep(c("solid", "dotted","dotted", "dotted"), each = longueurVariableFocale)
  alphaLine = rep(c("1", "1","1", "0"), each = longueurVariableFocale)
  
  # choix de l'indicateur
  references <- filter(references, index == indice)
  referenceFleche <- filter(references, orderMounth == 11)
  
  # a data frame with all the annotation info
  annotation <- data.frame(
    x = c(9, 12, 12),
    y = c((referenceFleche$Value[referenceFleche$label == "Q75"] - referenceFleche$Value[referenceFleche$label == "medianne"])/4 + referenceFleche$Value[referenceFleche$label == "medianne"],
          (referenceFleche$Value[referenceFleche$label == "Q75"] + referenceFleche$Value[referenceFleche$label == "medianne"])/2,
          (referenceFleche$Value[referenceFleche$label == "Q25"] + referenceFleche$Value[referenceFleche$label == "medianne"])/2),
    label = c("Médiane", "25 % des \n observations", "25 % des \n observations")
  )
  
  # Add text
  colorAnnotation = c(VNE_colors$vertVNE, "#314C4C", "#314C4C")
  if (indice == "diversite"){
    etiquetteAxeY <- "Diversité"
  } else {
    etiquetteAxeY <- "Abondance"
  }
  
  monthOrderShort <- monthOrderShort[-which(monthOrderShort == "Août")]
  
  # ajouter un test pour les valeurs supérieures à 98%
  ggplot(references, aes(x = labelMounth, y = Value, group = label)) +
    geom_line(color = labelCol, linetype = typeLine) +
    theme_minimal() +
    ylab(etiquetteAxeY) +
    scale_x_discrete(limits = monthOrderShort) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_text(size = 10),
          axis.text.x=element_text(angle = 0, size = 6),
          axis.text.y=element_text(angle = 0, size = 7, hjust = .5),
          text = element_text(colour = "#314C4C"),
          panel.grid.minor = element_line(size = 0.1), 
          panel.grid.major = element_line(size = .2)) +
    expand_limits(x = 16, y=0) +
    geom_segment(aes(x = 11, y = referenceFleche$Value[referenceFleche$label == "Q25"], xend = 11, yend = referenceFleche$Value[referenceFleche$label == "medianne"]), color='#314C4C', size=.2,arrow = arrow(length = unit(0.1, "cm"))) +
    geom_segment(aes(x = 11, y = referenceFleche$Value[referenceFleche$label == "medianne"], xend = 11, yend = referenceFleche$Value[referenceFleche$label == "Q25"]), color='#314C4C', size=.2,arrow = arrow(length = unit(0.1, "cm"))) +
    geom_segment(aes(x = 11, y = referenceFleche$Value[referenceFleche$label == "Q75"], xend = 11, yend = referenceFleche$Value[referenceFleche$label == "medianne"]), color='#314C4C', size=.2,arrow = arrow(length = unit(0.1, "cm"))) +
    geom_segment(aes(x = 11, y = referenceFleche$Value[referenceFleche$label == "medianne"], xend = 11, yend = referenceFleche$Value[referenceFleche$label == "Q75"]), color='#314C4C', size=.2,arrow = arrow(length = unit(0.1, "cm"))) +
    geom_text(data=annotation, aes( x=x, y=y, label=label),color = colorAnnotation,
              size= 1.9, fontface="bold", hjust = 0.1, lineheight = .8) +
    scale_color_VNE(palette = "medianeQuartile")+
    geom_rect(xmin = 3, ymin = 0, xmax = 7.5, ymax = 0)
}

graphLongueurRue <- function (df){
  
  df$longueurRue <- NA
  for (i in 1:nrow(df)){
    df$longueurRue[i] <- geosphere::distm(c(df$longitude_fin[i], df$latitude_fin[i]), c(df$longitude_debut[i], df$latitude_debut[i]), fun = distHaversine)
  }
  
  # somme de distance
  
  indexTableLong <- df %>%
    select(structurepk, annee_scolaire,longueurRue)%>%
    distinct()%>%
    group_by(structurepk, annee_scolaire) %>%
    summarise(longueurEchantillonnee = sum(longueurRue, na.rm = TRUE))  %>%
    filter(longueurEchantillonnee < 1000)
  
  
  # somme diversite
  
  indexTableDiv <- df %>%
    select(structurepk, annee_scolaire, espece) %>%
    distinct() %>%
    group_by(structurepk, annee_scolaire) %>%
    summarise(diversite = n())
  
  divLong <- inner_join(indexTableDiv, indexTableLong, by=c("structurepk","annee_scolaire"))
  
  ggplot(divLong, aes(x = longueurEchantillonnee, y = diversite))+
    geom_point(alpha = .3, size = .7)+
    geom_smooth(method = 'loess', span = .9, color = VNE_colors$vertVNE)+
    xlab("Longueur totale échantillonnée (m)")+
    ylab("Diversité")+
    theme_minimal()+
    theme(axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          axis.text.x=element_text(angle = 0, size = 6),
          axis.text.y=element_text(angle = 0, size = 7, hjust = .5),
          text = element_text(colour = "#314C4C"),
          panel.grid.minor = element_line(size = 0.1), 
          panel.grid.major = element_line(size = .2))
  
  
}


#graph_participation_par_mois(participation, anneeFocale = 2016)
# graph participation
graph_participation_par_mois <- function (df, anneeFocale = FALSE, selectProtocole = "all"){
  
  if (selectProtocole != "all") df <- filter(df, protocole == selectProtocole)
  
  obs <- df %>%
    filter(annee_scolaire != anneeFocale)%>%
    select(moisObs, annee_scolaire, num_observation)%>%
    distinct()%>%
    group_by(moisObs, annee_scolaire)%>%
    summarise(NombreObservations = n())
  
  obsNormalisee <- summariseData(obs, calcVar = "NombreObservations", groupVar = "moisObs")
  
  # reorder mounth
  obsNormalisee$moisObsLabel <- label_mounth(obsNormalisee$moisObs, short = TRUE)
  
  if (anneeFocale != FALSE){
    
    obsAnnee <- df %>%
      filter(annee_scolaire == anneeFocale) %>%
      select(moisObs, annee_scolaire, num_observation)%>%
      distinct()%>%
      group_by(moisObs) %>%
      summarise(NombreObservations = n())
    
    # reorder mounth
    obsAnnee$moisObsLabel <- label_mounth(obsAnnee$moisObs, short = TRUE)
  }
  
  couleursMinMax <- VNE_colors$vertclairVNE
  couleursQuartile <- VNE_colors$grisUrbain
  couleursMediane <- VNE_colors$texteVNE
  couleursAnnee <- VNE_colors$vertVNE
  
  graph <- ggplot(obsNormalisee, aes(x = moisObsLabel, group = 1)) +
    geom_ribbon(aes(ymin = quartile1, ymax = max), fill = couleursMinMax, alpha = 1) +
    #geom_ribbon(aes(ymin = quantile..2, ymax = quantile..4), fill = couleursQuartile, alpha = .6) +
    geom_line(aes(y = mediane), color = couleursMediane)
  
  if (anneeFocale != FALSE){
    graph <- graph +
      geom_point(data = obsAnnee, aes(y = NombreObservations), color = couleursAnnee) +
      geom_line(data = obsAnnee, aes(y = NombreObservations), color = couleursAnnee)
  }
  
  graph <- graph +    
    scale_x_discrete(limits = monthOrderShort)+
    ylab("Nombre de sessions")+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_text(size = 10),
          axis.text.x=element_text(angle = 0, size = 6),
          axis.text.y=element_text(angle = 0, size = 7, hjust = .5),
          text = element_text(colour = "#314C4C"),
          panel.grid.minor = element_line(size = 0.1), 
          panel.grid.major = element_line(size = .2))
  
  graph
}


## COULEURS ----

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


#' Function to extract VNE colors as hex codes
#'
#' @param ... Character names of VNE_colors 
#'
VNE_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (VNE_colors)
  
  VNE_colors[cols]
}


# palettes VNE
VNE_palettes <- list(
  `protocoles`  = VNE_cols("orangeVNE",
                           "rougeVNE",
                           "vertVNE",
                           "violetVNE",
                           "bleubiolitVNE",
                           "jauneVNE",
                           "bleulichenVNE",
                           "marronVNE"),
  `environnement` = VNE_cols("vertclairVNE",
                             "texteVNE",
                             "vertVNE"),
  `medianeQuartile` = VNE_cols("texteVNE",
                               "vertVNE",
                               "bleubiolitVNE"),
  
  
  `gris` = VNE_cols("vertclairVNE", "texteVNE")
)


#' Return function to interpolate a VNE color palette
#'
#' @param palette Character name of palette in VNE_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
VNE_pal <- function(palette = "protocoles", reverse = FALSE, ...) {
  pal <- VNE_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for VNE colors
#'
#' @param palette Character name of palette in VNE_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_VNE <- function(palette = "protocoles", discrete = TRUE, reverse = FALSE, ...) {
  pal <- VNE_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("VNE_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for VNE colors
#'
#' @param palette Character name of palette in VNE_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_VNE <- function(palette = "protocoles", discrete = TRUE, reverse = FALSE, ...) {
  pal <- VNE_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("VNE_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# vecteur definissant les couleurs pour les types d'etablissements
colorsType <- c("#B2B2B2", "#71B73A", "#314C4C", "#EC6669", "#FFFFFF")
names(colorsType) <- c("Ecole maternelle", "Ecole élémentaire", "Collège", "Lycée", "Autre" )


VNE_protocole_colors = function (x){
  hutils::Switch(x, DEFAULT = "#fb883b",
                 "Biolit" = VNE_colors$bleubiolitVNE, 
                 "Escargots des Jardins (Inventaire)" = VNE_colors$orangeVNE, 
                 "Observatoire des Vers de Terre" = VNE_colors$marronVNE,
                 "Oiseaux des Jardins" = VNE_colors$rougeVNE,
                 "Opération escargots" = VNE_colors$orangeVNE,
                 "Sauvages de ma rue" = VNE_colors$vertVNE,
                 "SPIPOLL" = VNE_colors$jauneVNE)
}

## RANDOM ----

VNE_protocoles <- list(oiseaux = "Oiseaux des Jardins",
                       sauvages = "Sauvages de ma rue",
                       spipoll = "SPIPOLL",
                       escargots = "Opération escargots",
                       "Escargots des Jardins (Inventaire)",
                       vdt = "Observatoire des Vers de Terre",
                       biolit = "Biolit",
                       chiro = "Vigie-Chiro",
                       alamer = "ALAMER",
                       lichens = "Lichen Go")


#ligne de test maintenue par jeu 
Sebastien <- "très gentil"
Nora <- "Dame Nora !"
Greg <- "Encore très gentil"


