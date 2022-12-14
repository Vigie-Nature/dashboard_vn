#import functions
source("fonctions/functions_restitution.R")

rmarkdown::render_site()

# import data
data_participation_ini <- data.table::fread("data/data_vn.csv")
table(data_participation_ini$protocole, data_participation_ini$observatoire)
annee_actuelle <- lubridate::year(Sys.time())

pages <- data.frame(observatory = c("oab", "oab",      "oab"     , "oab"    , "oab", 
                                    "ENI", "ENI", "ENI", "ENI", "ENI", "ENI", 
                                    "gestionnaires", "gestionnaires", "gestionnaires",
                                    "Vigie-Chiro", "Vigie-Chiro", "Vigie-Chiro", "Vigie-Chiro",
                                    "spipoll", "plages", "Vigie-flore"),
                    protocol =    c("all", "nichoir", "placette", "planche", "transect", 
                                    "all", "Coléoptère", "Flore", "Oiseau", "Vers de terre - BECHE", "Vers de terre - MOUTARDE",
                                    "all", "Florilèges", "Propage",
                                    "all", "PEDESTRE", "POINT_FIXE", "ROUTIER",
                                    "all",     "all",    "all"))

# filter data and render page
for (i in 1:nrow(pages)){
  print(pages[i, ])
  data_participation_filtered <- dplyr::filter(data_participation_ini, observatoire == pages$observatory[i])
  if (!pages$protocol[i] == "all") data_participation_filtered <- filter(data_participation_filtered, protocole == pages$protocol[i])
  file_name  <- stringi::stri_trans_general(str = pages$protocol[i], id = "Latin-ASCII")
  file_name <- tolower(file_name)
  file_name <- gsub(" ", "", file_name)
  rmarkdown::render("templates/participation_page.Rmd", params = list(
    data_set = data_participation_filtered,
    observatory_name = pages$observatory[i],
    protocol_names = pages$protocol[i]),
    output_file = paste0("../_site/", pages$observatory[i], "_", file_name, ".html"))
}
