library(sf)
library(dplyr)
path_to_file <- "../export_dashboard/"

files_to_merge <- list.files(path_to_file)
files_to_merge <- files_to_merge[grepl(pattern = ".csv", files_to_merge)]

# write metadata

for (i in seq_along(files_to_merge)){
  data_import <- data.table::fread(paste0(path_to_file, files_to_merge[i]), dec = ",")
  #add protocol
  data_import$observatoire <- stringr::str_split(files_to_merge[i], pattern = "_")[[1]][1]
  
  if("agregat_protocole" %in% colnames(data_import)) {
    colnames(data_import)[colnames(data_import) == "agregat_protocole"] <- "protocole"
  }
  
  data_import$user_id <- paste0(data_import$user_id, data_import$observatoire[1])
  data_import$participation_id <- paste0(data_import$participation_id, data_import$observatoire[1])
  
  if (class(data_import$user_id) == "integer") {
    data_import$user_id <- as.character(data_import$user_id)
  }
  
  if (class(data_import$participation_id) == "integer") {
    data_import$participation_id <- as.character(data_import$participation_id)
  }
  
  if (class(data_import$latitude) == "character") {
    data_import$latitude <- as.numeric(data_import$latitude)
    data_import$longitude <- as.numeric(data_import$longitude)
  }
  
  if(data_import$observatoire[1] %in% c("ENI", "gestionnaires", "plages" )){
    data_import$latitude_ <- data_import$latitude
    data_import$latitude <- data_import$longitude
    data_import$longitude <- data_import$latitude_
  }
  
  if (grepl(pattern = "/",data_import$participation_date[1])){
    data_import$participation_date <- lubridate::parse_date_time(data_import$participation_date,orders = "dmy")
  } 
  
  data_import$participation_date <- as.POSIXct(data_import$participation_date, format="%Y-%m-%d")
  
  
  
  
  if (i > 1){
    data_all <- bind_rows(data_all, data_import)
  } else {
    data_all <- data_import
  }
}

data_all$annee_participation <- lubridate::year(data_all$participation_date)
data_all$mois_participation <- lubridate::month(data_all$participation_date)

data.table::fwrite(data_all, "data/data_vn.csv")
