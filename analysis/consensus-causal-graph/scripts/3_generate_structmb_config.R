# generate the config files for use in GetStructure.cpp for the alarm datasets


# functions ---------------------------------------------------------------
generate_config_file <- function(configFile = "structmb.config",
                                 data = NA, DataFile = "", TotalVariables = ncol(data), 
                                 MaximumCategory = apply(data, 2, max), MinimumCategory = apply(data, 2, min),
                                 MaximumParents = 4, Order = 0:(ncol(data)-1), Percentage = "99.0",
                                 OutputFileName = "output.structures.txt"){
  
  cat("Creating config file for", basename(DataFile), "\n")
  config_opts <- list(DataFile = DataFile,
                      TotalVariables = TotalVariables,
                      MaximumCategory = paste(MaximumCategory, collapse = " "),
                      MinimumCategory = paste(MinimumCategory, collapse = " "),
                      MaximumParents = MaximumParents,
                      Order = paste(Order, collapse = " "),
                      Percentage = Percentage,
                      OutputFileName = OutputFileName)
  
  fw <- file(configFile, open = "wt")
  
  for (c in 1:length(config_opts)){
    
    line <- paste(names(config_opts)[c], "=", config_opts[[c]])
    writeLines(line, fw)
  }
  close(fw)
  return(config_opts)
}


# single dataset ----------------------------------------------------------
dataset <- "D50S9v2"
data_in <- paste0("./data/", dataset, ".txt")
config_out <- paste0("./analysis/structures/configs/", tolower(dataset), ".structmb.config")
data <- read.table(data_in, header = TRUE)

test <- generate_config_file(configFile = config_out, data = data,
                     DataFile = paste0("/home/samanthagonzales/order_search_cbn/data/", dataset, ".txt"),
                     MaximumCategory = c(2, 1, 2, 3, 2, 2, 1, 3, 3),
                     Order = c(6, 5, 8, 7, 3, 1, 2, 0, 4),
                     OutputFileName = paste0("/home/samanthagonzales/order_search_cbn/analysis/structures/", tolower(dataset), "_structures.txt"))



# multiple datasets -------------------------------------------------------
# datasets <- list.files("./data", pattern = ".txt",)
# max_time <- 1
# orders <- list(d1kc9v = c(),
#                d1ks9v2 = c(),
#                d50c9v = c(8, 0, 7, 5, 6, 1, 3, 2, 4),
#                d50s9v2 = c(6, 5, 8, 7, 3, 1, 2, 0, 4))
# for (ds in datasets){
#   ds <- gsub(pattern = ".txt", replacement = "", ds)
#   
#   data_in <- paste0("./data/", ds, ".txt")
#   config_out <- paste0("./analysis/orderscore/configs/", tolower(ds), ".", max_time, "h.preprior.config")
#   data <- read.table(data_in, header = TRUE)
#   
#   path_to_file <- paste0("/home/samanthagonzales/order_search_cbn/analysis/orderscore/input/", ds, ".txt")
#   generate_config_file(configFile = config_out, data = data, MaximumTime = max_time,
#                        DataFile = path_to_file)
#   
# }
