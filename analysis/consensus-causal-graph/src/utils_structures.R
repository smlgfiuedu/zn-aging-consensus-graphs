extractBestOrder <- function(results.path, nvariables){
  fr <- file(results.path, open = "rt")
  lines <- readLines(fr)
  
  for (l in 1:length(lines)){
    line <- lines[l]
    start.output <- "The total amount of unique iterations"
    isBeginOutput <- grepl(start.output, line)
    if(isBeginOutput){
      output.line <- lines[l+2]
      break
    }
  }
  ol <- strsplit(output.line, split = " ")[[1]][1:nvariables]
  return(ol)
}

# Generate config file for GetStructure.cpp -------------------------------
generateStructConfig <- function(configFile = "structmb.config",
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