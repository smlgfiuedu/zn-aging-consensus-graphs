# utility functions for Order Search and Structures algorithms


# generate preprior config file input for ordersearch: MCMC Prescreen.cpp --------
generatePrepriorConfig <- function(configFile = "preprior.config",
                                   data = NA, DataFile = "", TotalVariables = ncol(data), PriorFile = "NONE", 
                                   MaximumCategory = apply(data, 2, max), MinimumCategory = apply(data, 2, min), MaximumTime = NA,
                                   MaximumParents = 4, EpsilonDifference = "0.000001", StartingOption = "Y",
                                   StartingOrder = 0:(ncol(data)-1)){
  
  cat("Creating config file for", basename(DataFile), "\n")
  if(is.na(MaximumTime)| !is.numeric(MaximumTime)){
    stop("Must provide a valid MaximumTime in hours")
  }
  if(StartingOption == "n"){
    StartingOrder = ""
  }else if(StartingOption == "Y"){
    StartingOrder <- paste(StartingOrder, collapse = " ")
  }else{
    stop("Must provide a valid StartingOption: Y or N")
  }

  config_opts <- list(DataFile = DataFile,
                      TotalVariables = TotalVariables,
                      PriorFile = PriorFile,
                      MaximumCategory = paste(MaximumCategory, collapse = " "),
                      MinimumCategory = paste(MinimumCategory, collapse = " "),
                      MaximumTime = MaximumTime,
                      MaximumParents = MaximumParents,
                      EpsilonDifference = EpsilonDifference,
                      StartingOption = StartingOption,
                      StartingOrder = StartingOrder)
  
  fw <- file(configFile, open = "wt")
  
  for (c in 1:length(config_opts)){
    
    line <- paste(names(config_opts)[c], "=", config_opts[[c]])
    writeLines(line, fw)
  }
  close(fw)
  return(config_opts)
}
