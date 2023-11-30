# generate the config files for use in GetStructures.cpp for the three representative networks
source("./analysis/consensus-causal-graph/src/utils_structures.R")
set.seed(11)

organism <- "human"
dataset <- paste0(organism, "_representative")
data.in <- paste0("./analysis/consensus-causal-graph/", organism, "/ordersearch/input/", dataset, ".txt")
osearch.results.path <- paste0("./analysis/consensus-causal-graph/", organism, "/ordersearch/results/", dataset, "_4h_mcmc_prescreen_results_2023.11.16.txt")
structmb.config.out <- paste0("./analysis/consensus-causal-graph/", organism, "/structures/configs/", tolower(dataset), ".structmb.config")
config.dir <- paste0("./analysis/consensus-causal-graph/", organism, "/structures/configs/")

dir.create(config.dir, recursive = TRUE)
data <- read.table(data.in, header = FALSE)
best.order <- extractBestOrder(results.path = osearch.results.path, nvariables = ncol(data))


generateStructConfig(configFile = structmb.config.out, data = data,
                             DataFile = sub("\\.", "/home/samanthagonzales/zinc_aging", data.in),
                             Order = best.order,
                             OutputFileName = paste0("/home/samanthagonzales/zinc_aging/consensus-causal-graph/", organism, "/structures/out/", tolower(dataset), "_struct_results.txt"))
