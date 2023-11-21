# generate the config files for use in MCMC Prescreening for the three representative networks
source("./analysis/consensus-causal-graph/src/utils_ordersearch.R")
set.seed(11)

organism <- "mouse"
dataset <- paste0(organism, "_representative")
data.in <- paste0("./analysis/consensus-causal-graph/", organism, "/ordersearch/input/", dataset, ".txt")
max.time <- 4

data <- read.table(data.in, header = FALSE)
starting.order <- sample(0:(ncol(data)-1))

config.dir <- paste0("./analysis/consensus-causal-graph/", organism, "/ordersearch/configs/")
dir.create(config.dir)
config.out <- paste0(config.dir, tolower(dataset), ".", max.time, "h.preprior.config")


config <- generatePrepriorConfig(configFile = config.out,
                                 MaximumTime = max.time,
                                 data = data,
                                 DataFile = sub("\\.", "/home/samanthagonzales/zinc_aging", data.in),
                                 StartingOption = "Y",
                                 StartingOrder = starting.order)

cat("Config file created in: ", config.out, "\n")