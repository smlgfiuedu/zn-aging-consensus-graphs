#convert csv to text and numerically code data
organism <- "mouse"
organism.data <- paste0("./analysis/bnlearn/", organism, "/", organism, "_goi_subset_data")


# replace the csv with tsv
organism.csv <- read.csv(paste0(organism.data, ".csv"))
original <- organism.csv
for(g in 1:ncol(organism.csv)){
  if(g==1){
    
    organism.csv[,g] <- factor(organism.csv[,g], levels = c("Baseline", "Fasted"))
    
  }else{
    
    organism.csv[,g] <- factor(organism.csv[,g], levels = c("None", "Low", "High"))    
  }
}
organism.csv <- as.data.frame(lapply(organism.csv, FUN = function(x) as.numeric(x)-1))
nodes <- colnames(organism.csv)

write.table(organism.csv, file = paste0("./analysis/consensus-causal-graph/", organism, "/ordersearch/input/", organism, "_representative.txt"), sep = "\t", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(nodes, file = paste0("./analysis/consensus-causal-graph/", organism, "/", organism, "_node_names.txt"), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
cat("File can be found in:", paste0("./analysis/consensus-causal-graph/", organism, "/ordersearch/input/", organism, "_representative.txt"))