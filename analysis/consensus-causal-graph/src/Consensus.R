# adapted from https://github.com/smlgfiuedu/Consensus-Graph/tree/main
library(bnlearn)
library(openxlsx)
library(dplyr)

# @pathname: path to structures output from getStructures.cpp
# @nodes: vector of node names in network
# @exfilename: name of excel sheet output
sumTable <- function (pathname = "structures.txt", node.names = c(), exfilename = "structures_consensus.xlsx"){
    cat("File: ", pathname, "\n")
    vertices <- as.character(seq(0,length(node.names)-1),1)
    suminput <- read.table(pathname, header = FALSE)
    suminput <- na.omit(suminput)
    sumResults <- data.frame()
    
    n <- ncol(suminput)
    order.string <- vector()
    
    for(l in 1:nrow(suminput)){
      tmp <- suminput[l,1]
      for(m in 2:(n-4)){
        tmp <- paste(tmp, suminput[l,m], sep = " ")
      }
      order.string <- append(order.string, tmp)
    }
    suminput <- cbind(suminput, order.string)
    suminput <- suminput[-n]
    
    for(j in (n-4):1){
      suminput <- suminput[,-j]
    }
    colnames(suminput) <- c("OrderScores", "Structures", "StructScores", "Orders")

    # compute mean order score for each unique order string and sort by score
    orders <- aggregate(OrderScores~Orders, data = suminput, FUN = mean)
    orders <- orders[order(-orders[,2]), ]
    
    # calculate % of total order score and cumulative percentage
    for(i in 1:nrow(orders)){
      orders$Opercentage[i] <- exp(-log(sum(exp(orders$OrderScores-orders$OrderScores[i]))))
    }
    orders$Ocumper = cumsum(orders$Opercentage)
    
    # calculate % of total structures score and cumulative percentage
    for(i in 1:nrow(orders)){
      tmp <- suminput[which(suminput$Orders == orders[i,1]), ]
      
      tmp$Opercentage <- orders[i,3]
      tmp$Ocumper <- orders[i,4]
      
      for(j in 1:nrow(tmp)){
        tmp$Spercentage[j] <- exp(-log(sum(exp(tmp$StructScores-tmp$StructScores[j]))))
      }
      tmp$Scumper <- cumsum(tmp$Spercentage)
      tmp <- tmp[c("Orders", "OrderScores", "Opercentage","Ocumper", "Structures", "StructScores","Spercentage","Scumper")]
      sumResults <- rbind(sumResults, tmp)
    }
    
    
    for(k in 1:nrow(sumResults)){
      sumResults$sortedStructure[k] <- .sortStructure(as.character(sumResults$Structures[k]))
    }

    rl = .generateCombinations(vertices,nrow(sumResults))
    sumResults= cbind(sumResults, rl)
    
    
    
    combinations <- combn(vertices,2)
    
    for(i in 1:nrow(sumResults)){
      struct <- as.character(sumResults [i,5])
      relation <- .getRelation(struct)
      
      for(j in 1:ncol(combinations)){
        X <- combinations[1,j]
        Y <- combinations[2,j]
        c1 <- paste0(X, "->", Y)
        c2 <- paste0(Y, "->", X)
        c3 <- paste(X, "NA", Y)
        
        if (c1 %in% relation){
          sumResults[i,c1] <- sumResults$Opercentage[i]*sumResults$Spercentage[i]
        }
        else if(c2 %in% relation){
          sumResults[i,c2] <- sumResults$Opercentage[i]*sumResults$Spercentage[i]
        }
        else{
          sumResults[i,c3] <- sumResults$Opercentage[i]*sumResults$Spercentage[i]
        }
      }
    }
    
    sumP <- colSums(sumResults[, -c(1:9)])
    
    ##########################################
    
    StruWithO = aggregate(StructScores~sortedStructure, data = sumResults, FUN = mean)
    
    for(l in 1:nrow(StruWithO)){
      StruWithO$Spercentage[l] <- exp(-log(sum(exp(StruWithO$StructScores-StruWithO$StructScores[l]))))
    }
    
    StruWithO <- StruWithO[order(-StruWithO[,3]), ]

    s.combinations <- .generateCombinations(vertices, nrow(StruWithO))
    StruWithO <- cbind(StruWithO, s.combinations)
    
    for(m in 1:nrow(StruWithO)){
      o.struct <- as.character(StruWithO [m,1])
      s.rel <- .getRelation(o.struct)
      
      
      for(n in 1:ncol(combinations)){
        X <- combinations[1,n]
        Y <- combinations[2,n]
        c1 <- paste0(X, "->", Y)
        c2 <- paste0(Y, "->", X)
        c3 <- paste(X, "NA", Y)
        
        if (c1 %in% s.rel){
          StruWithO[m,c1] <- StruWithO$Spercentage[m]
        }
        else if(c2 %in% s.rel){
          StruWithO[m,c2] <- StruWithO$Spercentage[m]
        }
        else{
          StruWithO[m,c3] <- StruWithO$Spercentage[m]
        }
      }
    }
    sumPS <- colSums(StruWithO[, -c(1:3)])
    namesP <- names(sumP)

    for(i in 1:length(namesP)){
      if (grepl("->", namesP[i])){
        nodes <- strsplit(namesP[i], "->")
        a <- as.numeric(nodes[[1]][1])
        b <- as.numeric(nodes[[1]][2])
        namesP[i] = paste(node.names[a+1], "->", node.names[b+1], sep = "")
      }
      
      if (grepl(" NA ", namesP[i])){
        nodes <- strsplit(namesP[i], " NA ")
        a <- as.numeric(nodes[[1]][1])
        b <- as.numeric(nodes[[1]][2])
        namesP[i] <- paste(node.names[a+1], " NA ", node.names[b+1], sep = "")
      }
      
    }
    
    names(sumP) <- namesP
    names(sumPS) <- namesP
    
    sumP <- as.data.frame(sumP)
    sumPS <- as.data.frame(sumPS)
    
    sum.out <- list(sumP, sumPS)
    write.xlsx(sum.out, exfilename, sheetName = c("Pairs", "Pairs-No-Order"), colNames = TRUE, rowNames = TRUE, append = FALSE)
    
    return(sum.out)
}

# @pathname: path to excel file generated from sumTable
# @sheet.name: sheet name of excel file to be used 
# @str: structure to be averaged
# @node.names: vector of node names/labels
# @outfile: name of graphics output for use in dot
AverageNetwork <- function(pathname = "structures_consensus.xlsx", sheet.name = "Pairs",
                        str = "",  node.names = c(),
                        outfile = "out.gv"){
  
  
  res <- read.xlsx(pathname, sheet = sheet.name)
  res <- na.omit(res)
  
  names(node.names) <- as.character(seq(0,length(node.names)-1),1)
  
  strD <- model2network(as.character(str))
  arcD <- arcs(strD)
  nodeD <- nodes(strD)
  arNo <- unique(union(arcD[,1], arcD[,2]))
  nod <- setdiff(nodeD, arNo)
  
  cat("digraph G {", file=outfile, sep="\n")
  
  if (length(nod) > 0){
    for(j in 1:length(nod)){
      cat("\t", node.names[nod[j]], "\n", file=outfile, append=TRUE)
    }
  }
  
  for(i in 1:nrow(arcD)){
    node.A <- node.names[arcD[i,1]]
    node.B <- node.names[arcD[i,2]]
    
    mn <- paste(node.A,"->" , node.B, sep = "")
    mn2 <- paste(node.B,"->" , node.A, sep = "")
    s1 <- res[which(res[,1]==mn),2]
    s2 <- res[which(res[,1]==mn2),2]
    
    if((s1>=0.9999)&(s2<=0.0001)){
      
      cat("\t", mn, " [penwidth=", round(s1*10, digits = 2), ",", 'label="', ">99", " (", "~0",')"', "];", "\n", file=outfile, append=TRUE)
      
    } else if (s1>=0.9999) {
      
      cat("\t", mn, " [penwidth=", round(s1*10, digits = 2), ",", 'label="', ">99", " (", round(s2*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
      
    } else if (s2<=0.0001) {
      
      cat("\t", mn, " [penwidth=", round(s1*10, digits = 2), ",", 'label="', round(s1*100 ,digits = 2), " (", "~0",')"', "];", "\n", file=outfile, append=TRUE)
      
    } else {
      
      cat("\t", mn, " [penwidth=", round(s1*10, digits = 2), ",", 'label="', round(s1*100 ,digits = 2), " (", round(s2*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
      
    }
    
    
    
  }
  
  NoNA <- data.frame()
  
  arcD <- as.data.frame(arcD)
  combinations <- combn(nodeD,2)
  
  
  for(j in 1:ncol(combinations)){
    X <- combinations[1,j]
    Y <- combinations[2,j]
    
    pair_find1 <- data.frame(from = X, to = Y)
    pair_find2 <- data.frame(from = Y, to = X)
    
    if((nrow(merge(pair_find1,arcD )) == 0) & (nrow(merge(pair_find2,arcD )) == 0)){
      node.X <- node.names[X]
      node.Y <- node.names[Y]
      
      TX <- paste0(node.X,"->", node.Y)
      TY <- paste0(node.Y,"->", node.X)
      t1 <- res[which(res[,1]==TX),2]
      t2 <- res[which(res[,1]==TY),2]
      
      if(t1 > t2){
        
        if((t1>=0.9999)&(t2<=0.0001)){
          
          cat("\t", TX, " [penwidth=", round(t1*10, digits = 2), ",", 'label="', ">99", " (", "~0",')"', "];", "\n", file=outfile, append=TRUE)
          
        } else if (t1>=0.9999) {
          
          cat("\t", TX, " [penwidth=", round(t1*10, digits = 2), ",", 'label="', ">99", " (", round(t2*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
          
        } else if (t2<=0.0001) {
          
          cat("\t", TX, " [penwidth=", round(t1*10, digits = 2), ",", 'label="', round(t1*100 ,digits = 2), " (", "~0",')"', "];", "\n", file=outfile, append=TRUE)
          
        } else {
          
          cat("\t", TX, " [penwidth=", round(t1*10, digits = 2), ",", 'label="', round(t1*100 ,digits = 2), " (", round(t2*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
          
        }
        
      }
      else if (t2 > t1){
        
        if((t2>=0.9999) & (t1<=0.0001)){
          
          cat("\t", TY, " [penwidth=", round(t2*10, digits = 2), ",", 'label="', ">99", " (", "~0",')"', "];", "\n", file=outfile, append=TRUE)
          
        } else if (t2>=0.9999) {
          
          cat("\t", TY, " [penwidth=", round(t2*10, digits = 2), ",", 'label="', ">99", " (", round(t1*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
          
        } else if (t1<=0.0001) {
          
          cat("\t", TY, " [penwidth=", round(t2*10, digits = 2), ",", 'label="', round(t2*100 ,digits = 2), " (", "~0",')"', "];", "\n", file=outfile, append=TRUE)
          
        } else {
          
          cat("\t", TY, " [penwidth=", round(t2*10, digits = 2), ",", 'label="', round(t2*100 ,digits = 2), " (", round(t1*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
          
        }
        
      }
      else{
        next
      }
      
      
    }
    
    
  }
  
  cat("\t", 'label="', '      ', "\\l", "Notes:", "\\l", "    -   >99 represents percentage of 99.99 and above", "\\l", "    -   ~0  represents percentage of 0.01 and below", "\\l", "    -   Percentage in the parentheses represents the percentage of an arc that is reversed", '\\l"', ";", "\n", file = outfile, append = TRUE)
  cat("}", file = outfile, append = TRUE)
}

# helper functions --------------------------------------------------------
.sortStructure <- function(structure){
  sorted.structure = ""
  tmp.struct <- substr(structure, 2, nchar(structure)-1)
  stlist <- strsplit(tmp.struct, "\\]\\[")
  stlist <- sort(stlist[[1]])
  
  for(i in 1:length(stlist)){
    if(nchar(stlist[i])>3){

      tmp.child <- strsplit(stlist[i], "\\|")
      child <- tmp.child[[1]][1]
      
      parents <- strsplit(tmp.child[[1]][2], ":")
      parents <- sort(parents[[1]])
      
      # reconstruct structure based on sorting
      stlist[i] <- parents %>%
        paste(., collapse=":") %>%
        paste(child, "|", ., sep = "") 
    }
    
  }
  
  sorted.structure <- stlist %>%
    paste(., collapse="][") %>%
    paste("[", ., "]", sep = "") 
  
  return(sorted.structure)
}

.generateCombinations <- function(vertices, n.observations){
    comb.names <- c()
    combinations <- combn(vertices,2)
    for(i in 1:ncol(combinations )){
      X <- combinations[1,i]
      Y <- combinations[2,i]
      
      c1 <- paste(X, "->", Y, sep = "")
      comb.names <- append(comb.names,c1)
      
      c2 <- paste(Y, "->", X, sep = "")
      comb.names <- append(comb.names,c2)
      c3 <- paste(X, " NA ", Y, sep = "")
      comb.names <- append(comb.names,c3)
    }
    
    re <- data.frame(matrix(0, nrow=n.observations, ncol=ncol(combinations)*3))
    colnames(re) <- comb.names
    
    return(re)
  }

.getRelation = function(structure){
    relation <- c()
    tmp.struct <- substr(structure, 2, nchar(structure)-1)
    stlist <- strsplit(tmp.struct, "\\]\\[")
    
    
    for(i in 1:length(stlist[[1]])){
      tmp <- chartr(old = "|", new = "g", stlist[[1]][i])
      
      if (grepl("g", tmp)){
        tmp <- strsplit(tmp, "g")
        
        child <- tmp[[1]][1]
        parent <- strsplit(tmp[[1]][2], ":")
        
        for(j in 1:length(parent[[1]])){
          p.node <- parent[[1]][j]
          
          rel <- paste(p.node,"->", child, sep = "")
          relation <- append(relation, rel)
        }
      }
    }
    return(relation)
  }