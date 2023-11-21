#source("/home/zgong001/Documents/Alarm/D50C9v/RCode/CombineOrders.R")
getRelation = 
  function(st = "[6][8][5][7|6:8:5][1|7][3|7:1][4|1][2|4][0|2]"){
    re = c()
    temstr = substr(st, 2, nchar(st)-1)
    stlist = strsplit(chartr(old = "][", new = "##", temstr), "##")
    for(i in 1:length(stlist[[1]])){
      temc = chartr(old = "|", new = "g", stlist[[1]][i])
      
      if (grepl("g", temc)){
        temc2 = strsplit(temc, "g")
        
        X = temc2[[1]][1]
        TY = strsplit(temc2[[1]][2], ":")
        for(j in 1:length(TY[[1]])){
          Y = TY[[1]][j]
          
          cc= paste(Y,"->", X, sep = "")
          re = append(re, cc)
        }
      }
    }
    return(re)
  }

creatDataframe = 
  function(v = c("0", "1","2","3","4","5","6","7","8"), r = 246){
    cn = c()
    co= combn(v,2)
    for(i in 1:ncol(co)){
      X = co[1,i]
      Y = co[2,i]
      c1 = paste(X, "->", Y, sep = "")
      cn = append(cn,c1)
      c2 = paste(Y, "->", X, sep = "")
      cn = append(cn,c2)
      c3 = paste(X, " NA ", Y, sep = "")
      cn = append(cn,c3)
    }
    
    b= ncol(co)
    
    re = data.frame(matrix(0, nrow=r, ncol=b*3))
    colnames(re) = cn
    
    return(re)
  }

#---------------------------------------------------------------------------------
sortStru = function(st = "[5][6][8][7|5:8][1|5:7][3|7:8][2|6:1][4|1][0|2]"){
  library(dplyr)
  re = ""
  temstr = substr(st, 2, nchar(st)-1)
  stlist = strsplit(chartr(old = "][", new = "##", temstr), "##")
  stlist = sort(stlist[[1]])
  for(i in 1:length(stlist)){
    if(nchar(stlist[i])>3){
      
      temc = chartr(old = "|", new = "g", stlist[i])
      
      temc2 = strsplit(temc, "g")
      X = temc2[[1]][1]
      TY = strsplit(temc2[[1]][2], ":")
      TY = sort(TY[[1]])
      
      TY2 = paste(TY, collapse = ":")
      
      stlist[i] = paste(X,"|", TY2, sep = "")
      
      stlist[i] <- TY %>%
        paste(., collapse=":") %>%
        paste(X, "|", ., sep = "") 
      
    }
    
  }
  
  re <- stlist %>%
    paste(., collapse="][") %>%
    paste("[", ., "]", sep = "") 
  
  return(re)
}

MB = 
  function (struc ="[6][8][5][7|6:8:5][3|7][1|7:3][4|1][2|4][0|2]", v ="1"){
    
    library(xlsx)
    library(bnlearn)
    library(dplyr)
    
    
    mbs = ""
    
    strutem <- struc %>%
      as.character(.) %>%
      substr(., 2, nchar(.)-1) 
    
    stru =  strsplit(chartr(old = "][", new = "##", strutem), "##")
    
    for(j in 1:length(stru[[1]])){
      if(grepl(v,stru[[1]][j])){
        mbs = paste(mbs, "[", stru[[1]][j], "]", sep = "")
      }
    }
    return(mbs)
  }

sumTable = 
  function (pathname = "/home/zgong001/Documents/Alarm/D1KC9v/D1KC9v BestOrders/Strus_D1KC9v.txt", 
            t = c("B-W",	"AIFM1",	"ATP6V1C2",	"CACNA1D",	"CACNB1",	"CDH15",	"CLDN6",	"DDIT3",	"EIF2AK3",	"ENDOG",	"HRK",	
                  "LMNB1"	,"MAPK12",	"NRF1",	"PARP1"	,"PCK2",	"PLA2G4C",	"PPP2R3B",	"VDAC2",	"VDAC3"), 
            exfilename = "/home/zgong001/Documents/Alarm/D1KC9v/D1KC9v BestOrders/Strus_D1KC9v.xlsx"){
    library(bnlearn)
    library(xlsx)
    library(dplyr)
    
    v = as.character(seq(0,length(t)-1),1)
    suminput = read.table(pathname, header = FALSE)
    suminput = na.omit(suminput)
    sumResu = data.frame()
    n = ncol(suminput)
    ord = vector()
    
    for(l in 1:nrow(suminput)){
      tem = suminput[l,1]
      for(m in 2:(n-4)){
        tem = paste(tem, suminput[l,m], sep = " ")
      }
      ord = append(ord, tem)
    }
    
    
    suminput = cbind(suminput, ord)
    
    
    suminput = suminput[-n]
    
    for(j in (n-4):1){
      suminput = suminput[,-j]
    }
    
    colnames(suminput)[1] <- "OrderScores"
    colnames(suminput)[2] <- "Structures"
    colnames(suminput)[3] <- "StructScores"
    
    orders= aggregate( OrderScores ~ord, data=suminput, FUN = mean)
    orders = orders[order(-orders[,2]), ]
    for(i in 1:nrow(orders)){
      orders$Opercentage[i] = exp(-log(sum(exp(orders$OrderScores-orders$OrderScores[i]))))
    }
    
    orders$Ocumper = cumsum(orders$Opercentage)
    
    for(i in 1:nrow(orders)){
      tem = suminput[which(suminput$ord== orders[i,1]), ]
      
      tem$Opercentage = orders[i,3]
      tem$Ocumper = orders[i,4]
      
      for(j in 1:nrow(tem)){
        tem$Spercentage[j] = exp(-log(sum(exp(tem$StructScores-tem$StructScores[j]))))
      }
      tem$Scumper = cumsum(tem$Spercentage)
      tem = tem[c("ord", "OrderScores", "Opercentage","Ocumper", "Structures", "StructScores","Spercentage","Scumper")]
      sumResu = rbind(sumResu, tem)
    }
    
    
    for(k in 1:nrow(sumResu)){
      sumResu$sortS[k] = sortStru(as.character(sumResu$Structures[k]))
    }
    
    rs = nrow(sumResu)
    
    rl = creatDataframe(v,rs)
    sumResu= cbind(sumResu, rl)
    
    #    sumAnce = sumResu
    
    co= combn(v,2)
    
    for(i in 1:nrow(sumResu)){
      s = as.character(sumResu [i,5])
      rel = getRelation(st = s)
      
      for(j in 1:ncol(co)){
        X = co[1,j]
        Y = co[2,j]
        c1 = paste(X, "->", Y, sep = "")
        c2 = paste(Y, "->", X, sep = "")
        c3 = paste(X, " NA ", Y, sep = "")
        
        if (c1 %in% rel){
          sumResu[i,c1] = sumResu$Opercentage[i]*sumResu$Spercentage[i]
        }
        else if(c2 %in% rel){
          sumResu[i,c2] = sumResu$Opercentage[i]*sumResu$Spercentage[i]
        }
        else{
          sumResu[i,c3] = sumResu$Opercentage[i]*sumResu$Spercentage[i]
        }
        
      }
      
    }
    
    #    print(sumResu)
    
    sumP = colSums(sumResu[, -c(1:9)])
    
    #    print (sumP)
    
    ##########################################
    
    
    
    StruWithO = aggregate( StructScores ~sortS, data=sumResu, FUN = mean)
    
    for(l in 1:nrow(StruWithO)){
      StruWithO$Spercentage[l] = exp(-log(sum(exp(StruWithO$StructScores-StruWithO$StructScores[l]))))
    }
    
    StruWithO = StruWithO[order(-StruWithO[,3]), ]
    
    rss = nrow(StruWithO)
    
    rls = creatDataframe(v,rss)
    StruWithO= cbind(StruWithO, rls)
    
    for(m in 1:nrow(StruWithO)){
      ss = as.character(StruWithO [m,1])
      rels = getRelation(st = ss)
      #      print(rels)
      
      for(n in 1:ncol(co)){
        X = co[1,n]
        Y = co[2,n]
        c1 = paste(X, "->", Y, sep = "")
        c2 = paste(Y, "->", X, sep = "")
        c3 = paste(X, " NA ", Y, sep = "")
        
        if (c1 %in% rels){
          StruWithO[m,c1] = StruWithO$Spercentage[m]
        }
        else if(c2 %in% rels){
          StruWithO[m,c2] = StruWithO$Spercentage[m]
        }
        else{
          StruWithO[m,c3] = StruWithO$Spercentage[m]
        }
        
      }
      
    }
    
    
    sumpS = colSums(StruWithO[, -c(1:3)])
    #    print(sumpS)
    
    #    totS = (sumP+sumpS)/2
    
    #    nam = names(totS)
    nam = names(sumP)
    
    #    nam2 = names(sumpS)
    
    #    print(nam2)
    
    for(i in 1:length(nam)){
      if (grepl("->", nam[i])){
        TY = strsplit(nam[i], "->")
        a = as.numeric(TY[[1]][1])
        b = as.numeric(TY[[1]][2])
        nam[i] = paste(t[a+1], "->", t[b+1], sep = "")
      }
      
      if (grepl(" NA ", nam[i])){
        TY = strsplit(nam[i], " NA ")
        a = as.numeric(TY[[1]][1])
        b = as.numeric(TY[[1]][2])
        nam[i] = paste(t[a+1], " NA ", t[b+1], sep = "")
      }
      
    }
    
    #    print(nam)
    names(sumP) = nam
    names(sumpS) = nam
    
    sumP = as.data.frame(sumP)
    sumpS = as.data.frame(sumpS)
    
    #     print(sumP)
    
    #    print(totS)
    
    write.xlsx(sumP, exfilename, sheetName = "Pairs", col.names = TRUE, row.names = TRUE, append = FALSE)
    write.xlsx(sumpS, exfilename, sheetName = "Pairs-WO", col.names = TRUE, row.names = TRUE, append = TRUE)
    
    #    return(sumpS)
    
  }



AverageNet = function(pathname = "/home/zgong001/Documents/Projects/Luminal/3/SS/Pairs-A3-Luminal A-A-B.xlsx",
                      outfile = "/home/zgong001/Documents/dot/out.gv"){
  
  library(xlsx)
  
  res <- read.xlsx(pathname, 1)  # read first sheet
  res = na.omit(res)
  
  cat("digraph G {", file=outfile, sep="\n")
  
  p = round(nrow(res)/3)
  
  for(i in 1:p){
    
    t = (i-1)*3
    v = as.vector(res[(t+1):(t+3),2])
    names(v) = res[(t+1):(t+3),1]
    m = sort(v, decreasing = TRUE)
    mn = names(m[1])
    if(grepl("->", mn)){
      if(grepl("->", names(m[2]))){
        cat("\t", mn, " [penwidth=", round(m[[1]]*2,digits = 2), ",", 'label="', round(m[[1]]*100 ,digits = 2), " (", round(m[[2]]*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
      }
      else{
        cat("\t", mn, " [penwidth=", round(m[[1]]*2,digits = 2), ",", 'label="', round(m[[1]]*100,digits = 2) , " (", round(m[[3]]*100,digits = 2),')"', "];", "\n", file=outfile, append=TRUE)
      }
      
    }
    
  }
  
  cat("}", file=outfile, append=TRUE)
  return(res)
}

############This is draw pairs percent on the graph.
AverageNet11 = function(pathname = "/home/zgong001/Documents/Projects/Luminal/3/SS/Pairs-A3-Luminal A-A-B.xlsx",
                       str = "",
                       outfile = "/home/zgong001/Documents/dot/out.gv"){
  
  library(xlsx)
  library(bnlearn)
  
  res <- read.xlsx(pathname, 1)  # read first sheet
  res = na.omit(res)
  
  strD = model2network(as.character(str))
  arcD = arcs(strD)
  nodeD = nodes(strD)
  arNo = unique(union(arcD[,1], arcD[,2]))
  nod = setdiff(nodeD, arNo)
  
  cat("digraph G {", file=outfile, sep="\n")
  
  if (length(nod) > 0){
    for(j in 1:length(nod)){
      cat("\t", nod[j], "\n", file=outfile, append=TRUE)
    }
  }
  
  p = nrow(arcD)
  
  for(i in 1:p){
    
    mn = paste(arcD[i,1],"->" , arcD[i,2], sep = "")
    mn2 = paste(arcD[i,2],"->" , arcD[i,1], sep = "")
    s1 = res[which(res[,1]==mn),2]
    s2 = res[which(res[,1]==mn2),2]
    
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
  
  cat("\t", 'label="', '      ', "\\l", "Notes:", "\\l", "    -   >99 represents percentage of 99.99 and above", "\\l", "    -   ~0  represents percentage of 0.01 and below", "\\l", "    -   Percentage in the parentheses represents the percentage of an arc that is reversed", '\\l"', ";", "\n", file=outfile, append=TRUE)
  cat("}", file=outfile, append=TRUE)
  return(res)
}

ExeAve = function(epathname = "/home/zgong001/Documents/Projects/Luminal/3/SS/Pairs-A3-Luminal A-A-B.xlsx",
                  strfile = "",
                  outfile = "/home/zgong001/Documents/dot/"){
  library(xlsx)
  strs =  read.xlsx(strfile, sheetName = "Structures")  # read first sheet
  strs = na.omit(strs)
  strs = as.data.frame(strs[1:2,1])
  
  
  for(i in 1:2){
    oun = paste(outfile, "-", i, ".gv", sep = "")
    y = AverageNet11(pathname = epathname,
                    str = as.character(strs[i,1]),
                    outfile = oun)
  }
  
  return (strs)
  
}

##################################################################################################################
# The following is for the Consensus version 2
AverageNet21 = function(pathname = "/home/zgong001/Documents/Projects/Luminal/3/SS/Pairs-A3-Luminal A-A-B.xlsx",
                         str = "",
                         outfile = "/home/zgong001/Documents/dot/out.gv"){
  
  library(xlsx)
  library(bnlearn)
  
  res <- read.xlsx(pathname, 1)  # read first sheet
  res = na.omit(res)
  
  
  
  strD = model2network(as.character(str))
  arcD = arcs(strD)
  nodeD = nodes(strD)
  arNo = unique(union(arcD[,1], arcD[,2]))
  nod = setdiff(nodeD, arNo)
  
  cat("digraph G {", file=outfile, sep="\n")
  
  if (length(nod) > 0){
    for(j in 1:length(nod)){
      cat("\t", nod[j], "\n", file=outfile, append=TRUE)
    }
  }
  
  p = nrow(arcD)
  
  for(i in 1:p){
    
    mn = paste(arcD[i,1],"->" , arcD[i,2], sep = "")
    mn2 = paste(arcD[i,2],"->" , arcD[i,1], sep = "")
    s1 = res[which(res[,1]==mn),2]
    s2 = res[which(res[,1]==mn2),2]
    
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
  
  NoNA = data.frame()
  
  arcD = as.data.frame(arcD)
  co= combn(nodeD,2)
  
  
  for(j in 1:ncol(co)){
    X = co[1,j]
    Y = co[2,j]
    
    pair_find1 = data.frame(from= X, to=Y)
    pair_find2 = data.frame(from= Y, to=X)
    
    if((nrow(merge(pair_find1,arcD )) == 0) & (nrow(merge(pair_find2,arcD )) == 0)){
      
      TX = paste(X,"->", Y, sep = "")
      TY = paste(Y,"->", X, sep = "")
      t1 = res[which(res[,1]==TX),2]
      t2 = res[which(res[,1]==TY),2]
      if(t1>t2){
        
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
      else if (t2 >t1){
        
        if((t2>=0.9999)&(t1<=0.0001)){
          
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
  
  cat("\t", 'label="', '      ', "\\l", "Notes:", "\\l", "    -   >99 represents percentage of 99.99 and above", "\\l", "    -   ~0  represents percentage of 0.01 and below", "\\l", "    -   Percentage in the parentheses represents the percentage of an arc that is reversed", '\\l"', ";", "\n", file=outfile, append=TRUE)
  cat("}", file=outfile, append=TRUE)
  return(arcD)
}