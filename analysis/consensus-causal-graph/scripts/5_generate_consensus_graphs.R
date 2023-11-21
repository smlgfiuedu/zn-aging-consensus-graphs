source("./scripts/AverageNetwork3.R")

y = sumTable(pathname = "./analysis/structures/d50c9v_structures_as_zgs.txt",
             t = c("Anaphylaxis",	"ArtCO2",	"Catechol",	"ExpCO2",	"HR",	"InsuffAnesth",	"SaO2",	"TPR",	"VentLung"),
             exfilename = "./analysis/consensus-graph/d50c9v_zgs_consensus.xlsx")

z = AverageNet21 (pathname = "./analysis/consensus-graph/d50c9v_zgs_consensus.xlsx",
                  str = "[8][0][7|0][5][6|8][1|0:6][3|8:0:1][2|0:5:6][4|2]",
                  outfile = "./analysis/consensus-graph/structure.out.gv")
