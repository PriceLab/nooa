f.edges <- "200430_curated-longevity-proteins_STRING_Cytoscape-edges.tsv"
f.nodes <- "200430_curated-longevity-proteins_STRING_Cytoscape-nodes.tsv"

tbl.nodes <- read.table(f.nodes, sep="\t", header=TRUE, as.is=TRUE, nrow=-1)
tbl.edges <- read.table(f.edges, sep="\t", header=TRUE, as.is=TRUE, nrow=-1)

length(tbl.nodes$stringId) # 5708
dim(tbl.edges)             # 18291 4
with(tbl.edges, length(unique(c(stringId_A, stringId_B))))   # 5686

as.data.frame(sort(table(tbl.edges$edgeType)))
#               Var1  Freq
# 1 OuterInteraction  8151
# 2 InnerInteraction 10140

as.data.frame(sort(table(tbl.nodes$nodeType)))
#            Var1 Freq
# 1 LongevityGene 1010
# 2  NeighborGene 4698

save(tbl.nodes, tbl.edges, file="fromKengo-01may2020.RData")



