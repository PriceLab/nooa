library(RCyjs)
#----------------------------------------------------------------------------------------------------
# f <- "Orwoll-longevity-genes_SKATO_MergeFilterResults_BHpval-05.csv"
# tbl.assoc <- read.table(f, sep=",", as.is=TRUE, nrow=-1, header=TRUE, stringsAsFactors=FALSE)
# dim(tbl.assoc)
# head(tbl.assoc)
#
# tbl.edges <- tbl.assoc[, c("genes", "analyte_id")]
# tbl.edges$interaction <- "assoc"
# colnames(tbl.edges) <- c("source", "target", "interaction")
#
#
# g.json <- toJSON(dataFramesToJSON(tbl.edges))
# rcy <- RCyjs(title="kengo", quiet=TRUE)
# setGraph(rcy, g.json)
# layout(rcy, "cose")
#
tbl.e2 <- read.table("Orwoll-longevity-genes_SKATO_MergeFilterResults_BHpval-05.csv",
                     sep=",", header=TRUE, as.is=TRUE)
table(tbl.e2$type)
#   clinical metabolite    protein
#         2         80        126

genes <- sort(unique(tbl.e2$genes))
clinical.assays <- subset(tbl.e2, type == "clinical")$analyte_id
proteins <- unique(subset(tbl.e2, type == "protein")$analyte)
metabolites <- unique(subset(tbl.e2, type == "metabolite")$analyte)
all.nodes <- sort(unique(c(genes, clinical.assays, proteins, metabolites)))
length(all.nodes)

nodes.in.edges <- sort(unique(c(tbl.e2$genes, tbl.e2$analyte_id)))
length(nodes.in.edges)
all(nodes.in.edges %in% all.nodes)
all(all.nodes %in% nodes.in.edges)


colnames(tbl.e2) <- c("source", "target", "ignore", "pval")
tbl.e2$interaction <- "associated"
tbl.edges <- tbl.e2[, c("source", "target", "interaction", "pval")]

tbl.nodes <- read.table("Orwoll-longevity-genes_SKATO_Cytoscape_analyte-table.tsv",
                        sep="\t", header=TRUE, as.is=TRUE, nrow=-1)


colnames(tbl.nodes) <- c("id", "name", "alt.name",
           "F-18-25", "M-18-25", "F-25-30", "M-25-30", "F-30-35", "M-30-35", "F-35-40",
           "M-35-40", "F-40-45", "M-40-45", "F-45-50", "M-45-50", "F-50-55", "M-50-55",
           "F-55-60", "M-55-60", "F-60-65", "M-60-65", "F-65-70", "M-65-70", "F-70-89",
           "M-70-89", "MF-18-25", "MF-25-30", "MF-30-35", "MF-35-40", "MF-40-45",
           "MF-45-50", "MF-50-55", "MF-55-60", "MF-60-65", "MF-65-70", "MF-70-89")


assayed.nodes <- sort(unique(tbl.nodes$id))
   # create a data.frame for genes which will rbind to tbl.nodes:

tbl.genes <- data.frame(id=genes, name=genes, alt.name=genes, stringsAsFactors=FALSE)
for(col.name in colnames(tbl.nodes)[4:36]) tbl.genes[, col.name] <-1

tbl.nodes <- rbind(tbl.nodes, tbl.genes)
  # eliminate all edges for which we do not have node data

tbl.edges <- subset(tbl.e2, source %in% tbl.nodes$id & target %in% tbl.nodes$id)  # 152
tbl.edges <- tbl.edges[, c("source", "target", "interaction", "pval")]
dim(tbl.edges)   # 152 x 4
dim(tbl.nodes)   # 190 x 36

node.types <- rep("", length(tbl.nodes$id))
node.types[grep("_", tbl.nodes$id)] <- "protein"
tbl.nodes$type <- node.types
tbl.nodes <- tbl.nodes[, c("id", "type", "name", "alt.name", colnames(tbl.nodes)[4:36])]
write.table(tbl.nodes, sep="\t", file="tbl.nodes.forEditing.tsv", quote=FALSE)
  #-------------------------------------------
  # hand-edit to get the node types right
  #-------------------------------------------

tbl.nodes <- read.table("tbl.nodes.forEditing.tsv", header=TRUE, as.is=TRUE, sep="\t")
dim(tbl.nodes)  # [1] 190  37
save(tbl.edges, tbl.nodes, file="tbls-curated.RData")
