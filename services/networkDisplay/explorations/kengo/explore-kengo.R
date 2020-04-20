# kengo says (email, 13 apr 2020):
#   The basic interaction is between “genes" and “analyte_id” in
#  "Orwoll-longevity-genes_SKATO_Cytoscape_association-table.tsv”, which also includes how the
#   interaction is confident (p-value). The label and attributes of “analyte_id" are in
#  "Orwoll-longevity-genes_SKATO_Cytoscape_analyte-table.tsv”, and those of “genes” are in
#  "Orwoll-longevity-genes_SKATO_Cytoscape_input-table.tsv”.

# kengo updates (email 16 apr 2020): I have just sent the link for the updated files. In the files,
# I also simplified the columns. Please use
# "Orwoll-longevity-genes_SKATO_MergeFilterResults_BHpval-05.csv” as the basic interaction. This
# time, the p-value to use is easier to see. As to “genes” nodes, the label and attributes include
# in "Orwoll-longevity-genes_SKATO_Cytoscape-input-node.tsv”. As to “analyte_id” nodes, in
# "Orwoll-longevity-genes_SKATO_Cytoscape-analyte-node.tsv”.


# files <- list.files(pattern="*.tsv")
# files
# tbl.analytes <- read.table(files[1], sep="\t", as.is=TRUE, nrow=-1, header=TRUE)
# dim(tbl.analytes) # 170 36
# head(tbl.analytes)
# tbl.assoc <- read.table(files[2], sep="\t", as.is=TRUE, nrow=-1, header=TRUE)
# dim(tbl.assoc)    # 218 14
# colnames(tbl.assoc)
# head(tbl.assoc)
# tbl.input <- read.table(files[3], sep="\t", as.is=TRUE, nrow=-1, header=TRUE)
# dim(tbl.input)    # 25 4
# colnames(tbl.input)
#
# table(tbl.assoc$genes)
# table(tbl.assoc$AnalyteRename)
#
# tbl.edges <- tbl.assoc[, c("genes", "analyte_id", "mlog10_adj_pval_skato"), stringsAsFactors=FALSE]
# tbl.edges$interaction <- "associatesWith"
#
# colnames(tbl.edges) <- c("source", "target", "confidence", "interaction")
# tbl.edges <- tbl.edges[, c("source", "target", "interaction", "confidence")]
# dim(tbl.edges)
# save(tbl.edges, file="tbl.edges.RData")


f <- "Orwoll-longevity-genes_SKATO_MergeFilterResults_BHpval-05.csv"
tbl.assoc <- read.table(f, sep=",", as.is=TRUE, nrow=-1, header=TRUE)
dim(tbl.assoc)
head(tbl.assoc)


tbl.edges <-  as the basic interaction. This
# time, the p-value to use is easier to see. As to “genes” nodes, the label and attributes include
# in "Orwoll-longevity-genes_SKATO_Cytoscape-input-node.tsv”. As to “analyte_id” nodes, in
# "Orwoll-longevity-genes_SKATO_Cytoscape-analyte-node.tsv”.
