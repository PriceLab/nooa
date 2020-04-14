# kengo says (emai, 13 apr 2020):
#   The basic interaction is b/w “genes" and “analyte_id” in
#  "Orwoll-longevity-genes_SKATO_Cytoscape_association-table.tsv”, which also includes how the
#   interaction is confident (p-value). The label and attributes of “analyte_id" are in
#  "Orwoll-longevity-genes_SKATO_Cytoscape_analyte-table.tsv”, and those of “genes” are in
#  "Orwoll-longevity-genes_SKATO_Cytoscape_input-table.tsv”.

files <- list.files(pattern="*.tsv")
files
tbl.analytes <- read.table(files[1], sep="\t", as.is=TRUE, nrow=-1, header=TRUE)
dim(tbl.analytes) # 170 36
head(tbl.analytes)
tbl.assoc <- read.table(files[2], sep="\t", as.is=TRUE, nrow=-1, header=TRUE)
dim(tbl.assoc)    # 218 14
colnames(tbl.assoc)
head(tbl.assoc)
tbl.input <- read.table(files[3], sep="\t", as.is=TRUE, nrow=-1, header=TRUE)
dim(tbl.input)    # 25 4
colnames(tbl.input)

table(tbl.assoc$genes)
table(tbl.assoc$AnalyteRename)
