library(jsonlite)
library(GSEABase)
library(GOstats)
library(GO.db)
library(Category)
library(org.Hs.eg.db)
library(KEGG.db)
library(RUnit)
library(plumber)
#---------------------------------------------------------------------------------
# kegg and GO enrichment needs entrez geneIDs.  we usually start with gene sybmols
# conversion is offered in this file:
# note that the assignGeneIDs function provide here returns a list with three
# name fields:
#     mapped: successful, unique entrezIDs found for symbols
#     multiples: quasi-successful, multiple ids found for each of these symbols
#     failures: no entrezIDs found for these symbols
source("symToGeneID.R");
test_assignGeneIDs()
#---------------------------------------------------------------------------------
# requires a list of genes
#---------------------------------------------------------------------------------
runTests <- function()
{
   test_goEnrich()

} # runTests
#---------------------------------------------------------------------------------
#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="")
{
  list(msg = paste0("The message is: '", msg, "'"))
}
#---------------------------------------------------------------------------------
#* return the input, add calculate length of the name
#* @post /postdemo
postdemo <- function(req, id, name)
{
  list(
    id = id,
    name = name,
    nameLength = nchar(name)
    #   raw = req$postBody
    )

}  # postdemo
#---------------------------------------------------------------------------------
#* return the input, add calculate length of the name
#* @post /head_mtcars
smalltable <- function(req, rows)
{
   tbl <- head(mtcars, n=rows)

   #write.table(tbl, file="testing_v3_xyz.csv", sep=",", row.names=FALSE, col.names=TRUE, append = T)
   return(toJSON(tbl))

}  # smalltable
#---------------------------------------------------------------------------------
#* Calculate a data.frame of enriched GO categories
#* @post /goEnrich
goEnrich <- function(req, geneSymbols)
{
  #print(geneSymbols)
  #print(length(geneSymbols))

  #geneSymbols <- fromJSON(geneSymbols)
  #print(geneSymbols)
  symbol.entrez.map <- assignGeneIDs(geneSymbols)
  gene.universe = character(0)
  geneIDs <- unlist(symbol.entrez.map$mapped, use.names=FALSE)
  #print(geneIDs)

  go.params <- new("GOHyperGParams", geneIds=unique(geneIDs),
                   universeGeneIds = gene.universe, annotation = "org.Hs.eg.db",
                   ontology = 'BP', pvalueCutoff = 0.05, conditional = FALSE,
                   testDirection = "over")

  go.bp.hgr <- hyperGTest(go.params)
  tbl.go <- summary(go.bp.hgr)
  #print(tbl.go)
  geneSymbols <- lapply(tbl.go$GOBPID,
                        function(goTerm){
                           all.geneIDs.this.term <- unique(unlist(get(goTerm, org.Hs.egGO2ALLEGS)))
                           keepers <- intersect(geneIDs, all.geneIDs.this.term)
                           keeper.geneSymbols <- mget(keepers, org.Hs.egSYMBOL)
                           keeper.geneSymbols <- unlist(keeper.geneSymbols, use.names=FALSE)
                           paste(keeper.geneSymbols, collapse=";")
                           })
  tbl.go$genes <- unlist(geneSymbols, use.names=FALSE)

  return(toJSON(tbl.go))

} # goEnrich
#---------------------------------------------------------------------------------
keggEnrich <- function(geneIDs)
{
   symbol.entrez.map <- assignGeneIDs(geneIDs)

   gene.universe = character(0)
   geneIDs <- unlist(symbol.entrez.map$mapped, use.names=FALSE)

   kegg.params <- new("KEGGHyperGParams", geneIds = unique(geneIDs),
                      universeGeneIds = character(0), annotation = "org.Hs.eg.db",
                      pvalueCutoff = 0.1, testDirection = "over")

   kegg.hgr  <- hyperGTest(kegg.params)

   return(kegg.hgr)

} # keggEnrich
#--------------------------------------------------------------------------------
test_goEnrich <- function()
{
   igap.ad.genes <- c("CR1", "BIN1", "CD2AP", "EPHA1", "CLU", "MS4A6A", "PICALM",
                      "ABCA7", "CD33", "HLA-DRB5", "HLA-DRB1", "PTK2B", "SORL1",
                      "SLC24A4", "RIN3", "DSG2", "INPP5D", "MEF2C", "NME8", "ZCWPW1",
                      "CELF1", "FERMT2", "CASS4", "APOE", "TOMM40")

   #x <- assignGeneIDs(igap.ad.genes)
   #lapply(x, length)   # mapped: 25   failures: 0   muplitples: 0
   #geneIDs <- unlist(x$mapped, use.names=FALSE)

   tbl.go <- goEnrich(head(igap.ad.genes))
   checkEquals(ncol(tbl.go), 8)
   checkTrue(nrow(tbl.go) > 100)
   checkEquals(tbl.go$Term[1], "negative regulation of amyloid-beta formation")
   checkEquals(tbl.go$genes[1], "BIN1;CLU")


} # test_goEnrich
#--------------------------------------------------------------------------------

