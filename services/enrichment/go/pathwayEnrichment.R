library(jsonlite)
library(GSEABase)
library(GOstats)
library(GO.db)
library(Category)
library(org.Hs.eg.db)
library(KEGG.db)
library(RUnit)
library(plumber)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(TxDb.Hsapiens.UCSC.hg38.knownGene)
#---------------------------------------------------------------------------------
# kegg and GO enrichment needs entrez geneIDs.  we usually start with gene sybmols
# conversion is offered in this file:
# note that the assignGeneIDs function provide here returns a list with three
# name fields:
#     mapped: successful, unique entrezIDs found for symbols
#     multiples: quasi-successful, multiple ids found for each of these symbols
#     failures: no entrezIDs found for these symbols

source("~/github/nooa/services/enrichment/go/symToGeneID.R");
# test_assignGeneIDs()

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
  suppressMessages(
     symbol.entrez.map <- assignGeneIDs(geneSymbols)
     )
  gene.universe = character(0)
  geneIDs <- unlist(symbol.entrez.map$mapped, use.names=FALSE)

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
#* Calculate a data.frame of enriched KEGG pathways
#* @post /keggEnrich
keggEnrich <- function(req, geneSymbols)
{
   debugFile <- file("~/tmp/keggEnrich.debug")
   writeLines("line 101", debugFile)
   symbol.entrez.map <- assignGeneIDs(geneSymbols)
   writeLines("line 103", debugFile)

   gene.universe = character(0)
   writeLines("line 106", debugFile)
   geneIDs <- unlist(symbol.entrez.map$mapped, use.names=FALSE)
   writeLines("line 108", debugFile)

   kegg.params <- new("KEGGHyperGParams", geneIds = unique(geneIDs),
                      universeGeneIds = character(0), annotation = "org.Hs.eg.db",
                      pvalueCutoff = 0.1, testDirection = "over")

   writeLines("line 114", debugFile)
   kegg.hgr  <- hyperGTest(kegg.params)
   #print(kegg.hgr)
   writeLines("line 116", debugFile)

   tbl.kegg <- summary(kegg.hgr)
   #print(tbl.kegg)
   writeLines("line 119", debugFile)
   close(debugFile)

   result <- toJSON(tbl.kegg)

   return(result)

} # keggEnrich
#--------------------------------------------------------------------------------
test_goEnrich <- function()
{
   igap.ad.genes <- c("CR1", "BIN1", "CD2AP", "EPHA1", "CLU", "MS4A6A", "PICALM",
                      "ABCA7", "CD33", "HLA-DRB5", "HLA-DRB1", "PTK2B", "SORL1",
                      "SLC24A4", "RIN3", "DSG2", "INPP5D", "MEF2C", "NME8", "ZCWPW1",
                      "CELF1", "FERMT2", "CASS4", "APOE", "TOMM40")

   tbl.go.json <- goEnrich(req=NA, head(igap.ad.genes))
   tbl.go <- fromJSON(tbl.go.json)
   dim(tbl.go)
   checkEquals(ncol(tbl.go), 8)
   checkTrue(nrow(tbl.go) > 100)
   checkEquals(tbl.go$Term[1], "negative regulation of amyloid-beta formation")
   checkEquals(tbl.go$genes[1], "BIN1;CLU")


} # test_goEnrich
#--------------------------------------------------------------------------------
test_keggEnrich <- function()
{
   igap.ad.genes <- c("CR1", "BIN1", "CD2AP", "EPHA1", "CLU", "MS4A6A", "PICALM",
                      "ABCA7", "CD33", "HLA-DRB5", "HLA-DRB1", "PTK2B", "SORL1",
                      "SLC24A4", "RIN3", "DSG2", "INPP5D", "MEF2C", "NME8", "ZCWPW1",
                      "CELF1", "FERMT2", "CASS4", "APOE", "TOMM40")

   tbl.kegg.json <- keggEnrich(req=NA, head(igap.ad.genes, n=-1))
   tbl.kegg <- fromJSON(tbl.kegg.json)
   checkEquals(ncol(tbl.kegg), 7)
   checkTrue(nrow(tbl.kegg) > 10)
   checkEquals(tbl.kegg$Term[1], "Hematopoietic cell lineage")
   checkEquals(tbl.kegg$Count[1], 4)

      #--------------------------------------
      #--------------------------------------

   cpt1a.tfs <- c("GRHL2", "POU2F2", "TP53", "KLF5", "TFAP2C", "BHLHE40","MEIS1", "SMAD1")
   tbl.kegg.json <- keggEnrich(req=NA, cpt1a.tfs)
   tbl.kegg <- fromJSON(tbl.kegg.json)
   dim(tbl.kegg)


} # test_keggEnrich
#--------------------------------------------------------------------------------
#* Get the chrom, start and end of gene symbol or rsid
#* @post /geneLoc
geneLoc <- function(req, gene, genome, shoulder)
{
   if(!genome %in% c("hg19", "hg38"))
       return(toJSON(list(gene=gene, genome=genome, chrom=NA_character_, start=NA_integer_, end=NA_integer_)))

   suppressMessages(
     map <- assignGeneIDs(gene)
     )

   if(all(is.null(map$mapped)))
      return(toJSON(list(gene=gene, genome=genome, chrom=NA_character_, start=NA_integer_, end=NA_integer_)))

   geneID <- map$mapped[[gene]]

   if(genome == "hg19"){
      genes <- genes(TxDb.Hsapiens.UCSC.hg19.knownGene)
      tbl.loc <- as.data.frame(genes[geneID])
      }

   if(genome == "hg38"){
      genes <- genes(TxDb.Hsapiens.UCSC.hg38.knownGene)
      tbl.loc <- as.data.frame(genes[geneID])
      }

   chrom <- as.character(tbl.loc$seqnames[1])
   start <- tbl.loc$start[1] - shoulder
   end   <- tbl.loc$end[1] + shoulder

   toJSON(list(gene=gene, genome=genome, chrom=chrom, start=start, end=end))

} # geneLoc
#---------------------------------------------------------------------------------
test_geneLoc <- function()
{
   uri <- sprintf("http://localhost:8000/geneLoc")
   body.jsonString <- sprintf('%s', toJSON(list(gene="APOE", genome="hg38", shoulder=100)))
   r <- POST(uri, body=body.jsonString)
   fromJSON(content(r)[[1]])

} # test_geneLoc
#--------------------------------------------------------------------------------
