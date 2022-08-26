library(RUnit)
library(org.Hs.eg.db)
library(httr)
library(jsonlite)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
  suppressMessages({
    test_simpleEcho()
    test_postDemo()
    test_smallTable()
    test_geneLoc()
    test_goEnrichment()
    test_keggEnrichment()
    })

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_simpleEcho <- function()
{
    printf("--- simpleEcho")

    msg <- "helloGo"
    uri <- sprintf("http://localhost:8000/echo?msg=%s", msg)
    r <- GET(uri)
    msg.response <- content(r)$msg[[1]]
    checkEquals(msg.response, sprintf("The message is: '%s'", msg))
    #printf("back from server: %s", msg.response)

} # test_simpleEcho
#------------------------------------------------------------------------------------------------------------------------
test_postDemo <- function()
{
    printf("--- test_postDemo")
      # from bash: curl --data '{"id":123, "name": "Jennifer"}' "http://localhost:8000/postdemo"
    uri <- sprintf("http://localhost:8000/postdemo")
    r <- POST(uri, body='{"id":123, "name": "Jennifer"}')
    contents <- content(r)
    checkEquals(contents$nameLength[[1]], nchar("Jennifer"))

} # test_postDemo
#------------------------------------------------------------------------------------------------------------------------
# possible help here: https://github.com/rstudio/plumber/issues/512
test_smallTable <- function()
{
    printf("--- test_smallTable")
      # from bash: curl --data '{"id":123, "name": "Jennifer"}' "http://localhost:8000/postdemo"

    uri <- sprintf("http://localhost:8000/head_mtcars")
    r <- POST(uri, body='{"rows": 5}')
    contents <- content(r)
    tbl <- fromJSON(contents[[1]])
    checkEquals(dim(tbl), c(5, 11))
    checkEquals(rownames(tbl),
                c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive", "Hornet Sportabout"))

} # test_smallTable
#------------------------------------------------------------------------------------------------------------------------
test_goEnrichment <- function()
{
   printf("--- test_goEnrichment")

   dna.repair <- "GO:0006281"
   x <- select(org.Hs.eg.db, keys=dna.repair, keytype="GOALL", columns="SYMBOL")
   length(x$SYMBOL)
   goi <- sort(sample(x$SYMBOL, 10))
   goi.string <- toJSON(goi)
   uri <- sprintf("http://localhost:8000/goEnrich")
   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=goi)))

   r <- POST(uri, body=body.jsonString)

      #sprintf('{"geneSymbols": "%s"}', goi.string))
   tbl <- fromJSON(content(r)[[1]])
   checkTrue(nrow(tbl) > 50)
   checkEquals(colnames(tbl), c("GOBPID", "Pvalue", "ExpCount", "Count", "Size", "Term", "genes", "OddsRatio"))
   checkTrue(grep("^DNA repair$", tbl$Term) <= 3)  # probably always first, but allow for slop

} # test_goEnrichment
#------------------------------------------------------------------------------------------------------------------------
test_goEnrichment.cellularComponent <- function()
{
   printf("--- test_goEnrichment.cellularComponent")

      # genes symbols from clustered gautier erythropoiesis ribosomal proteins, raw and normalized
      # here combined
   goi <- c("RPL12", "RPL21", "RPL22", "RPL23", "RPL23A", "RPL24", "RPL37A", "RPS14", "RPS18",
            "RPS19", "RPS20", "RPS21", "RPS28", "RPS3", "RPS8")

   goi.string <- toJSON(goi)
   uri <- sprintf("http://localhost:8000/goEnrich")
   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=goi, ontology="CC")))

   r <- POST(uri, body=body.jsonString)

      #sprintf('{"geneSymbols": "%s"}', goi.string))
   tbl <- fromJSON(content(r)[[1]])
   dim(tbl)
   checkTrue(nrow(tbl) > 50)
   checkEquals(colnames(tbl), c("GOCCID", "Pvalue", "ExpCount", "Count", "Size", "Term", "genes", "OddsRatio"))
   checkTrue(grep("^cytosolic ribosome$", tbl$Term) <= 3)  # probably always first, but allow for slop

} # test_goEnrichment
#------------------------------------------------------------------------------------------------------------------------
test_keggEnrichment <- function()
{
   printf("--- test_keggEnrichment")

   dna.repair <- "GO:0006281"
   x <- select(org.Hs.eg.db, keys=dna.repair, keytype="GOALL", columns="SYMBOL")
   length(x$SYMBOL)
   set.seed(17)
   goi <- sort(sample(x$SYMBOL, 100))
   goi.string <- toJSON(goi)
   uri <- sprintf("http://localhost:8000/keggEnrich")
   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=goi)))

   r <- POST(uri, body=body.jsonString)

      #sprintf('{"geneSymbols": "%s"}', goi.string))
   tbl.kegg <- fromJSON(content(r)[[1]])

   checkTrue(nrow(tbl.kegg) > 8)
   checkEquals(colnames(tbl.kegg),
               c("KEGGID", "Pvalue", "OddsRatio", "ExpCount", "Count", "Size", "Term"))

     # this pathway should be at or near the top
   base.excision.repair <- grep("Base excision repair", tbl.kegg$Term)
   checkTrue(base.excision.repair <= 3)

     #----------------------------------------

   goi <- c("GRHL2", "POU2F2", "TP53", "KLF5", "TFAP2C", "BHLHE40","MEIS1", "SMAD1")
   uri <- sprintf("http://localhost:8000/keggEnrich")
   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=goi)))
   r <- POST(uri, body=body.jsonString)
   tbl.kegg <- fromJSON(content(r)[[1]])
   checkTrue("p53 signaling pathway" %in% tbl.kegg$Term)

} # test_keggEnrichment
#------------------------------------------------------------------------------------------------------------------------

# Expression Analysis of Platinum Sensitive and Resistant Epithelial Ovarian Cancer Patient Samples
# Reveals New Candidates for Targeted Therapies.
# https://www.ncbi.nlm.nih.gov/pubmed/30056367
#
# Ovarian cancer has the highest mortality rate of all gynecologic malignancies. Identification of
# new biomarkers is highly needed due to its late diagnosis and high recurrence rate. The objective
# of this study was to identify mechanisms of therapy resistance and potential biomarkers by
# analyzing mRNA and protein expression from samples derived from patients with platinum-sensitive
# and -resistant ovarian cancer (total cohort n = 53). The data revealed new candidates for targeted
# therapies, such as GREB1 and ROR2. We showed that the development of platinum resistance
# correlated with upregulation of ROR2, whereas GREB1 was downregulated. Moreover, we demonstrated
# that high levels of ROR2 in platinum-resistant samples were associated with upregulation of Wnt5a,
# STAT3 and NF-kB levels, suggesting that a crosstalk between the non-canonical Wnt5a-ROR2 and
# STAT3/NF-kB signaling pathways. Upregulation of ROR2, Wnt5a, STAT3 and NF-kB was further detected
# in a platinum-resistant cell-line model. The results of the present study provided insight into
# molecular mechanisms associated with platinum resistance that could be further investigated to
# improve treatment strategies in this clinically challenging gynecological cancer.
#
test_platinumResistanceGenes <- function()
{
   printf("--- test_platinumResistanceGenes")

   f <- "~/github/nooa/explorations/pathway-enrichment/staff-downRegulated.tsv"
   tbl <- read.table(f, sep="\t", as.is=TRUE, header=TRUE)
   goi.down <- tbl$Gene.Name
   length(goi.down)

   f <- "~/github/nooa/explorations/pathway-enrichment/staff-upRegulated.tsv"
   tbl <- read.table(f, sep="\t", as.is=TRUE, header=TRUE)
   goi.up <- tbl$Gene.Name
   length(goi.up)

    #------------------------
    #  up-regulated genes
    #------------------------
   uri <- sprintf("http://localhost:8000/goEnrich")
   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=c(goi.up))))

   r <- POST(uri, body=body.jsonString)

   tbl.up <- fromJSON(content(r)[[1]])
   subset(tbl.up, Count > 3 & Pvalue < 0.05)

    #------------------------
    #  down-regulated genes
    #------------------------

   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=goi.down)))
   r <- POST(uri, body=body.jsonString)
   tbl.down <- fromJSON(content(r)[[1]])
   subset(tbl.down, Count > 3 & Pvalue < 0.05)


    #------------------------
    #  down-regulated genes
    #------------------------

   uri <- sprintf("http://localhost:8000/keggEnrich")
   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=c(goi.down, goi.up))))

   r <- POST(uri, body=body.jsonString)
   tbl.kegg <- fromJSON(content(r)[[1]])
   dim(tbl.kegg)

} # test_platinumResitanceGenes
#------------------------------------------------------------------------------------------------------------------------
test_geneLoc <- function()
{
   printf("--- test_geneLoc")

   uri <- sprintf("http://localhost:8000/geneLoc")

   body.jsonString <- sprintf('%s', toJSON(list(gene="APOE", genome="hg38", shoulder=0)))
   r <- POST(uri, body=body.jsonString)
   x.hg38 <- fromJSON(content(r)[[1]])
   checkEquals(with(x.hg38, sprintf("%s:%d-%d", chrom, start, end)), "chr19:44905791-44909393")
   checkEquals(x.hg38$strand, "+")

   body.jsonString <- sprintf('%s', toJSON(list(gene="APOE", genome="hg38", shoulder=100)))
   r <- POST(uri, body=body.jsonString)
   x.hg38 <- fromJSON(content(r)[[1]])
   checkEquals(with(x.hg38, sprintf("%s:%d-%d", chrom, start, end)), "chr19:44905691-44909493")
   checkEquals(x.hg38$strand, "+")

   uri <- sprintf("http://localhost:8000/geneLoc")
   body.jsonString <- sprintf('%s', toJSON(list(gene="APOE", genome="hg19", shoulder=0)))
   r <- POST(uri, body=body.jsonString)
   x.hg19 <- fromJSON(content(r)[[1]])
   checkEquals(with(x.hg19, sprintf("%s:%d-%d", chrom, start, end)), "chr19:45409039-45412650")
   checkEquals(x.hg19$strand, "+")

   body.jsonString <- sprintf('%s', toJSON(list(gene="APOE", genome="hg19", shoulder=100)))
   r <- POST(uri, body=body.jsonString)
   x.hg19 <- fromJSON(content(r)[[1]])
   checkEquals(with(x.hg19, sprintf("%s:%d-%d", chrom, start, end)), "chr19:45408939-45412750")

   body.jsonString <- sprintf('%s', toJSON(list(gene="bogus", genome="hg19", shoulder=100)))
   r <- POST(uri, body=body.jsonString)
   x.bogus <- fromJSON(content(r)[[1]])
   with(x.bogus, checkTrue(all(is.na(chrom), is.na(start), is.na(end))))

   body.jsonString <- sprintf('%s', toJSON(list(gene="bogus", genome="hg38", shoulder=100)))
   r <- POST(uri, body=body.jsonString)
   x.bogus <- fromJSON(content(r)[[1]])
   with(x.bogus, checkTrue(all(is.na(chrom), is.na(start), is.na(end))))

      # make sure upper casing works
   body.jsonString <- sprintf('%s', toJSON(list(gene="Myc", genome="hg38", shoulder=100)))
   r <- POST(uri, body=body.jsonString)
   x.myc <- fromJSON(content(r)[[1]])
   checkTrue(with(x.myc, checkTrue(all(checkEquals(gene, "Myc"),
                                       checkEquals(chrom, "chr8"),
                                       checkEquals(start, 127735334),
                                       checkEquals(end,127743051)))))

      # check at least one "-" strand gene
   body.jsonString <- sprintf('%s', toJSON(list(gene="ZNF560", genome="hg38", shoulder=100)))
   r <- POST(uri, body=body.jsonString)
   x.hg38 <- fromJSON(content(r)[[1]])
   checkEquals(x.hg38$strand, "-")

} # test_geneLoc
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
    runTests()

