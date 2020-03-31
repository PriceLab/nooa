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

   checkTrue(nrow(tbl.kegg) > 10)
   checkEquals(colnames(tbl.kegg),
               c("KEGGID", "Pvalue", "OddsRatio", "ExpCount", "Count", "Size", "Term"))

     # this pathway should be at or near the top
   base.excision.repair <- grep("Base excision repair", tbl.kegg$Term)
   checkTrue(base.excision.repair <= 3)

} # test_keggEnrichment
#------------------------------------------------------------------------------------------------------------------------
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
if(!interactive())
    runTests()

