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

    goi <-   c("COLEC11", "MYCN", "ESM1", "IGF1R", "TNNC1", "LGSN", "LOC100134423", "DLK1", "CRYGC",
               "A_33_P3286709", "PLEKHG4B", "TSPAN8", "ENPP6", "FLJ30901", "PROM1", "COL22A1",
               "KIAA1324", "PTCH2", "SLC35F3", "GABRG3", "LOC613266", "LCE1E", "B4GALNT4", "STC2",
               "FOXL2NB", "AK124496", "CU677518", "NM_130777", "NR_102701", "NSG1", "STAR",
               "MCTS2P", "TRABD2A", "DEPTOR", "CLEC4GP1", "LINC01405", "COL2A1", "HS6ST2", "PRAME",
               "LINC02398", "GJB7", "PLCXD3", "ERVI-1", "ZNF556", "NTS", "BU963192", "GMNC",
               "A_33_P3274001", "CRYGD", "GREB1")

   goi.string <- toJSON(goi)
   uri <- sprintf("http://localhost:8000/goEnrich")
   body.jsonString <- sprintf('%s', toJSON(list(geneSymbols=goi)))

   r <- POST(uri, body=body.jsonString)

      #sprintf('{"geneSymbols": "%s"}', goi.string))
   tbl <- fromJSON(content(r)[[1]])


} # test_platinumResitanceGenes
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
    runTests()

