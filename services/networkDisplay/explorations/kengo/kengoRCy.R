# kengoRCyj.R: before shiny app, before services, explore kengo's data with RCyjs
#----------------------------------------------------------------------------------------------------
library(RCyjs)
#----------------------------------------------------------------------------------------------------
if(!exists("tbl.nodes")){
   load("tbls-curated-v2.RData")
   tbl.nodes$assay <- 0.0
   dim(tbl.nodes)
   dim(tbl.edges)
   score <- -log10(tbl.edges$pval)
   tbl.edges$score <- score
   stopifnot(all(c(tbl.edges$source, tbl.edges$target) %in% tbl.nodes$id))
   g.json.string <- RCyjs::dataFramesToJSON(tbl.edges, tbl.nodes)
   }
#----------------------------------------------------------------------------------------------------
if(!exists("rcy"))
    rcy <- RCyjs(title="kengo")
#----------------------------------------------------------------------------------------------------
showCurrentNetwork <- function()
{
   setGraph(rcy, g.json.string)
   layout(rcy, "cose")
   getSupportedNodeShapes(rcy)
   loadStyleFile(rcy, "kengo-style.js")

} # showCurrentNetwork
#----------------------------------------------------------------------------------------------------
iterateAcrossSubjectCategories <- function()
{
   cytidine.assay <- as.numeric(subset(tbl.nodes, name=="cytidine")[, -c(1:4)])
   loadStyleFile(rcy, "kengo-style.js")

   noi <- c("100000963", "100002405", "100008976", "100010918", "100010919", "100015851", "1537", "827")
   cohorts <- colnames(tbl.nodes)[5:37]

   for(cohort in cohorts){
     values <- tbl.nodes[, cohort]
     setNodeAttributes(rcy, "assay", tbl.nodes$id, values)
     Sys.sleep(1)
     } # for cohort

   fivenum(cytidine.assay) # -0.3396491 -0.2407187 -0.1993833 -0.1446404  0.1161721

   indices <- match(noi, tbl.nodes$id)
   noa.youngF <- tbl.nodes[indices, young.female.cohort]
   noa.oldMF  <- tbl.nodes[indices, old.cohort]

   setNodeAttributes(rcy, "assay", noi, noa.youngF)
   setNodeAttributes(rcy, "assay", noi, noa.oldMF)


   selectNodes(rcy, "827")
   fitSelection(rcy, 100)

} # iterateAcrossSubjectCategories
#----------------------------------------------------------------------------------------------------


