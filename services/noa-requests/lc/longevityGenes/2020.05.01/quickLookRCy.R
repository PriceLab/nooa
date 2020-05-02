# kengoRCyj.R: before shiny app, before services, explore kengo's data with RCyjs
#----------------------------------------------------------------------------------------------------
library(RCyjs)
#----------------------------------------------------------------------------------------------------
if(!exists("tbl.nodes")){
   load("fromKengo-01may2020.RData")
   tbl.nodes$assay <- 0.0
   dim(tbl.nodes)
   dim(tbl.edges)
   colnames(tbl.edges) <- c("source", "target", "score", "interaction")
   colnames(tbl.nodes) <- c("label", "id", "pubmedCount", "direction", "nodeType", "assay")
   lc.genes <- sort(unique(subset(tbl.nodes, nodeType=="LongevityGene")$id))
   length(lc.genes)  # 988
   tbl.edges <- subset(tbl.edges, score >= 0.95 & interaction=="InnerInteraction"  &
                                  source %in% lc.genes & target %in% lc.genes)

   dim(tbl.edges)  # 10140 4
   dim(tbl.nodes)
   tbl.nodes <- subset(tbl.nodes, id %in% lc.genes)
   dim(tbl.nodes)

   stopifnot(all(c(tbl.edges$source, tbl.edges$target) %in% tbl.nodes$id))
   save(tbl.edges, tbl.nodes, file="01may2020-curated.01-1k.nodes-1k.edges.RData")
   g.json.string <- RCyjs::dataFramesToJSON(tbl.edges, tbl.nodes)
   }
#----------------------------------------------------------------------------------------------------
if(!exists("rcy"))
    rcy <- RCyjs(title="kengo")
#----------------------------------------------------------------------------------------------------
showCurrentNetwork <- function()
{
   setGraph(rcy, g.json.string)
   layout(rcy, "grid")
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


