library(shiny)
library(jsonlite)
library(Chinook)
library(cyjShiny)
library(htmlwidgets)
#------------------------------------------------------------------------------------------------------------------------
load("01may2020-curated.01-1k.nodes-1k.edges.RData")
tbl.nodes <- tbl.nodes[, c("id", "label", "pubmedCount", "direction", "nodeType", "assay")]
tbl.edges <- head(tbl.edges)
noi <- unique(c(tbl.edges$source, tbl.edges$target))
tbl.nodes <- subset(tbl.nodes, id %in% noi)
g.json.string <- cyjShiny::dataFramesToJSON(tbl.edges, tbl.nodes)
defaultLayoutStrategy <- "cose"
#------------------------------------------------------------------------------------------------------------------------
.LC.networkTab <- setClass("LC.networkTab", contains="ChinookTab")
#------------------------------------------------------------------------------------------------------------------------
#' Create a LC.networkTab object
#'
#' @description
#'
#' @rdname LC.networkTab
#'
#' @param namem  A character string
#'
#' @return An LC.networkTab object, a subclass of Chinookab
#'
#' @export
#'
LC.networkTab <- function(name, menuItemName, parentApp, quiet=TRUE)
{
   state <- new.env(parent=emptyenv())
   state$tabs <- list()
   obj <- .LC.networkTab(ChinookTab(name=name, menuItemName=menuItemName, parentApp=parentApp, quiet=quiet))

   obj

} # LC.networkTab
#------------------------------------------------------------------------------------------------------------------------
setMethod("show", "LC.networkTab",

    function(object){
        cat(sprintf("a LC.networkTab object with name '%s'\n", getName(object)))
        })

#------------------------------------------------------------------------------------------------------------------------
setMethod("createPage", "LC.networkTab",

   function(obj){

     button.style <- "padding:4px; font-size:120%";

     fluidPage(id="cyjShinyPageContent",
        includeCSS("cyjShiny.css"),
        fluidRow(
          column(
            actionButton("fitButton", "Fit", style=button.style),
            actionButton("fitSelectionButton", "Fit Selected", style=button.style),
            actionButton("sfn", "Select First Neighbor", style=button.style),
            actionButton("hideUnselected", "Hide Unselected", style=button.style),
            actionButton("showAll", "Show All", style=button.style),
            actionButton("clearSelection", "Deselect Nodes", style=button.style),
            width=6),
          column(
            selectInput("cyjSelectLayout", "",
                         c("breadthfirst", "circle", "cola", "concentric", "cose",
                           "cose-bilkent", "dagre", "grid", "random")),
            width=2)
           ),
        fluidRow(
           cyjShinyOutput('cyjShiny')
           )
        ) # fluidPage
   }) # createPage

#------------------------------------------------------------------------------------------------------------------------
# setMethod("displayPage", "LC.networkTab",
#
#    function(obj) {
#       removeUI(selector="#lowercaseViewPageContent", immediate=TRUE)
#       insertUI(selector="#lowercaseViewPage", where="beforeEnd", createPage(obj), immediate=TRUE)
#       }) # displayPage
#
#------------------------------------------------------------------------------------------------------------------------
setMethod("addEventHandlers", "LC.networkTab",

   function(obj, session, input, output) {

      printf("--- LowerCaseView::addEventHandlers")

      obj@state$session <- session
      obj@state$input <- input
      obj@state$output <- output

      output$cyjShiny <- renderCyjShiny({
         printf("renderCyjShiny")
         cyjShiny(graph=g.json.string, layoutName=defaultLayoutStrategy, styleFile="kengo-style.js")
         })
      }) # addEventHandlers

#------------------------------------------------------------------------------------------------------------------------
setMethod("handleMessage", "LC.networkTab",

     function(obj, source, destination, cmd, json.payload){
         printf("%s has message from %s, %s(%s)", getName(obj), source, cmd,
                jsonlite::fromJSON(json.payload))
        })

#------------------------------------------------------------------------------------------------------------------------
