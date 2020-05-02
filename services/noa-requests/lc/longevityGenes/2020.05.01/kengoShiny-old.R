library(shiny)
library(cyjShiny)
library(htmlwidgets)
#----------------------------------------------------------------------------------------------------
printf <- function(...) print (noquote (sprintf (...)))
#----------------------------------------------------------------------------------------------------
# one way to create a graph is via the Bioconductor graphNEL class.
# here we use the data.frame strategy.
#----------------------------------------------------------------------------------------------------
load("tbls-curated-v2.RData")
tbl.nodes$assay <- 0.0
dim(tbl.nodes)
dim(tbl.edges)
score <- -log10(tbl.edges$pval)
tbl.edges$score <- score
stopifnot(all(c(tbl.edges$source, tbl.edges$target) %in% tbl.nodes$id))
g.json.string <- cyjShiny::dataFramesToJSON(tbl.edges, tbl.nodes)

cohort.names <- colnames(tbl.nodes)[5:37]  # from F.18.25  to MF.70.89
button.style <- "padding:4px; font-size:90%"

currentFrame <- 1;

defaultLayoutStrategy <- "cola"

#----------------------------------------------------------------------------------------------------
ui = shinyUI(fluidPage(

  tags$head(
     tags$style("#cyjShiny{height:97vh !important;}")),

  sidebarLayout(
     sidebarPanel(
        selectInput("selectCohort", "Select Cohort", c("", cohort.names), selectize=FALSE),
        #actionButton("nextCohortButton", "Next Cohort", style=button.style),
        #actionButton("previousCohortButton", "previous Cohort", style=button.style),
        hr(),
        wellPanel(
          actionButton("fitButton", "Fit", style=button.style),
          actionButton("fitSelectionButton", "Fit Selected", style=button.style),
          actionButton("sfn", "Select First Neighbor", style=button.style),
          actionButton("hideUnselected", "Hide Unselected", style=button.style),
          actionButton("showAll", "Show All", style=button.style),
          actionButton("clearSelection", "Deselect Nodes", style=button.style),
          ),
        #actionButton("getSelectedNodes", "Get Selected Nodes", style=button.style),
        #htmlOutput("selectedNodesDisplay"),
        selectInput("layoutSelector", "Layout",
                    c("breadthfirst", "circle", "cola", "concentric", "cose", "cose-bilkent",
                      "dagre", "grid", "random"), selected=defaultLayoutStrategy),
        width=2,
        style="margin-top: 20px; margin-right:0px; padding-right:10px;"
        ),
     mainPanel(
        cyjShinyOutput('cyjShiny'),
        width=10,
        style="margin-left:0px; padding-left:0px; padding-right:30px;padding-top: 10px;"
        )
     ) # sidebarLayout
))
#----------------------------------------------------------------------------------------------------
server = function(input, output, session) {

   observeEvent(input$fitButton, ignoreInit=TRUE, {
      fit(session)
      Sys.sleep(0.5)
      selectNodes(session, tbl.nodes$id[sample(1:3, 1)])
      })

   observeEvent(input$fitSelectionButton, ignoreInit=TRUE, {
      fitSelected(session)
      Sys.sleep(0.5)
      selectNodes(session, tbl.nodes$id[sample(1:3, 1)])
      })

   observeEvent(input$sfn, ignoreInit=TRUE, {
      selectFirstNeighbors(session)
      })

   observeEvent(input$hideUnselected, ignoreInit=TRUE, {
      selectedNodes <- getSelectedNodes(session)
      invertSelection(session)
      hideSelection(session)
      selectNodes(session, selectedNodes)
      })


   observeEvent(input$showAll, ignoreInit=TRUE, {
      showAll(session)
      })

   observeEvent(input$clearSelection, ignoreInit=TRUE, {
      clearSelection(session)
      })


   observeEvent(input$layoutSelector, ignoreInit=TRUE, {
      layoutName <- input$layoutSelector
      doLayout(session, layoutName)
      fit(session)
      })

   observeEvent(input$selectCohort, ignoreInit=TRUE, {
      cohort.name <- input$selectCohort;
      nodeNames <- tbl.nodes$id
      if(nchar(cohort.name) == 0)
        newValues <- rep(0, nrow(tbl.nodes))
      else
        newValues <- tbl.nodes[, cohort.name]
      setNodeAttributes(session, attributeName="assay", nodes=nodeNames, newValues)
      #newValues <- runif(n=3, min=-3, max=3)
      #setNodeAttributes(session, attributeName="lfc", nodes=nodeNames, newValues)
      })

   output$value <- renderPrint({input$action})

   output$cyjShiny <- renderCyjShiny({
     printf("renderCyjShiny")
     #print(graph.json)
     #print(class(graph.json))
     cyjShiny(graph=g.json.string, layoutName=defaultLayoutStrategy, styleFile="kengo-style.js")
     })

   observeEvent(input$savePNGbutton, ignoreInit=TRUE, {
     file.name <- tempfile(fileext=".png")
     savePNGtoFile(session, file.name)
     })

   observeEvent(input$pngData, ignoreInit=TRUE, {
     printf("received pngData")
     png.parsed <- fromJSON(input$pngData)
     substr(png.parsed, 1, 30) # [1] "data:image/png;base64,iVBORw0K"
     nchar(png.parsed)  # [1] 768714
     png.parsed.headless <- substr(png.parsed, 23, nchar(png.parsed))  # chop off the uri header
     png.parsed.binary <- base64decode(png.parsed.headless)
     printf("writing png to foo.png")
     conn <- file("foo.png", "wb")
     writeBin(png.parsed.binary, conn)
     close(conn)

     })


} # server
#----------------------------------------------------------------------------------------------------
if(grepl("hagfish", Sys.info()[["nodename"]]))
   runApp(shinyApp(ui=ui,server=server), port=6769)
else  # good for docker
    shinyApp(ui=ui, server=server)


