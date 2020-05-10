# Sys.setlocale("LC_ALL", "C")

library(shiny)
library(DT)
options(warn=2)  # warning are turned into errors
library(shinyBS)
library(yaml)
#----------------------------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
#----------------------------------------------------------------------------------------------------
tooltips <- yaml.load_file("tooltips.yaml")
for(i in 1:length(tooltips)) tooltips[[i]]$text <- paste(tooltips[[i]]$text, collapse=" ")
printf("length of tooltips read: %d", length(tooltips))
# tt <- list(longevityTab=list(selector="DataTables_Table_0_wrapper > div > div:nth-child(3) > div > div.dataTables_scrollHead > div > table > thead > tr > th:nth-child(2)",
#                             text=paste0('[+-] causal evidence<br>',
#                                        '[+-]? correlational only<br>',
#                                         '+/- direction unknown, causal<br>',
#                                         '+/-? unknown, correlational')))


tbl.summary <- get(load("tbl.summary.1010x6.RData"))
tbl.summary <- tbl.summary[, c("Gene", "Longevity", "Feature", "Function", "PMID")]
colnames(tbl.summary)[3] <- "Longevity Feature"

tbl.orthologsBySpecies <- get(load("tbl.orthologsBySpecies.9999x4.RData"))
#----------------------------------------------------------------------------------------------------
# https://docs.google.com/document/d/1Qwj9-vj8Q7b0GWLCs5UmHirQqGkejMex5w11E-CXLvU/edit?usp=sharing
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
   includeCSS("lcGenes.css"),
   titlePanel("Longevity-associated Genes & Homologies"),
   fluidRow(wellPanel(
       selectInput("selectDestination",
                   label="Table selection goes to",
                   c("NA", "HomoloGene", "GeneCards", "PubMed", "Orthologs", "Notes")),
       with(tooltips[[1]], bsTooltip(selector, text, location, options = list(container = "body", html=TRUE))),
       with(tooltips[[2]], bsTooltip(selector, text, location, options = list(container = "body", html=TRUE))),
       with(tooltips[[3]], bsTooltip(selector, text, location, options = list(container = "body", html=TRUE))),
       with(tooltips[[4]], bsTooltip(selector, text, location, options = list(container = "body", html=TRUE))),
       with(tooltips[[5]], bsTooltip(selector, text, location, options = list(container = "body", html=TRUE))),
       style="padding-bottom:0px; float: right; width: 200px;")),
   tabsetPanel(type="tabs", id="lcGenesTabs",
       tabPanel(title="By Gene", value="byGeneTab",
                wellPanel(DTOutput("geneTable"), style="margin-top:5px;")),
       tabPanel(title="GeneCard", value="geneCardsTab",
                wellPanel(htmlOutput("geneCardsDisplay"))),
       tabPanel(title="HomoloGene", value="homoloGeneTab",
                wellPanel(htmlOutput("homologeneDisplay"))),
       tabPanel(title="PubMed", value="pubmedTab",
                wellPanel(htmlOutput("pubmedDisplay"))),
       tabPanel(title="Orthologs", value="orthologsTab",
                wellPanel(DTOutput("orthologsTableDisplay"), style="margin-top:5px;")),
       tabPanel(title="Notes & Comments", value="notesAndCommentsTab",
                wellPanel(htmlOutput("notesAndCommentsDisplay"))),
       tabPanel("Help", includeHTML("help.html"))),
   style="margin: 10px; margin-top: 5px;"
   ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(session, input, output) {

   reactiveInputs <- reactiveValues(gene="",
                                    destination="",
                                    pmid="",
                                    homogeneQuery=FALSE,
                                    geneCardsQuery=FALSE,
                                    pubmedQuery=FALSE,
                                    orthologsQuery=FALSE,
                                    notesQuery=FALSE
                                    )

   addTooltip(session, id="selectDestination", title="destination",
               placement = "bottom", trigger = "hover",   options = NULL)

   output$geneTable <- DT::renderDataTable({
       DT::datatable(tbl.summary,
                     rownames=FALSE,
                     options=list(pageLength=10,
                                  dom='<lfip<t>>',
                                  scrollX=TRUE,
                                  autoWidth=TRUE,
                                  columnDefs=list(list(width="10%", targets=c(0,1)),
                                                  list(width="40%", targets=c(2,3)),
                                                  list(width="10%", targets=4)),
                                  paging=TRUE),
                     selection="single")

      })

   observeEvent(input$geneTable_rows_selected, {
      selectedTableRow <- isolate(input$geneTable_rows_selected)
      gene <- tbl.summary[selectedTableRow, "Gene"]
      printf("--- geneTable_rows_selected: %s", gene);
      pubmedIds <- tbl.summary[selectedTableRow, "PMID"]
         # but see https://www.html5rocks.com/en/tutorials/security/sandboxed-iframes/
         # https://github.com/niutech/x-frame-bypass
      if(!grepl(",", pubmedIds))
          pubmedIds <- sprintf("%s, %s", pubmedIds, pubmedIds)
      printf("pubmedIDs: %s", pubmedIds)
      destination <- isolate(input$selectDestination)
      printf("destination: %s", destination)

      tabName <- "byGeneTab"
      if(destination == "GeneCards"){
         print(1)
         tabName <- "geneCardsTab"
         print(2)
         reactiveInputs$geneCardsQuery <- TRUE
         print(3)
         }

      if(destination == "HomoloGene"){
         print(4)
         tabName <- "homoloGeneTab"
         print(5)
         reactiveInputs$homologeneQuery <- TRUE
         print(6)
         }


      #switch(destination,
      #       "GeneCards" = {
      #       "HomoloGene" = {reactiveValues$homoloGeneQuery <- TRUE},
      #       "PubMed" = {reactiveValues$pubmedQuery <- TRUE},
      #       "Orthologs" = {reactiveValues$orthologsQuery <- TRUE})
      updateTabsetPanel(session, "lcGenesTabs", selected=tabName)
      printf("send %s to %s", gene, destination)
      reactiveInputs$gene <- gene
      reactiveInputs$destination <- destination
      reactiveInputs$pmid <- pubmedIds
      }) # observe row selection event

    output$orthologsTableDisplay <- DT::renderDataTable({
       goi <- isolate(reactiveInputs$gene)
       if(reactiveInputs$orthologQuery){
         printf("--- rendering orthologsTableDisplay")
         printf("goi: %s", goi)
         printf("subset tbl.orthologsBySpecies by %s", goi)
         tbl.orthoSub <- subset(tbl.orthologsBySpecies, HsOrtholog_Gene_Symbol==goi)
         DT::datatable(tbl.orthoSub, rownames=FALSE, options=list(paging=FALSE))
         } # if Orthologs
      }) # orthologsDisplay

    output$geneCardsDisplay <- renderUI({
      goi <- isolate(reactiveInputs$gene)
      if(reactiveInputs$geneCardsQuery){
         printf("--- rendering geneCardsDisplay %s", goi)
         uri <- sprintf("https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s", goi)
         printf("uri: %s", uri)
         htmlText <- tags$iframe(src=uri, height=1000, width="100%")
         htmlText
         }
       }) # geneCardsDisplay

    output$homologeneDisplay <- renderUI({
       if(reactiveInputs$destination == "HomoloGene"){
          printf("--- rendering homologeneDisplay")
          goi <- reactiveInputs$gene
          doi <- reactiveInputs$destination
          printf("goi: %s", goi)
          printf("doi: %s", doi)
          uri <- sprintf("https://www.ncbi.nlm.nih.gov/homologene/?term=%s", goi)
          printf("uri: %s", uri)
          htmlText <- tags$iframe(src=uri, height=1000, width="100%")
          htmlText
          } # HomoloGene
       }) # homologeneDisplay


    output$pubmedDisplay <- renderUI({
       if(reactiveInputs$destination == "PubMed"){
          printf("--- rendering pubmedDisplay")
          pmids <- reactiveInputs$pmid
          doi <- reactiveInputs$destination
          printf("pmids: %s", pmids)
          printf("doi: %s", doi)
          uri <- sprintf("https://www.ncbi.nlm.nih.gov/pubmed/%s", pmids)
          printf("uri: %s", uri)
          #browseURL(sprintf("https://www.ncbi.nlm.nih.gov/pubmed/%s", "25677554"))
          htmlText <- tags$iframe(src=uri, height=1000, width="100%")
          #"NCBI restrictions required us to open a new browser page"
          htmlText
          }
       }) # pubmedDisplay

    output$notesAndCommentsDisplay <- renderUI({
       printf("--- notesAndComment")
       uri <- sprintf("https://docs.google.com/document/d/1Qwj9-vj8Q7b0GWLCs5UmHirQqGkejMex5w11E-CXLvU/edit?usp=sharing")
       printf("uri: %s", uri)
       htmlText <- tags$iframe(src=uri, height=1000, width="100%")
       htmlText
       }) # notesAndCommentsDisplay

   } # server

#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui=ui, server=server), port=9003)
#shinyApp(ui=ui, server=server)




