library(shiny)
library(DT)
options(warn=2)  # warning are turned into errors
#----------------------------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
#----------------------------------------------------------------------------------------------------
tbl.summary <- get(load("tbl.summary.252x3.RData"))
tbl.summary <- tbl.summary[, c("Gene_Symbol", "speciesCount", "HsOrtholog_Gene_ID")]
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
   titlePanel("Longevity-associated Genes, Across Species"),
   fluidRow(wellPanel(
       selectInput("selectDestination",
                   label="Table selection goes to",
                   c("NA", "HomoloGene", "GeneCards")),
       style="padding-bottom:0px; float: right; width: 200px;")),
   tabsetPanel(type="tabs", id="lcGenesTabs",
       tabPanel(title="By Gene", value="byGeneTab",
                wellPanel(DTOutput("table"), style="margin-top:5px;")),
       tabPanel(title="GeneCard", value="geneCardTab",
                wellPanel(htmlOutput("geneCardsDisplay"))),
       tabPanel(title="HomoloGene", value="homoloGeneTab",
                wellPanel(htmlOutput("homologeneDisplay")))),
   style="margin: 10px; margin-top: 5px;"
   ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(session, input, output) {

   output$table <- DT::renderDataTable({
       DT::datatable(tbl.summary,
                     options=list(pageLength=-1,
                                  dom='<lfip<t>>',
                                  paging=FALSE),
                     selection="single",
                     class='nowrap display')
      })

    observeEvent(input$table_rows_selected, {
       selectedTableRow <- isolate(input$table_rows_selected)
       gene <- tbl.summary[selectedTableRow, "Gene_Symbol"]
       destination <- isolate(input$selectDestination)
       tabName <- switch(destination,
                         "GeneCards" = "geneCardTab",
                         "HomoloGene" = "homoloGeneTab")
       updateTabsetPanel(session, "lcGenesTabs", selected=tabName)
       printf("send %s to %s", gene, destination)
       }) # observe row selection event

    output$NOgeneCardsDisplay <- renderUI({
       tableRow <- input$table_rows_selected
       selectDestination <- isolate(input$selectDestination)
       goi <- tbl.summary[tableRow, "Gene_Symbol"]
       printf("selectedTableRow: %d", tableRow)
       printf("goi: %s", goi)
       printf("selectDestination: %s", selectDestination)
       if(selectDestination == "GeneCards"){
          printf("asking for tab raise");
          updateTabsetPanel(session, "lcGenesTabs", selected="geneCardTab")
          uri <- switch(selectDestination,
                      "HomoloGene" = sprintf("https://www.ncbi.nlm.nih.gov/homologene/?term=%s", goi),
                      "GeneCards"  = sprintf("https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s", goi)
                      )
          printf("uri: %s", uri)
          my_test <- tags$iframe(src=uri, height=1000, width="100%")
          my_test
          }
       })

    output$NOhomologeneDisplay <- renderUI({
       tableRow <- input$table_rows_selected
       selectDestination <- isolate(input$selectDestination)
       goi <- tbl.summary[tableRow, "Gene_Symbol"]
       printf("selectedTableRow: %d", tableRow)
       printf("goi: %s", goi)
       printf("selectDestination: %s", selectDestination)
       if(selectDestination == "HomoloGene"){
          uri <- sprintf("https://www.ncbi.nlm.nih.gov/homologene/?term=%s", goi)
          printf("uri: %s", uri)
          my_test <- tags$iframe(src=uri, height=1000, width="100%")
          updateTabsetPanel(session, "lcGenesTabs", selected="GeneCard")
          my_test
          }
      })


   } # server

#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui=ui, server=server), port=9999)

