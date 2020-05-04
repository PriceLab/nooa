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
   tabsetPanel(
       tabPanel("By Gene", wellPanel(DTOutput("table"), style="margin-top:5px;")),
       tabPanel("Tissue",  wellPanel()),
       tabPanel("Species Homologs", wellPanel())),
       style="margin: 10px; margin-top: 5px;"
   ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(session, input, output) {

   output$table <- DT::renderDataTable({
       DT::datatable(tbl.summary,
                     options=list(pageLength=-1,
                                  dom='<lfip<t>>',
                                  paging=FALSE),
                     class='nowrap display')
      })
   } # server

#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui=ui, server=server), port=9999)

