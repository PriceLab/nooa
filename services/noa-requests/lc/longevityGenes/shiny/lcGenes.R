library(shiny)
library(DT)
options(warn=2)  # warning are turned into errors
#----------------------------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
#----------------------------------------------------------------------------------------------------
tbl.summary <- get(load("tbl.summary.252x3.RData"))
db.file.name <- "./tms.brand105.fimo2.sqlite"
directory.01 <- "~/github/TrenaMultiScore/misc/erythropoiesis/marjorieDemos/shiny/data"
directory.02 <- "/home/shiny/data"
file.path.01 <- file.path(directory.01, db.file.name)
file.path.02 <- file.path(directory.02, db.file.name)
if(file.exists(file.path.01)){
    printf("loading sqlite db from %s", file.path.01)
    full.path <- file.path.01
}else if(file.exists(file.path.02)){
    printf("loading sqlite db from %s", file.path.02)
    full.path <- file.path.02
} else {
    stop(sprintf("sqlite database '%s' not found", full.path))
    }
#----------------------------------------------------------------------------------------------------
tfs <- c("any", "fu", "bar")
targetGenes <- c("any", "bar", "fu")
length(targetGenes)
log.max.abs.tss <- 6 # round(log(max(abs(tbl$tss))) + 0.5)
max.gh.score <- 700 # round(max(tbl$gh) + 0.5)
max.motif.score <- 10
#----------------------------------------------------------------------------------------------------
ui = fluidPage(
   titlePanel("Longevity-associated Genes, Across Species"),
   sidebarLayout(
      sidebarPanel(

         selectInput("tf", "Transcription Factor:", tfs, selectize=FALSE, selected="TBX15"),
         selectInput("targetGene", "Target Gene:", targetGenes, selectize=FALSE, selected="PDZK1IP1"),
         sliderInput("absCorrelation", "abs(cor):", min = 0, max = 1.0, value = c(0.0, 1.0)),
         sliderInput("absTSS", "log(abs(tss)):", min = 0, max = log.max.abs.tss,
                     value = c(0, log.max.abs.tss)),
         sliderInput("motifScore", "motif score:", min=0, max=max.motif.score, value=c(2, max.motif.score)),

         sliderInput("geneHancer", "GeneHancer combined score:", min = 0, max = max.gh.score,
                     value = c(0, max.gh.score)),
         sliderInput("phast30", "PhastCons30:", min = 0, max = 1, value = c(0, 1)),
         radioButtons("ChIP", "ChIP", choices = c("Yes", "No", "Either"), selected = "Either",
                      inline = TRUE, width = NULL, choiceNames = NULL,  choiceValues = NULL),
         width=2),
      mainPanel(
         DTOutput("table")
         )
      ) # sidebarLayout
   ) # fluidPage

#----------------------------------------------------------------------------------------------------
server = function(session, input, output) {

   output$table <- DT::renderDataTable({
      queryElements <- c()
      if(input$tf != "any")
          queryElements <- c(queryElements, sprintf("tf='%s'", input$tf))
          #tbl <- subset(tbl, tf==input$tf)
      if(input$targetGene != "any")
         queryElements <- c(queryElements, sprintf("targetGene='%s'", input$targetGene))
         #tbl <- subset(tbl, targetGene == input$targetGene)
      min <- input$absCorrelation[1]
      max <- input$absCorrelation[2]
      queryElements <- c(queryElements, sprintf("abs(cor) >= %f AND abs(cor) <= %f", min, max))

      min <- (input$absTSS[1])
      max <- (input$absTSS[2])
      queryElements <- c(queryElements, sprintf("log10(abs(tss)+0.00001) >= %f AND log10(abs(tss)+0.00001) <= %f", min, max))

      min <- input$motifScore[1]
      max <- input$motifScore[2]
      queryElements <- c(queryElements, sprintf("motifScore >= %f AND motifScore <= %f", min, max))

      min <- input$phast30[1]
      max <- input$phast30[2]
      queryElements <- c(queryElements, sprintf("phast30 >= %f AND phast30 <= %f", min, max))

      min <- input$geneHancer[1]
      max <- input$geneHancer[2]
      queryElements <- c(queryElements, sprintf("gh >= %f AND gh <= %f", min, max))

      chip <- input$ChIP;

      DT::datatable(tbl.summary, options=list(pageLength=100, dom='<lfip<t>>'), class='nowrap display')
      })
   } # server

#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui=ui, server=server), port=9999)

