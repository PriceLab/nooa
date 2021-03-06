library(jsonlite)
library(shiny)
library(cyjShiny)
library(websocket)
#----------------------------------------------------------------------------------------------------
# one way to create a graph is via the Bioconductor graphNEL class.
# here we use the data.frame strategy.
# websocket additions from https://github.com/rstudio/shiny-examples/blob/master/147-websocket/app.R
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
WEB.SOCKET.URL <- "ws://localhost:9455"

port <- 9991  # a default value, usually overridden from the command line
args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0)
    port <- as.integer(args[1])
print(port)
#----------------------------------------------------------------------------------------------------
tbl.nodes <- data.frame(id=c("A", "B", "C"),
                        type=c("kinase", "TF", "glycoprotein"),
                        lfc=c(-3, 1, 1),
                        count=c(0, 0, 0),
                        stringsAsFactors=FALSE)

tbl.edges <- data.frame(source=c("A", "B", "C"),
                        target=c("B", "C", "A"),
                        interaction=c("phosphorylates", "synthetic lethal", "unknown"),
                        stringsAsFactors=FALSE)

graph.json <- toJSON(dataFramesToJSON(tbl.edges, tbl.nodes), auto_unbox=TRUE)
#----------------------------------------------------------------------------------------------------
ui = shinyUI(fluidPage(

  tags$head(
     tags$style("#cyjShiny{height:95vh !important;}")),
  shinyjs::useShinyjs(),

  sidebarLayout(
     sidebarPanel(
        fluidRow(
          column(12,
                 h1("WebSocket client", style = "text-align: center;"),
                 tags$p(
                     tags$strong("Status:"),
                     textOutput("wsStatus", inline = TRUE)
                     ),
                 wellPanel(
                     textInput("textMessageInput", "Message to send:"),
                     actionButton("sendButton", "Send"),
                     actionButton("closeButton", "Close"),
                     actionButton("debugMessageButton", "debug message")
                    ),
                  tags$strong("Messages received:")
                  )
           ),
        actionButton("selectRandomNodeButton", "Select random node"),
        hr(),
        selectInput("visualStyleSelector", "Select Visual Style",
                    choices=c("Default" = "basicStyle.js", "Biological"="biologicalStyle.js")),
        h6("Send random node 'lfc' attributes (visible only with Biological Style, mapped to color):"),
        actionButton("randomNodeAttributes", "Send"),
        h6("Try out png-saving capability, using the currently displayed network"),
        actionButton("savePNGbutton", "Save PNG to 'foo.png'"),
        h3(textOutput("updatableText")),
        width=3
        ),

     mainPanel(
      tabsetPanel(type = "tabs",
          tabPanel("network",  cyjShinyOutput('cyjShiny')),
          tabPanel("Debug", tableOutput("wsTableOutput"))
          ),
        width=9
        )
     ) # sidebarLayout
))
#----------------------------------------------------------------------------------------------------
server = function(input, output, session) {

    # https://gist.github.com/tgirke/8b9abe202c59bca72012ddeb79303e56


   output$updatableText<- renderText({
        s <- session$clientData$url_search
        paste("search? ", s)
        })
   observe({
       query <- parseQueryString(session$clientData$url_search)
       print("url_search!")
       })

   observeEvent(input$selectRandomNodeButton, ignoreInit=TRUE, {
      clearSelection(session)
      selectNodes(session, tbl.nodes$id[sample(1:3, 1)])
      })

   observeEvent(input$visualStyleSelector, ignoreInit=TRUE, {
      newStyleFile <- input$visualStyleSelector
      printf("newStyle: %s", newStyleFile)
      loadStyleFile(newStyleFile)
      })

   observeEvent(input$randomNodeAttributes, ignoreInit=TRUE, {
      nodeNames <- tbl.nodes$id
      newValues <- runif(n=3, min=-3, max=3)
      setNodeAttributes(session, attributeName="lfc", nodes=nodeNames, newValues)
      })

   output$value <- renderPrint({ input$action })
   output$cyjShiny <- renderCyjShiny({
     printf("renderCyjShiny")
     print(graph.json)
     print(class(graph.json))
     cyjShiny(graph=graph.json, layoutName="cola", styleFile="basicStyle.js")
     })

   observeEvent(input$savePNGbutton, ignoreInit=TRUE, {
     file.name <- tempfile(fileext=".png")
     savePNGtoFile(session, file.name)
     })

     # https://github.com/rstudio/shiny/issues/1418
    observeEvent(session$clientData, ignoreInit=TRUE,{
        printf("session$clientData!")
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

   setupWebSocket(input, output, session)

} # server
#----------------------------------------------------------------------------------------------------
setupWebSocket <- function(input, output, session){

   status <- reactiveVal("Waiting for input")
   history <- reactiveVal(
     data.frame(Date = NULL, Message = NULL)
     )

  setEnabled <- function(enable) {
    withReactiveDomain(session, {
      shinyjs::toggleState("textMessageInput", enable)
      shinyjs::toggleState("sendButton", enable)
      shinyjs::toggleState("closeClose", enable)
      })
    }
    setEnabled(TRUE)

    appendToHistory <- function(x) {
       old <- isolate(history())
       new <- data.frame(
         Date = format(Sys.time()),
         Message = x,
         stringsAsFactors = FALSE)
       history(rbind(new, old))
       }


  connect <- function(url) {
    ws <- WebSocket$new(WEB.SOCKET.URL)
    status(paste0("Connecting to ", url, ", please wait..."))
    ws$onError(function(event) {
      setEnabled(FALSE)
      status(paste0("Error: ", event$message))
      })
    ws$onMessage(function(event) {
      newMessage <- fromJSON(event$data);
      status("new websocket message")
      appendToHistory(newMessage)
      status(paste0("newMessage: ", newMessage))
      if(is.list(newMessage)){
          if("cmd" %in% names(newMessage)){
             cmd <- newMessage[["cmd"]]
             payload <- newMessage[["payload"]]
             #status(paste0("CMD: ", cmd))
             if(cmd == "selectNodes"){
                #status(paste0("asking session to selectNodes: ", payload))
                selectNodes(session, payload) #c("A", "B"))
                }
             } # cmd field in message
          } # newMessage is a list
      })
    ws$onOpen(function(event) {
      status(paste0("Connected to ", WEB.SOCKET.URL))
      setEnabled(TRUE)
      })
    ws$onClose(function(event) {
      setEnabled(FALSE)
      status(paste0("Closed: ", event$code, " - ", event$reason))
      })
    ws
    } # connect

    ws <<- connect(WEB.SOCKET.URL)

  observeEvent(input$sendButton, {
    printf("--- sendButton, sending message to ws")
    msg <- input$textMessageInput
    printf("--- msg: %s", msg)
    printf("---- ws")
    print(ws)
    ws$send(msg)
    updateTextInput(session, "input", value = "")
    })

  observeEvent(input$closeButton, {
    ws$close()
    })

  observeEvent(input$debugMessageButton, {
    old <- isolate(history())
    new <- data.frame(
       Date = format(Sys.time()),
       Message = "abracadabra",
       stringsAsFactors = FALSE)
    history(rbind(new, old))
    })

  output$wsTableOutput <- renderTable(width = "100%", {
    history()
    })

  output$wsStatus <- renderText({
    status()
    })

} # setupWebSocket
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui=ui,server=server), port=port)
