library(plumber)
library(callr)
library(httpuv)
#----------------------------------------------------------------------------------------------------
websocket <- NULL
envApp <- new.env(parent=emptyenv())
#----------------------------------------------------------------------------------------------------
#* list commands
#* @get /
#* @html
function()
{
  s <- "<html><head><title>dispatcher</title></head>
       <body><h3>Dispatch</h3>
       <ul>
         <li><a href='http://google.com'>google</a>
         <li><a href='http://localhost:8004/cy'>cyjws</a>
       </ul></body></html"
  s

}
#---------------------------------------------------------------------------------
#' @get /google
#' @html
function(req, res) {
  res$status <- 303 # redirect
  res$setHeader("Location", "http://google.com")
"<html>
  <head>
    <meta http-equiv='Refresh' content='0; url=http://google.com' />
  </head>
  <body>
    <p>Please follow <a href='http://www.example.com/'>this link</a>.</p>
  </body>
</html>"
}
#----------------------------------------------------------------------------------------------------
#' @get /cy
#' @html
function(req, res)
{
  command <- "/usr/local/bin/Rscript"
  directory <- "/Users/paul/github/nooa/services/networkDisplay/explorations/shinyApp/plumberSockets"
  scriptFile <- "cyjws.R"
  full.path <- file.path(directory, scriptFile)
  port <- 6002
  processArgs <- rscript_process_options(script=full.path, cmdargs=port)
  proc <- rscript_process$new(processArgs)
  print("sleeping")
  Sys.sleep(2)
  res$status <- 303 # redirect
  res$setHeader("Location", sprintf("http://localhost:%d", port))

    # return this string:
  sprintf("<html><head><meta http-equiv='Refresh' content='0; url=http://localhost:%d'/></head>
                 <body><p>redirected to shiny app</a>.</p> </body></html>", port)

} # cySock
#----------------------------------------------------------------------------------------------------
createWebSocketServer <- function()
{
  callFunction <- function(req){
     list(
        status = 200L,
        headers = list('Content-Type' = 'text/html'),
        body = c(file="index.html")
        )
      } # callFunction

  onOpenFunction <- function(ws){
     websocket <<- ws
     ws$onMessage(function(binary, message) {
       printf("message received: %s", message)
       ws$send(toupper(message))
       })
     } # onOpenFunction

  envApp <- new.env(parent=emptyenv())
  envApp$call <- callFunction
  envApp$onWSOpen <- onOpenFunction
  browseURL("http://localhost:9455/")
  s <- startServer("0.0.0.0", 9455, envApp)

} # createWebSocketServer
#----------------------------------------------------------------------------------------------------
