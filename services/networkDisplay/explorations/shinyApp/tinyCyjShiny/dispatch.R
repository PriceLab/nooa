library(plumber)
library(processx)
library(callr)
#----------------------------------------------------------------------------------------------------
#* list commands
#* @get /
function()
{
  list(msg = paste0("available commands: echo, redirect (google) cy"))
}
#---------------------------------------------------------------------------------
#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="")
{
  list(msg = paste0("The message is: '", msg, "'"))
}
#---------------------------------------------------------------------------------
#' @get /google
#' @html
function(req, res) {
  res$status <- 303 # redirect
  res$setHeader("Location", "http://google.com")
"<html>
  <head>
    <meta http-equiv=\"Refresh\" content=\"0; url=http://google.com\" />
  </head>
  <body>
    <p>Please follow <a href=\"http://www.example.com/\">this link</a>.</p>
  </body>
</html>"
}
#----------------------------------------------------------------------------------------------------
#' @get /cy
#' @html
function(req, res)
{
  proc <- process$new("/usr/local/bin/R", " -f /Users/paul/github/cyjShiny/inst/examples/tinyApp/tinyApp.R")
  Sys.sleep(2)
  res$status <- 303 # redirect
  res$setHeader("Location", "http://localhost:6789")

} # /cy
#----------------------------------------------------------------------------------------------------
#' @get /cySockets
#' @html
function(req, res) {
  command <- "/usr/local/bin/Rscript"
  directory <- "/Users/paul/github/nooa/services/networkDisplay/explorations/shinyApp/tinyCyjShiny"
  scriptFile <- "cyjWithWebSockets.R"
  port <- 6001
  arg <- sprintf("%s/%s %d", directory, scriptFile, port)
  printf("command: %s", command)
  printf("   args: %s", arg)
  proc <- process$new(command, arg)
  for(i in 1:10){
     print(proc)
     Sys.sleep(0.5)
     }
  res$status <- 303 # redirect
  res$setHeader("Location", sprintf("http://localhost:%d", port))
"<html>
  <head>
    <meta http-equiv=\"Refresh\" content=\"0; url=http://localhost:9991\" />
  </head>
  <body>
    <p>Please follow <a href=\"http://www.example.com/\">this link</a>.</p>
  </body>
</html>"
}
#----------------------------------------------------------------------------------------------------
#' @get /cySock
#' @html
function(req, res) {
  command <- "/usr/local/bin/Rscript"
  directory <- "/Users/paul/github/nooa/services/networkDisplay/explorations/shinyApp/tinyCyjShiny"
  scriptFile <- "cyjWithWebSockets.R"
  full.path <- file.path(directory, scriptFile)
  port <- 6001
  processArgs <- rscript_process_options(script=full.path, cmdargs=port)
  proc <- rscript_process$new(processArgs)
  print("sleeping")
  for(i in 1:10){
     Sys.sleep(0.2)
     print(proc)
     }
  res$status <- 303 # redirect
  res$setHeader("Location", sprintf("http://localhost:%d", port))

    # return this string:
  sprintf("<html><head><meta http-equiv='Refresh' content='0; url=http://localhost:%d'/></head>
                 <body><p>redirected to shiny app</a>.</p> </body></html>", port)

} # cySock
#----------------------------------------------------------------------------------------------------

