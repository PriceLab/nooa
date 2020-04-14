library(shiny)

ui = shinyUI(fluidPage(
textInput("symbol", "Symbol Entry", ""),

dateInput("date_start", h4("Start Date"), value = "2005-01-01" ,startview = "year"),

selectInput("period_select", label = h4("Frequency of Updates"),
            c("Monthly" = 1,
              "Quarterly" = 2,
              "Weekly" = 3,
              "Daily" = 4)),

sliderInput("smaLen", label = "SMA Len",min = 1, max = 200, value = 115),br(),

checkboxInput("usema", "Use MA", FALSE)

))

server <-  shinyServer(function(input, output,session) {
observe({
 query <- parseQueryString(session$clientData$url_search)

 for (i in 1:(length(reactiveValuesToList(input)))) {
  nameval = names(reactiveValuesToList(input)[i])
  valuetoupdate = query[[nameval]]

  if (!is.null(query[[nameval]])) {
    if (is.na(as.numeric(valuetoupdate))) {
      updateTextInput(session, nameval, value = valuetoupdate)
    }
    else {
      updateTextInput(session, nameval, value = as.numeric(valuetoupdate))
    }
  }

 }

 })
}) # shinyServer
runApp(shinyApp(ui=ui,server=server), port=9997)


