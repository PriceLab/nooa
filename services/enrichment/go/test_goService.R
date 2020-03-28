library(httr)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
    test_simpleEcho()
    # test_goService()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_simpleEcho <- function()
{
    msg <- "helloGo"
    uri <- sprintf("http://localhost:8000/echo?msg=%s", msg)
    r <- GET(uri)
    msg.response <- content(r)$msg[[1]]
    checkEquals(msg.response, sprintf("The message is: '%s'", msg))

} # test_simpleEcho
#------------------------------------------------------------------------------------------------------------------------
