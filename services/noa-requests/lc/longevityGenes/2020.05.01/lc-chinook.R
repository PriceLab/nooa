library(Chinook)
source("LC.networkTab.R")
homePage <- "lc-chinook-intro.html"
stopifnot(file.exists(homePage))

c <- Chinook("LC", homePage)

tab.cyj <- LC.networkTab("Network", "network", c, quiet=TRUE)
addTab(c, tab.cyj)

createApp(c, 10002)
