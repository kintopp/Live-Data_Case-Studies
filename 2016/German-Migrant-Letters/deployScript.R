## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

## This file allows the files to be deployed programmatically

library(shiny)
library(rsconnect)

apps.dirs <- c("america-map","europe-map","world-timeline")

deployApp("./america-map", account = "livedataoxford", appName = "german-letters_Conversation_AmericaMap", launch.browser = FALSE)

deployApp("./europe-map", account = "livedataoxford", appName = "german-letters_Conversation_EuropeMap", launch.browser = FALSE)

deployApp("./world-timeline", account = "livedataoxford", appName = "german-letters_Conversation_WorldMap", launch.browser = FALSE)





list.files()

lapply(apps.dirs, function(x) deployApp(x, name = "livedataoxford"))

applications(account="livedataoxford")$name

accounts()

accountInfo(name = "livedataoxford")
