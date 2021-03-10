## Tony Johnson
## Last modified 12/9/2020 to allow manual taring and add leeway for initial
## subzero data during the pull initialization and debug.
## Pulls serial data from isometric pull force device
## For the purpose of visualization
## NOTE: assumes device is ONLY serial connection on computer!!
## imports functions from "serialsource_app.R"

## libraries
rm(list=ls())

#######################################################################
## Shiny Gui setup
#######################################################################


ui <- ...
server <- ...
shinyApp(ui=ui, server=server)
