
# ------------------------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------------------------

# -- Library for main app
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# -- Module dependencies
library(leaflet)


# ------------------------------------------------------------------------------
# Shared objects
# ------------------------------------------------------------------------------

# -- Declare path list
path <- list(code = "./R",
             data = "../data",
             resources = "./resources",
             settings = "./settings")


# ------------------------------------------------------------------------------
# Source code
# ------------------------------------------------------------------------------

# -- Source scripts
cat("Source code from:", path$code, " \n")
for (nm in list.files(path$code, full.names = TRUE, recursive = TRUE, include.dirs = FALSE))
{
  source(nm, encoding = 'UTF-8')
}
rm(nm)
