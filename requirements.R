# SQI Shiny App Launcher
# Run this script to start the Soil Quality Index application

# Check and install required packages
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "leaflet",
  "readxl", "writexl", "tidyverse", "FactoMineR", "factoextra", 
  "corrplot", "sf", "terra", "gstat", "automap", "sp", "caret"
)

# Function to check and install packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Install missing packages
cat("Checking required packages...\n")
install_if_missing(required_packages)

# Source and run the application
cat("Starting SQI Shiny Application...\n")
source("app.R")

# The app will start automatically when sourced
cat("Application should now be running in your browser!\n")
cat("If it doesn't open automatically, check the R console for the URL\n")