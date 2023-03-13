###############################################
# Shiny App UI for FW Test Results Reformatting
# Hasan Sulaeman, v2.0;05/15/2023
###############################################

# Dependencies ----
library(shinydashboard)

# Dashboard Page ----
# Header
dashboard_header = dashboardHeader()
# Sidebar
dashboard_sidebar = dashboardSidebar()
# Body
dashboard_body = dashboardBody()
# Build the page
dashboardPage(dashboard_header, dashboard_sidebar, dashboard_body, skin = "black")

# UI ----
ui = fluidPage(
  titlePanel("Test Results Reformatting Web App for Freezerworks Import", 
             windowTitle = "FW Reformat Web App"),
  theme = "www:/bootstrap_lux.css",
  tags$h4("v2.1 (Beta)"),
  # All fields are in the sidebar
  sidebarLayout(
    # Header for the instructions
    sidebarPanel(
      # Test Name
      selectInput("test_name", "Test Name", choices = "test_names", selected = NULL),      
      tags$hr(),
      # File Upload
      fileInput("file_upload", "File Upload", multiple = F, accept = c("text/csv",".csv")),
      tags$hr(),
      # Download
      downloadButton("download_table", "Download")),
    mainPanel(
      # Table output for user review if successful
      tableOutput("contents")
    )
  )
)
