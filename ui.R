###############################################
# Shiny App UI for FW Test Results Reformatting
# Hasan Sulaeman, v2.0;05/15/2023
###############################################

# Dependencies ----
require(shinyWidgets)
require(shinydashboard)
require(shinydashboardPlus)

# UI ----
theme = "www:/bootstrap_lux.css"

# Dashboard ----
header = dashboardHeader()

# Sidebar
sidebar = dashboardSidebar(
  sidebarMenu(
  menuItem("Import Reformatting", tabName = "import", icon = icon("import", lib = "glyphicon")),
  menuItem("Export Reformatting", tabName = "export", icon = icon("export", lib = "glyphicon")),
  width = 3
  ),
  collapsed = T
)

# Body
body = dashboardBody(
  tabItems(
    # Import reformatting page ----
    tabItem(tabName = "import",
            fluidRow(
              box(
                title = "Import File Reformatting",
                # Test name
                selectInput("test_name", "Test Name", choices = ""),
                # File upload
                fileInput("file_upload", "File Upload", multiple = F, accept = ".csv"),
                # Download button
                uiOutput("download_import_btn"),
                # Other arguments
                width = 3
              ),
              column(
                # Table output for user review if successful
                tableOutput("contents_import"), width = 9
              )
            )
    ),
    # Export reformatting page ----
    tabItem(tabName = "export",
            fluidRow(
              box(
                title = "Export File Reformatting",
                # Select an identifier
                selectInput("key_selection_reporting", "Identifier", 
                            choices = c("Freezerworks ID", "Unique Aliquot ID"), 
                            selected = NULL),
                # Upload the file
                fileInput("file_upload_reporting", "File Upload", multiple = F, accept = ".csv"),
                # Download button
                uiOutput("download_export_btn"),
                # Other arguments
                width = 3
              ),
              column(
                # Table output for user review if successful
                tableOutput("contents_reporting"),
                width = 9
              )
            ),
    )
  )
)

# Build the dashboard page ----
dashboardPage(header, sidebar, body)
