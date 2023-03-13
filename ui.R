###############################################
# Shiny App UI for FW Test Results Reformatting
# Hasan Sulaeman, 10/05/2020
###############################################

library(shinythemes)
library(shinydashboard)

# Dashboard Header ----
dashboard_header = dashboardHeader(title = "VRI ROC")

# Dashboard Sidebar ----
dashboard_sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("QC Received Samples", tabName = "dashboard", icon = icon("fas fa trucks")),
    menuItem("Reformat Test Results", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

# Dashboard Body ----
dashboard_body = dashboardBody()

# Dashboard Page ----
dashboardPage(dashboard_header, dashboard_sidebar, dashboard_body, skin = "black")

# UI ----
ui = fluidPage(

  titlePanel("Reformatting Test Results for Freezerworks Import"),
  theme = shinytheme("cosmo"),
  tags$h4("v1.0; 10/12/2020"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Instructions:"),
      tags$body("~TESTING~"),
      tags$hr(),
      textOutput("instructions", container = div, inline = FALSE),
      textAreaInput("test_name", "Test Name:", rows = 1,
                    placeholder = "Vitros CoV2T"),      
      tags$hr(),      
      fileInput("manifest", "Testing Manifest",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectInput("manifest_sampleid", "Sample ID Field:", "", multiple = F),
      tags$hr(),
      fileInput("results", "Testing Results",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectInput("results_sampleid","Sample ID Field:","", multiple = F),
      selectInput("results_quant","Quantitative Results Field:","", multiple = F),
      selectInput("results_qual","Qualitative Results Field:","", multiple = F),
      tags$hr(),
      textAreaInput("quant_paramname", "Quantitative Parameter Name in Freezerworks:", rows = 1,
                    placeholder = "Vitros CoV2T S/CO"),
      textAreaInput("qual_paramname", "Qualitative Parameter Name in Freezerworks:", rows = 1,
                    placeholder = "Vitros CoV2T Interpretation"),
      downloadButton("downloadData", "Download")),
    mainPanel(
      tableOutput("contents")
    )
  )
)
