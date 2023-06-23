###################################
# Installation of Required Packages
# v1.0
###################################

# Defining the required packages ----
pkgs = c("dplyr",
         "tidyr",
         "DT",
         "yaml",
         "glue",
         "shinythemes",
         "shinyWidgets",
         "shinydashboard",
         "shinydashboardPlus")

# Download required packages (if needed) ----
if ("librarian" %in% row.names(installed.packages())) {
  librarian::stock(pkgs)
} else {
  install.packages("librarian")
  librarian::stock(pkgs)
}
