###################################################
# Shiny App Server for FW Test Results Reformatting
# Hasan Sulaeman, 10/05/2020
###################################################

# Single upload only. No limitation for if the headers are not on the first row
# No test order id = not reformatted
# Need to accommodate assays with either no quant or no qual parameters

# Server ----
server = function(input, output, session) {
  # Required packages ----
  require(dplyr)
  require(tidyr)
  require(DT)
  require(yaml)
  require(glue)
  require(shinythemes)
  
  # Loading the static variables ----
  cat("\nInitializing Freezerworks Reformatting Web App..", fill = T)
  ref = read_yaml("www:/reference.yaml")
  cat("> Reference file was successfully loaded in", fill = T)
  # Serving the list of accepted test_names
  updateSelectInput(session, "test_name", choices = names(ref), selected = "")
  cat("> Web app ready", fill = T)
  identifier = "Unique Test Order ID"
  export_fnames = c(identifier, "Test Name", "Parameter Name", "Qualitative Result", "Quantitative Result")
  
  # Content ----
  fw_import_file = reactive({
    # Inputs required ----
    req(input$test_name)
    req(input$file_upload)
    
    # Pulling in variables from reference file ----
    if (input$test_name != "test_names") {
      cat(glue("\n\nTest name selected: {input$test_name}\n\n"), fill = T)
      quant_vars = ref[[input$test_name]][["quantitative"]]
      qual_vars = names(ref[[input$test_name]][["qualitative"]])
      if (!is.null(qual_vars)) {
        qual_vars_list = ref[[input$test_name]][["qualitative"]]
      }
    }
    
    # File upload ----
    # Read in as a list to look for headers
    cat(glue("\nFile uploaded: {input$file_upload$datapath}\n"), fill = T)
    file_as_list = as.list(readLines(input$file_upload$datapath))
    
    # Check for if headers aren't on first row
    header_row = grep(identifier, file_as_list, ignore.case = T)
    
    # Reading the file in as a data frame
    values = read.csv(input$file_upload$datapath, skip = header_row-1, check.names = F)
    uploaded_file = as.data.frame(values)
    
    # Field name standardization ----
    names(uploaded_file)[grep(identifier, names(uploaded_file), ignore.case = T)] = identifier
    required_fields = c(identifier, qual_vars, quant_vars)
    for (field in required_fields) {
      names(uploaded_file)[grep(field, names(uploaded_file), ignore.case = T)] = field
    }
    cat("> Field names are standardized\n\nInitiating validation of field names:", fill = T)
    
    # Saving the final field names for validation
    fnames = names(uploaded_file)

    # Perform validation ----
    #  Identifier
    cat("> Validating identifier field..", fill = T)
    validate(
      need(identifier %in% fnames, "Unique Test Order ID not found in the file")
    )
    
    #  Quantitative variable(s)
    if (!is.null(quant_vars)) {
      cat("> Validating quantitative variables..", fill = T)
      validate(
        need(all(quant_vars %in% fnames), 
             glue("Missing quantitative variable fields: {quant_vars[quant_vars %in% fnames == F]}"))
      )
    }
    
    #  Qualitative variable(s)
    if (!is.null(qual_vars)) {
      cat("> Validating qualitative variable names..", fill = T)
      validate(
        need(all(qual_vars %in% fnames), 
             glue("Missing qualitative variable fields: {qual_vars[qual_vars %in% fnames == F]}"))
      )
      for(i in 1:length(qual_vars)) {
        cat("> Validating qualitative variable entries..", fill = T)
        enums = qual_vars_list[[qual_vars[i]]]
        if (!is.null(enums)) {
          entries = unique(uploaded_file[,qual_vars[i]])
          validate(
            need(all(entries %in% enums), glue("Values for {qual_vars[i]} are incorrect"))
          )
        }
      }
    }
    
    # Pull the required fields from the file ----
    uploaded_file %>% 
      select(all_of(required_fields)) %>% 
      na.omit() -> results
    
    # Reformatting qualitative parameters ----
    cat("> Validation complete\n\nInitiating reformatting:", fill = T)
    if (!is.null(qual_vars)) {
      cat("> Reformatting qualitative variables..", fill = T)
      qual_results = results %>%
        pivot_longer(cols = qual_vars, names_to = "Parameter Name", 
                     values_to = "Qualitative Result") %>%
        mutate(`Test Name` = input$test_name,
               `Quantitative Result` = "") %>%
        select(all_of(export_fnames))
    }
    
    # Reformatting quantitative parameters ----
    if (!is.null(quant_vars)) {
      cat("> Reformatting quantitative variables..", fill = T)
      quant_results = results %>%
        pivot_longer(cols = quant_vars, names_to = "Parameter Name", 
                     values_to = "Quantitative Result") %>%
        mutate(`Test Name` = input$test_name,
               `Qualitative Result` = "") %>%
        select(all_of(export_fnames))
    }
    
    # Binding reformatted data frames ----
    bound = rbind(qual_results, quant_results) %>% arrange(`Unique Test Order ID`)
    import_file = as.data.frame(bound)
    cat("> Reformatting complete\n", fill = T)
    return(import_file)
  })
  
  # Table rendering ----
  output$contents = renderTable(fw_import_file())
  
  # Output for Download ----
  output$download_table = downloadHandler(
    filename = paste0(tolower(gsub(" ","_",input$test_name)),"_",format(Sys.Date(), "%Y%m%d"),".csv"),
    content = function(filename) {write.csv(fw_import_file(), filename, row.names = F)}
  )
}