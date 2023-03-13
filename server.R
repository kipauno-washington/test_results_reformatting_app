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
    # Inputs required
    req(input$test_name)
    req(input$file_upload)
    
    # Pulling in variables
    if (input$test_name != "test_names") {
      cat(paste0(input$test_name," selected"), fill = T)
      quant_vars = ref[[input$test_name]][["quantitative"]]
      cat(paste0("> Quantitative Variables: ", quant_vars), fill = T)
      qual_vars = names(ref[[input$test_name]][["qualitative"]])
      cat(paste0("> Qualitative Variables: ",qual_vars), fill = T)
      if (!is.null(qual_vars)) {
        qual_vars_list = ref[[input$test_name]][["qualitative"]]
        cat(paste0("> Qualitative enums: ",qual_vars_list), fill = T)
      }
    }
    
    # File upload
    cat(paste0("File uploaded: ", input$file_upload$datapath), fill = T)
    # Read in as a list to look for headers
    file_as_list = as.list(readLines(input$file_upload$datapath))
    # Check for if headers aren't on first row
    header_row = grep(gsub(" ", ".", identifier), file_as_list)
    cat(paste0("> Header row found on line no: ",header_row,"\n"), fill = T)
    
    # Reading the file in
    values = read.csv(input$file_upload$datapath, skip = header_row-1, check.names = F)
    uploaded_file = as.data.frame(values)

    # Perform validation
    cat("Validating the uploaded file..", fill = T)
    #  Identifier
    if (identifier %in% names(uploaded_file) == F) {
      cat(paste0("> ",identifier," field not found"), fill = T)
      cat(paste0("> ",names(uploaded_file)), fill = T)      
    } else {
      cat("> identifier field present in the file", fill = T)
    }
    #  Quantitative variable(s)
    if (!is.null(quant_vars)) {
      if (F %in% (quant_vars %in% names(uploaded_file))) {
        cat("> Missing quant variables in the uploaded file", fill = T)
      } else {
        cat("> All quant variable(s) are present", fill = T)
      }
    } else {
      cat("> No quantitative field for the test", fill = T)
    }
    #  Qualitative variable(s)
    if (!is.null(qual_vars)) {
      if (F %in% (qual_vars %in% names(uploaded_file))) {
        cat("> Missing qualitative variable(s) for the assay", fill = T)
      } else {
        cat("> All qualitative variable(s) for the test are present", fill = T)
      }
    } else {
      cat("> No qualitative variable(s) for the test name", fill = T)
    }
    cat("> file validation complete", fill = T)
    
    # Pull the appropriate fields from the file
    required_fields = c(identifier, qual_vars, quant_vars)
    results = uploaded_file %>% 
      select(all_of(required_fields)) %>% 
      na.omit() 
    
    # Quality check on qualitative variables if enumeration is specified
    if (!is.null(qual_vars)) {
      cat(paste0("\nQC check on ",length(qual_vars)," qualitative variable(s)"), fill = T)
      for(i in 1:length(qual_vars)) {
        cat(paste0("> ",qual_vars[i]),fill = T)
        enums = qual_vars_list[[qual_vars[i]]]
        if (!is.null(enums)) {
          cat(">   Field has enums set: ")
          cat(enums, sep = ", ", fill = T)
          entries = unique(results[,qual_vars[i]])
          if (all(enums %in% unique(uploaded_file[,qual_vars[i]]))) {
            cat(paste0(">   ",qual_vars[i]," values are all within the set enums"), fill = T)
          } else {
            cat(paste0(">   Not all ",qual_vars[i]," values are within the set enums"), fill = T)
          }
        } else {
          cat(paste0(">   ",qual_vars[i]," has no enums set"), fill = T)
        }
      }
      # Reformat the file
      cat("\nReformatting the qualitative variables..", fill = T)
      qual_results = results %>%
        pivot_longer(cols = qual_vars, names_to = "Parameter Name", 
                     values_to = "Qualitative Result") %>%
        mutate(`Test Name` = input$test_name,
               `Quantitative Result` = "") %>%
        select(all_of(export_fnames))
    } else {
      cat("\nSkipping qualitative variable check..", fill = T)
    }
    
    # Quantitative Parameters
    if (!is.null(quant_vars)) {
      cat("\nReformatting the quantitative variables..", fill = T)
      quant_results = results %>%
        pivot_longer(cols = quant_vars, names_to = "Parameter Name", 
                     values_to = "Quantitative Result") %>%
        mutate(`Test Name` = input$test_name,
               `Qualitative Result` = "") %>%
        select(all_of(export_fnames))
    } else {
      cat("\nSkipping quantitative variable check..", fill = T)
    }
    
    cat("\nPreparing file for download..", fill = T)
    bound = rbind(qual_results, quant_results) %>% arrange(`Unique Test Order ID`)
    import_file = as.data.frame(bound)
    cat("> Preparation complete \n", fill = T)
    return(import_file)
  })
  output$contents = renderTable(fw_import_file())
  
  # Output for Download ----
  output$download_table = downloadHandler(
    filename = paste0(tolower(gsub(" ","_",input$test_name)),"_",format(Sys.Date(), "%Y%m%d"),".csv"),
    content = function(filename) {write.csv(fw_import_file(), filename, row.names = F)}
  )
}
