###################################################
# Shiny App Server for FW Test Results Reformatting
# Hasan Sulaeman, v2.1; 06/06/2023
###################################################

# Single upload only. No limitation for if the headers are not on the first row
# No test order id = not reformatted
# Need to accommodate assays with either no quant or no qual parameters

server = function(input, output, session) {
  # Required packages ----
  require(dplyr)
  require(tidyr)
  require(DT)
  require(yaml)
  require(glue)
  require(shinythemes)

  # Load References ----
  cat("\nInitializing Freezerworks Reformatting Web App..", fill = T)
  ref = read_yaml("www:/reference.yaml")
  cat("> Reference file was successfully loaded in", fill = T)
  # Serving the list of accepted test_names
  updateSelectInput(session = session, "test_name", choices = names(ref), selected = "")
  cat("> Web app ready", fill = T)
  identifier = "Unique Test Order ID"
  export_fnames = c(identifier, "Test Name", "Parameter Name",
                    "Qualitative Result", "Quantitative Result")

  # Import file reformatting ----
  fw_import_file = reactive({
    # Inputs required ----
    req(input$test_name)
    req(input$file_upload)

    # Pulling in variables from reference file ----
    if (input$test_name != "test_names") {
      cat(glue("\n\nTest name selected: {input$test_name}\n\n"), fill = T)
      test_ref = ref[[input$test_name]]
      quant_vars = test_ref[["quantitative"]]
      qual_vars = names(test_ref[["qualitative"]])
      if (!is.null(qual_vars)) {
        qual_vars_list = test_ref[["qualitative"]]
      }
      add_date = test_ref[["add_date"]]
      # Check for whether add_date is logical
      validate(
        need(class(add_date) == "logical",
             "Test date configuration error. Check with an administrator.")
      )
      # Set export field names for if test date is supposed to be added
      if (add_date == T) {
        export_fnames = c("Test Name", identifier, "Test Date", "Parameter Name",
                          "Qualitative Result", "Quantitative Result")
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
    # In case test date is needed
    if (add_date == T) {
      required_fields = append(required_fields, "Test Date")
    }
    for (field in required_fields) {
      which_name = names(uploaded_file)[grep(field, names(uploaded_file), ignore.case = T)]
      which_name_nchar = which_name[nchar(which_name) == nchar(field)]
      names(uploaded_file)[names(uploaded_file) == which_name_nchar] = field
    }
    cat("> Field names are standardized\n\nInitiating validation of field names:", fill = T)

    # Saving the final field names for validation
    fnames = names(uploaded_file)

    # Validation ----
    #  Identifier ----
    cat("> Validating identifier field..", fill = T)
    validate(
      need(identifier %in% fnames, "Unique Test Order ID not found in the file")
    )
    #  Removing whitespaces in UTID
    uploaded_file[identifier] = trimws(uploaded_file[,identifier], which = "both")
    #  Remove trailing records without UTIDs
    uploaded_file[,identifier][identifier == ""] = NA
    uploaded_file = subset(uploaded_file, !is.na(uploaded_file[,identifier]))

    #  Quantitative variable(s) ----
    if (!is.null(quant_vars)) {
      cat("> Validating quantitative variables..", fill = T)
      validate(
        need(all(quant_vars %in% fnames),
             glue("Missing quantitative variable field: {quant_vars[quant_vars %in% fnames == F]}"))
      )
      for (variable in quant_vars) {
        class(uploaded_file[,variable]) = "numeric"
      }
    }

    #  Qualitative variable(s) ----
    if (!is.null(qual_vars)) {
      cat("> Validating qualitative variable names..", fill = T)
      validate(
        need(all(qual_vars %in% fnames),
             glue("Missing qualitative variable fields: {qual_vars[qual_vars %in% fnames == F]}"))
      )
      # Checking for enumerator(s)
      for(variable in qual_vars) {
        # Trimming whitespace(s) in the qualitative variable
        uploaded_file[variable] = trimws(uploaded_file[,variable], which = "both")
        cat("> Validating qualitative variable entries..", fill = T)
        enums = qual_vars_list[[variable]]
        if (!is.null(enums)) {
          # Standardizing qualitative variable entry(ies)
          for (enum in enums) {
            which_enum = which(grepl(enum, uploaded_file[,variable], ignore.case = T) &
                                 nchar(uploaded_file[,variable]) == nchar(enum))
            uploaded_file[which_enum, variable] = enum
          }
          # Validate qualitative variables
          entries = unique(uploaded_file[,variable])
          validate(
            need(all(entries %in% enums), glue("Values for {variable} are incorrect"))
          )
        }
      }
    }

    #  Test date ----
    cat("> Validating test date..", fill = T)
    if (add_date == T) {
      # Check if test date is included in the uploaded file
      validate(
        need("Test Date" %in% fnames, "'Test Date' field not found")
      )
      # Reformat the dates
      uploaded_file[,"Test Date"] = as.Date(uploaded_file[,"Test Date"],
                                            tryFormats = c("%m/%d/%Y","%Y-%m-%d","%m/%d/%y"))
      uploaded_file[,"Test Date"] = format(uploaded_file[,"Test Date"], format = "%m/%d/%Y")
      # See if reformatting was successful
      validate(
        need(all(!is.na(uploaded_file[,"Test Date"])),
             "Date format uploaded is incorrect")
        )
    }

    #  Admin-set validation, if applicable ----
    if ("validation" %in% names(test_ref)) {
      extra_qc = test_ref[["validation"]]

      # Look for set quantitative variable check(s) ----
      if ("quant_check" %in% names(extra_qc)) {
        quant_qc = extra_qc[["quant_check"]]
        quant_params = names(quant_qc)
        validate(
          need(all(quant_params %in% quant_vars),
               message = glue("Validation setting error: {quant_params} is not recognized for the asssay"))
        )
        # Go through each quantitative parameter
        for (param in quant_params) {
          # Pull the qc setting(s) for the parameter
          qc_ck = quant_qc[[param]]
          # Check for minimum value setting
          if ("min" %in% names(qc_ck)) {
            validate(all(as.numeric(uploaded_file[,param]) <= as.numeric(qc_ck[["min"]])),
                     message = glue("{uploaded_file[,param]} is below the set minimum value for {param}"))
          }
          # Check for maximum value setting
          if ("max" %in% names(qc_ck)) {
            validate(all(as.numeric(uploaded_file[,param]) >= as.numeric(qc_ck[["max"]])),
                     message = glue("{uploaded_file[,param]} is above the set maximum value for {param}"))
          }
        }
      }

      # Look for set qualitative variable check(s) ----
      if ("qual_check" %in% names(extra_qc)) {
        qual_qc = extra_qc[["quant_check"]]
        qual_params = names(qual_qc)
        validate(
          need(all(qual_params %in% qual_vars),
               message = glue("Validation setting error: {qual_params} is not recognized for the assay"))
        )
        # Go through each qualitative parameter
        for (param in qual_params) {
          # Pull the qc setting(s) for the parameter
          qc_ck = qual_qc[[param]]
          cvalue = c("cutoff","value","quant_parameter")
          validate(
            need(all(cvalue %in% names(qc_ck)),
                 message = glue("Validation setting error: {param} is missing {cvalue[cvalue %in% names(qc_ck) == F]}"))
          )
          check_quant = qc_ck[["quant_parameter"]]
          check_value = qc_ck[["value"]]
          check_cutoff = qc_ck[["cutoff"]]
          validate(
            need(all(record[,quant_parameter][record[,param] == check_value] >= check_cutoff),
                 message = "Found mismatching quant and qual values")
          )
        }
      }
    }
    # Reformatting ----
    #  Pull all required fields ----
    results = uploaded_file %>%
      select(all_of(required_fields))

    #  Qualitative variable(s) ----
    cat("> Validation complete\n\nInitiating reformatting:", fill = T)
    if (!is.null(qual_vars)) {
      if (add_date == T) {
        cat("> Reformatting qualitative variables..", fill = T)
        qual_results = results %>%
          pivot_longer(cols = qual_vars, names_to = "Parameter Name",
                       values_to = "Qualitative Result") %>%
          mutate(`Test Name` = input$test_name,
                 `Quantitative Result` = "") %>%
          select(`Unique Test Order ID`, `Test Name`, `Test Date`, `Parameter Name`,
                 `Qualitative Result`, `Quantitative Result`)
      } else {
        cat("> Reformatting qualitative variables..", fill = T)
        qual_results = results %>%
          pivot_longer(cols = qual_vars, names_to = "Parameter Name",
                       values_to = "Qualitative Result") %>%
          mutate(`Test Name` = input$test_name,
                 `Quantitative Result` = "") %>%
          select(`Unique Test Order ID`, `Test Name`, `Parameter Name`,
                 `Qualitative Result`, `Quantitative Result`)
      }
    }

    #  Quantitative variable(s) ----
    if (!is.null(quant_vars)) {
      if (add_date == T) {
        cat("> Reformatting quantitative variables..", fill = T)
        quant_results = results %>%
          pivot_longer(cols = quant_vars,
                       names_to = "Parameter Name",
                       values_to = "Quantitative Result") %>%
          mutate(`Test Name` = input$test_name,
                 `Qualitative Result` = "") %>%
          filter(!is.na(`Quantitative Result`)) %>%
          select(`Unique Test Order ID`, `Test Name`, `Test Date`, `Parameter Name`,
                 `Qualitative Result`, `Quantitative Result`)
      } else {
        cat("> Reformatting quantitative variables..", fill = T)
        quant_results = results %>%
          pivot_longer(cols = quant_vars,
                       names_to = "Parameter Name",
                       values_to = "Quantitative Result") %>%
          mutate(`Test Name` = input$test_name,
                 `Qualitative Result` = "") %>%
          filter(!is.na(`Quantitative Result`)) %>%
          select(`Unique Test Order ID`, `Test Name`, `Parameter Name`,
                 `Qualitative Result`, `Quantitative Result`)
      }
    }

    #  Bind the variables together ----
    if (!is.null(quant_vars) & !is.null(qual_vars)) {
      bound = rbind(qual_results, quant_results) %>% arrange(`Unique Test Order ID`)
    } else {
      if (is.null(qual_vars)) {
        bound = quant_results %>% arrange(`Unique Test Order ID`)
      }
      if (is.null(quant_vars)) {
        bound = qual_results %>% arrange(`Unique Test Order ID`)
      }
    }

    # Filter out blanks ----
    bound$pk = seq(1, nrow(bound))
    blanks = bound %>%
      filter(`Qualitative Result` == "" & `Quantitative Result` == "")
    bound = bound %>%
      filter(pk %in% blanks$pk == F) %>%
      select(-pk)
    import_file = as.data.frame(bound)
    cat("> Reformatting complete\n", fill = T)
    return(import_file)
  })

  # Export file reformatting ----
  reformat_report = reactive({
    # Inputs required ----
    req(input$key_selection_reporting)
    req(input$file_upload_reporting)

    # Static requirements ----
    # Fieldnames in the export file
    reporting_id = input$key_selection_reporting
    req_fnames_rep = c("Test Name", reporting_id, "Test Date", "Parameter Name",
                       "Quantitative Result", "Qualitative Result")

    # Validation of uploaded file ----
    fw_export = read.csv(input$file_upload_reporting$datapath, check.names = F)
    fw_export_fnames = names(fw_export)

    # Check if all the fields are there
    validate(
      need(all(req_fnames_rep %in% names(fw_export)),
           glue("Missing required field: {req_fnames_rep[req_fnames_rep %in% fw_export_fnames  == F]}"))
    )

    # Check if the assay name is in the config file
    export_assay_name = unique(fw_export$`Test Name`)
    validate(
      need(length(export_assay_name) == 1,
           glue("More than one assay name included in the uploaded file: {cat(export_assay_name, sep = ', ')}"))
    )
    # Check test name in config file
    validate(
      need(export_assay_name %in% names(ref),
           glue("{export_assay_name} is not a valid assay. Check with an administator."))
    )

    # Set up the config file
    test_ref_export = ref[[export_assay_name]]
    vars_export = append(test_ref_export[["quantitative"]], names(test_ref_export[["qualitative"]]))
    add_date_export = test_ref_export[["add_date"]]
    # Check for whether add_date is logical
    validate(
      need(class(add_date_export) == "logical",
           "Test date configuration error. Check with an administrator.")
    )
    # Set export field names for if test date is supposed to be added
    if (add_date_export == T) {
      req_fnames_rep = c("Test Name", reporting_id, "Test Date", "Parameter Name",
                         "Quantitative Result", "Qualitative Result")
    }

    # Reformat the export ----
    #  Quantitative variables
    quant_fnames = req_fnames_rep[req_fnames_rep != "Qualitative Result"]
    export_quant = fw_export %>%
      select(all_of(quant_fnames)) %>%
      filter(!is.na(`Quantitative Result`) & `Quantitative Result` != "") %>%
      pivot_wider(id_cols = reporting_id,
                  names_from = `Parameter Name`,
                  values_from = `Quantitative Result`)

    #  Qualitative variables
    qual_fnames = req_fnames_rep[req_fnames_rep != "Quantitative Result"]
    export_qual = fw_export %>%
      select(all_of(qual_fnames)) %>%
      filter(!is.na(`Qualitative Result`) & `Qualitative Result` != "") %>%
      pivot_wider(id_cols = reporting_id,
                  names_from = `Parameter Name`,
                  values_from = `Qualitative Result`)

    #  Join the two tables by the selected identifier
    joined = as.data.frame(full_join(export_quant, export_qual, by = reporting_id))

    # Check if all of the field names are present
    full_vars_export = append(reporting_id, vars_export)
    if (!all(full_vars_export %in% names(joined))) {
      missing_fields_export = full_vars_export[full_vars_export %in% names(joined) == F]
      for (i in 1:length(missing_fields_export)) {
        joined[,missing_fields_export[i]] = NA
      }
    }
    export_df = select(joined, full_vars_export)

    # Return the reformatted file ----
    export_file = export_df
    return(export_file)
  })

  # Table renderings ----
  output$contents_import = renderTable(fw_import_file())
  output$contents_reporting = renderTable(reformat_report())

  # Import tab download handling ----
  # UI
  output$download_import_btn = renderUI({
    if(!is.null(input$test_name) & !is.null(input$file_upload)) {
      downloadButton('download_table_import', 'Download')
    }
  })

  # Handler
  output$download_table_import = downloadHandler(
    filename = function() {
      paste0(tolower(gsub(" ","_",input$test_name)),"_",format(Sys.Date(), "%Y%m%d"),".csv")
    },
    content = function(file) {
      write.csv(fw_import_file(), file, row.names = F, na = "")
    }
  )

  # Export tab download handling ----
  # UI
  output$download_export_btn = renderUI({
    if(!is.null(input$key_selection_reporting) & !is.null(input$file_upload_reporting)) {
      downloadButton('download_table_export', 'Download')
    }
  })

  # Handler
  output$download_table_export = downloadHandler(
    filename = function() {
      paste0("export_", format(Sys.Date(), "%Y%m%d"),".csv")
    },
    content = function(file) {
      write.csv(reformat_report(), file, row.names = F, na = "")
    }
  )
}
