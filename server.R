###################################################
# Shiny App Server for FW Test Results Reformatting
# Hasan Sulaeman, 10/05/2020
###################################################

# Server ----
server = function(input, output, session) {
  # Setup ----
  library(dplyr)
  library(tidyr)
  library(shinythemes)
  library(DT)
  
  ## Static List to be updated after upload
  choicelist = list(manifest_sampleid = c("Primary.Sample.ID","DIN"),
                    results_sampleid = c("WSample.Name","Sample.ID","Primary.Sample.ID"),
                    results_quant = c("Concentration","RVP_Neut_NT50","Elecsys CoV2T SCO"),
                    results_qual = c("wSQQText","RVP_Neut_Interpretation"))
  
  # Changing Dropdowns based on file uploaded ----
  ## Setting Reactive Values
  dropdowns = reactiveValues(
    manifest_sampleid = choicelist$manifest_sampleid,
    results_sampleid = choicelist$results_sampleid,
    results_quant = choicelist$results_quant,
    results_qual = choicelist$results_qual
  )
  
  ## Observe Uploads to Update the Dropdowns
  observeEvent(input$manifest,{
    mnfst = read.csv(input$manifest$datapath)
    dropdowns$manifest_sampleid = names(mnfst)
  })
  
  observeEvent(input$results,{
    rslts = read.csv(input$results$datapath)
    dropdowns$results_sampleid = names(rslts)
    dropdowns$results_quant = names(rslts)
    dropdowns$results_qual = names(rslts)
  })
  
  ## Update Dropdowns Based on Uploads
  observe({
    ## manifest_sampleid
    updateSelectInput(session, "manifest_sampleid",
                      label = "Sample ID Field:",
                      choices = dropdowns$manifest_sampleid)
    ## results_sampleid
    updateSelectInput(session, "results_sampleid",
                      label = "Sample ID Field:",
                      choices = dropdowns$results_sampleid)
    updateSelectInput(session, "results_quant",
                      label = "Quantitative Results Field:",
                      choices = dropdowns$results_quant)
    updateSelectInput(session, "results_qual",
                      label = "Qualitative Results Field:",
                      choices = dropdowns$results_qual)
  })
  
  # Output Content ----
  fw_import_file = reactive({
    req(input$manifest)
    req(input$manifest_sampleid)
    req(input$results_sampleid)
    req(input$results_quant)
    req(input$results_qual) 
    req(input$quant_paramname)
    req(input$qual_paramname)
    req(input$test_name)
    tryCatch({
      manifest = read.csv(input$manifest$datapath,
                          header = T,
                          sep = ",")
      results = read.csv(input$results$datapath,
                         header = T,
                         sep = ",")
    },
    error = function(e) {
      stop(safeError(e))
    })
    # Results Reformatting
    ## Trimming down the columns to what's necessary
    manifest = select(manifest, "Unique.Test.Order.ID", input$manifest_sampleid)
    names(manifest)[1] = "Unique Test Order ID"
    results = select(results, input$results_sampleid, input$results_quant, input$results_qual)
    ## Reformatting the results file uploaded
    names(results)[1:3] = c(input$manifest_sampleid,input$quant_paramname,input$qual_paramname)
    results[,input$manifest_sampleid] = toupper(results[,input$manifest_sampleid])
    results$`Test Name` = input$test_name
    import_file = inner_join(manifest, results, by = input$manifest_sampleid)
    import_file = select(import_file, -input$manifest_sampleid)
    ## Joining the two files together
    import_file = tidyr::gather(import_file, "Parameter Name", "Result",  2:3)
    import_file_quant = subset(import_file, import_file$`Parameter Name` == input$quant_paramname)
    import_file_qual = subset(import_file, import_file$`Parameter Name` == input$qual_paramname)
    ## Quant First
    import_file_quant$`Qualitative Result` = ""
    names(import_file_quant)[4] = "Quantitative Result"
    ## Qual Next
    import_file_qual$`Quantitative Result` = ""
    names(import_file_qual)[4] = "Qualitative Result"
    import_file_qual = select(import_file_qual, names(import_file_quant))
    ## Binding
    import_file = rbind(import_file_qual, import_file_quant)
    import_file = as.data.frame(import_file)
    return(import_file)
  })
  output$contents = renderTable(fw_import_file())
  
  # Output for Download
  output$downloadData = downloadHandler(
    filename = paste(input$test_name," Results FW Import File ",Sys.Date(),".csv", sep = ""),
    content = function(filename) {write.csv(fw_import_file(),filename,row.names = F)}
  )
}
