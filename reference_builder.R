###########################################################
# Reference File Builder for Freezerworks Reformatting App
# v1.1
###########################################################

# Dependencies ----
require(librarian)
librarian::shelf(yaml, readxl, svDialogs, dplyr, tidyr)

# Import the Freezerworks test table ----
# Ensuring the user has access to the test configuration export
msg = svDialogs::dlg_message(
  message = "Do you have a Freezerworks export with the following parameters?
  
  TESTNAME, INACTIVE, PARAMETERNAME, QUALITATIVE",
  type = "yesno"
)

# If yes, proceed
if (msg$res == "yes") {
  path_q = file.choose()
  sheet_q = svDialogs::dlg_input(message = "Which sheet should be used for test names?",
                                 default = 1)
  ref_table = readxl::read_xlsx(path = path_q, sheet = as.integer(sheet_q$res))
} else {
  svDialogs::dlg_message(
    message = "The script cannot run without the export",
    type = "ok"
  )
  stop("No export provided")
}

# Import the Freezerworks test qualitative parameters table ----
# Ensuring the user has access to the test configuration export
msg = svDialogs::dlg_message(
  message = "Do you have a Freezerworks export with the following parameters?
  
  TESTNAME, INACTIVE, PARAMETERNAME, QUALRESULT",
  type = "yesno"
  )

# If yes, proceed
if (msg$res == "yes") {
  path_q = file.choose()
  sheet_q = svDialogs::dlg_input(message = "Which sheet should be used for qualitative variables?",
                                 default = 1)
  excel = readxl::read_xlsx(path = path_q, sheet = as.integer(sheet_q$res))
} else {
  svDialogs::dlg_message(
    message = "The script cannot run without the export",
    type = "ok"
  )
  stop("No export provided")
}

# Script ----
# Pulling the relevant fields from the tables
ref_table %>% 
  select(test_name = TESTNAME,
         inactive = INACTIVE,
         parameter = PARAMETERNAME,
         qualitative = QUALITATIVE) %>%
  filter(inactive == 0) -> ref_table

excel %>%
  select(test_name = TESTNAME, 
         inactive = INACTIVE, 
         parameter_name = PARAMETERNAME, 
         enum = QUALRESULT) %>%
  filter(test_name %in% ref_table$test_name) -> qual_entries

# Building the list
test_names = unique(ref_table$test_name)
ref2 = vector(mode = "list", length = length(test_names))
names(ref2) = test_names
for (i in 1:length(ref2)) {
  # Make quantitative and qualitative lists
  ref2[[i]] = vector(mode = "list", length = 2)
  names(ref2[[i]]) = c("quantitative", "qualitative")
  # Handle the quant variables
  quantfs = ref_table$parameter[ref_table$test_name == names(ref2[i]) & ref_table$qualitative == 0]
  ref2[[i]]["quantitative"] = list(quantfs)
  # Handle the qual variables
  qualfs = ref_table$parameter[ref_table$test_name == names(ref2[i]) & ref_table$qualitative == 1]
  if (length(qualfs) > 0) {
    ref2[[i]][["qualitative"]] = vector(mode = "list", length = length(qualfs))
    names(ref2[[i]][["qualitative"]]) = qualfs
    for (j in 1:length(qualfs)) {
      enums = unique(qual_entries$enum[qual_entries$parameter_name == qualfs[j]])
      if (length(enums) > 0) {
        ref2[[i]][["qualitative"]][[j]] = vector(mode = "list", length = length(enums))
        ref2[[i]][["qualitative"]][[j]] = enums
      } else {
        ref2[[i]][["qualitative"]][[j]] = NULL
      }
    }
  }
}

# Reference file export ----
#  Specify the directory and filename of the .yaml output
save_as = svDialogs::dlg_save(default = "reference.yaml", title = "Save reference file to")
yaml::write_yaml(ref2, file = save_as$res)