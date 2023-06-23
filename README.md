# Shiny-based Test Result Reformatting App

## Designed to complement the implementation of Freezerworks' testing module

[Freezerworks](https://freezerworks.com) is a Laboratory Information Management System (LIMS) designed by Dataworks Development Inc. and is a great tool for managing bioarchives which involves sample storage in (often times) many freezers.

Apart from sample management, Freezerworks also has a testing module which has the capability of managing test results in the software - an invaluable tool for certain organizations. The process of importing and exporting the results, however, are not as straightforward and large datasets would require some technical expertise to perform.

The aim for this Shiny-based web app is to invalidate the skill floor required to perform reformatting of import and export test results files from Freezerworks and to streamline the process. The web app additionally can perform data quality control checks using a human-friendly reference [.yaml](https://yaml.org) file; a feature Freezerworks does not currently have.

To reformat an import file, all the user will have to do is upload the .csv file containing test results and choose the assay from a list specified in the reference file. To reformat an export, simply upload the export and specify which Freezerworks-specific aliquot or sample-level identifier to use.

<div>

#### Installation

1.  Clone this repository.
2.  Install the required packages by sourcing [install_packages.R](./install_packages.R).
3.  Build a reference file by either sourcing [reference_builder.R](./reference_builder.R) or by simply editing the reference file to suit your needs. See reference file section for more info.
4.  Deploy!

</div>

## Reference File

The reference file must be labelled as reference.yaml and is in the www/: subdirectory of the app for the web application to function. The file informs the web app on acceptable test names, the variables for those tests, and acceptable values for qualitative variables (if configured) and so it cannot be omitted.

To generate a reference.yaml file, the reference builder script included in the repository could be used. Source reference_builder.R in either R or RStudio and you'll be able to select the reference tables and export a reference.yaml file. Users can also write the .yaml file from scratch or modify a pre-existing one, as long as the reference.yaml file is correctly formatted and is in the correct subdirectory.

It's important to note that the reference builder uses two internal tables exported from the Freezerworks testing module. To access these tables, users would need to have access to the database backend. Contact Freezerworks support if you don't know how to extract the two tables required: parameter and test names table as well as the enumerator table for qualitative parameters.
