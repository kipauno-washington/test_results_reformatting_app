# Shiny-based Test Result Reformatting App

### Designed to complement the implementation of Freezerworks' testing module

[Freezerworks](https://freezerworks.com) is a Laboratory Information Management System (LIMS) designed by Dataworks Development Inc. and is a great tool for managing bioarchives which involves sample storage in (often times) many freezers.

Apart from sample management, Freezerworks also has a testing module which has the capability of managing test results in the software - an invaluable tool for certain organizations. The process of importing and exporting the results, however, are not as straightforward and large datasets would require some technical expertise to perform.

The aim for this Shiny-based web app is to invalidate the skill floor required to perform reformatting of import and export test results files from Freezerworks and to streamline the process. The web app additionally can perform data quality control checks using a human-friendly reference [.yaml](https://yaml.org) file; a feature Freezerworks does not currently have.

To reformat an import file, all the user will have to do is upload the .csv file containing test results and choose the assay from a list specified in the reference file. To reformat an export, simply upload the export and specify which Freezerworks-specific aliquot or sample-level identifier to use.

#### Installation

1.  Clone this repository
2.  Build the reference file using either the [reference builder](./reference_builder.R) script and an export from your Freezerworks instance or by simply editing the reference file to suit your needs
3.  Deploy!
