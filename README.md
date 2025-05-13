# fingreen-data
Data pipelines &amp; analyses for the FINGREEN macroeconomic model

## Basics

The repo should be self-contained - meaning everything needed to run any data creation script should be included in the repo.

## Naming conventions

Use only lowcase and `-` as a delimiter in all file names. Exception are source data files, that may be named similarly as in the source.

R files: use structure `{excel-file}-{excel-sheet}.R`, so eg. R script producing the beta elasticities used in the FINGREEN InputsEconomy excel on the beta_elasticities sheet should be named `inputs-economy-beta-elasticities.R`.

Graphs: use the `graphs` subfolder and then the corresponding FINGREEN excel file and excel sheet as subfolder names for any graph results produced by your scripts. Example: `graphs/inputs-economy/beta-elasticities/my-informative-plot.jpeg`.

Result files like `.xlsx`: use the `results` subfolder and again the excel file and sheet as subfolder names for results producted by your scripts. Example: `results/inputs-economy/beta-elasticities/my-results.xlsx`

Queries into eg. StatFin database: use the `queries` subfolder and the same folder and file name structure as specified for graphs and results.

Source data: use the `source-data` subfolder and either a subfolder structure as specified above, or a subfolder (eg. `euklems`) according to the data source. The latter especially for data that may be used in calculations of multiple input sheet data.
