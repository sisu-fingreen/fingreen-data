# fingreen-data
Data pipelines &amp; analyses for the FINGREEN macroeconomic model

## Basics

The repo should be self-contained - meaning everything needed to run any data creation script should be included in the repo.

## Naming conventions

Use only lowcase and `-` as a delimiter in all file names.

Source data files: use the `source-data` subfolder and then the corresponding FINGREEN excel file and excel sheet as subfolder names for any source data needed by your scripts. Example: `source-data/inputs-economy/beta-elasticities/beta-elasticities-italy.csv`.

R files: use structure `{excel-file}-{excel-sheet}.R`, so eg. R script producing the beta elasticities used in the FINGREEN InputsEconomy excel on the beta_elasticities sheet should be named `inputs-economy-beta-elasticities.R`.

Graphs: use the `graphs` subfolder and then the corresponding FINGREEN excel file and excel sheet as subfolder names for any graph results produced by your scripts. Example: `graphs/inputs-economy/beta-elasticities/my-informative-plot.jpeg`.

Result files like `.xlsx`: use the `results` subfolder and again the excel file and sheet as subfolder names for results producted by your scripts. Example: `results/inputs-economy/beta-elasticities/my-results.xlsx`

Both graphs and results are excluded from the repo in the .gitignore for now.
