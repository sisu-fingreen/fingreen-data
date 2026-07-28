# fingreen-data
Data pipelines &amp; analyses for the FINGREEN macroeconomic model

## Basics

The repo should be self-contained - meaning everything needed to create a dataset should be included in the repo.

The country and year for which data should be created can be set in `global-params.yml`. Use the two-letter country code used by eurostat.

Changing to another country than Finland (FI) will result in an error from those few pipelines where we rely on Statistics Finland. In `wage-distribution` and `inputs-economy-labour-wages` we also rely on statistics calculated from Finnish microdata. Similar statistics could be calculated for other countries from national microdata sources.

## Data pipelines

The datasets correspond to items in the input excel-sheets for the FINGREEN model. All datasets are created using pipelines with the [`targets` -package](https://books.ropensci.org/targets/). This ensures the code can be split into pieces with a clear dependency structure. This way we can create datasets once and then build on them and reuse them easily. The pipelines are defined in the `_targets file`, all datasets are saved as `.ods` files and the file paths are passed between the functions.

### Raw data

All raw data pulled from Eurostat or other sources should be saved separately. This ensures that analyses are replicable, and if the data in the sources change, we can compare to learn what happened.

This is achieved by pulling the raw data with dedicated `pull-raw-*` -functions.

### Datasets used by fingreen

The `create` -functions create the final datasets that are the material for the input excel sheets of the model. The results are created in the `results` directory.

The `pull-raw` -functions can be stored in the same file as the `create` -functions, or separately, if shared between multiple `create` -functions.

### Other content

Additionally there are some `source-data` -files, most importantly mappings between different industry categorizations. There are slight differences for nace even within Eurostat, so there are multiple mapping tables. One should be able to change the mappings and create data for a different categorization, although one should carefully review the results.

The `wage-distribution` -function also creates one dataset to `results/intermediate`, which is then used later in the pipeline.

## Naming conventions

Use only lowcase and `-` as a delimiter in all file names. Exception is source data files, that may be named similarly as in the original source.

R files: use structure `R/{excel-file}-{excel-sheet}.R`, so eg. a function producing the beta elasticities used in the FINGREEN InputsEconomy excel on the beta_elasticities sheet should be named `R/inputs-economy-beta-elasticities.R`. 

Graphs: use the `graphs` subfolder and then the corresponding FINGREEN excel file and excel sheet as subfolder names for any graph results produced by your scripts. Example: `graphs/inputs-economy/beta-elasticities/my-informative-plot.jpeg`.

Result files like `.ods`: use the `results` subfolder and again the excel file and sheet as subfolder names for results producted by your scripts. Example: `results/inputs-economy/beta-elasticities/my-results.ods`

Source data files: use the `source-data` subfolder and then the corresponding FINGREEN excel file and excel sheet as subfolder names for any source data needed by your scripts. Example: `source-data/inputs-economy/beta-elasticities/beta-elasticities-italy.csv`. Mapping tables should go under `source-data/mappings`.

Both graphs and results are excluded from the repo in the .gitignore for now. Source data files are also excluded by default, add manually with `git add` if tracking is needed. In most cases source data files should be added.

## Usage

Make sure you have git set up. Clone the repository to a desired location on your machine. If you use git on the command line: `git clone git@github.com:sisu-fingreen/fingreen-data`

Open the project/repository folder in RStudio, Positron or other IDE of your choice. If using RStudio, it is best to create a project in the folder, or use the RStudio git integration when cloning the project. If using another IDE than Positron or RStudio, make sure that it sets working directory paths automatically to the project root.

Make sure that the R-packages listed in `R/fingreen-r-utils.R` in `check_required_package_installs` are installed.

Set your desired `base_year` (year for most of the data) and `geo` (country) parameters.

Then run
```{R}
library(targets)
tar_make()
```

This will run all the pipelines. Expect errors from some pipelines if you changed the `geo` parameter. Succesfull pipelines will still complete their datasets. Failed pipelines require additional development for other countries than Finland.

Refer to the [targets`-manual](https://books.ropensci.org/targets/) for advanced usage, debugging help, etc.

## Licences
`/source-data/euklems/18II*`: The Conference Board, 2023, "EU KLEMS July 2018 Release", https://doi.org/10.34894/6GDD7Q, DataverseNL, V1. Licenced under [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0)
