# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "tibble",
    "dplyr",
    "readxl",
    "eurostat",
    "tidyr",
    "ggplot2",
    "pxweb",
    "data.table",
    "broom",
    "stringi",
    "plotly",
    "writexl",
    "htmlwidgets",
    "mipfp",
    "config",
    "wid" # github world-inequality-database/wid-r-tool
  )
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

tar_source(
  files = c(
    "R/fingreen-r-utils.R",
    "R/inputs-economy-beta-elasticities.R",
    "R/inputs-economy-consumption-income.R"
  )
)

# This defines the targets and their relationships
list(
  tar_target(
    name = inputs_economy_beta_elasticities,
    command = create_data_inputs_economy_beta_elasticities(),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_consumption_income,
    command = create_data_inputs_economy_consumption_income(),
    format = "file"
  )
)
