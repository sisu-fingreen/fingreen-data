# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
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
    "R/get-number-of-households.R",
    "R/pull-raw-data-inputs-economy-consumption.R",
    "R/inputs-economy-beta-elasticities.R",
    "R/inputs-economy-consumption-coicop-nace-bridge-ras.R",
    "R/inputs-economy-consumption-income.R",
    "R/inputs-economy-consumption-shares.R",
    "R/inputs-economy-demography-skills.R",
    "R/inputs-economy-finance-wealth.R",
    "R/inputs-economy-government-investment.R",
    "R/inputs-economy-investments-depreciation.R",
    "R/inputs-economy-labour-demographics.R",
    "R/inputs-economy-labour-hours.R",
    "R/wage-distribution.R",
    "R/inputs-economy-labour-wages.R",
    "R/pull-raw-data-inputs-economy-inputoutput.R",
    "R/inputs-economy-finaldemand-npish.R",
    "R/inputs-economy-labour-unemployment.R"
  )
)

# This defines the targets and their relationships
list(
  tar_target(
    name = global_params,
    command = config::get(file = "global-params.yml")
  ),
  tar_target(
    name = raw_data_n_households,
    command = pull_raw_data_n_households(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = n_households,
    command = get_number_of_households(raw_data_n_households)
  ),
  tar_target(
    name = raw_data_inputs_economy_beta_elasticities,
    command = pull_raw_data_inputs_economy_beta_elasticities(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_beta_elasticities,
    command = create_inputs_economy_beta_elasticities(
      raw_data_inputs_economy_beta_elasticities,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_consumption,
    command = pull_raw_data_inputs_economy_consumption(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_consumption_coicop_nace_bridge,
    command = create_inputs_economy_consumption_coicop_nace_bridge(
      raw_data_inputs_economy_consumption,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_consumption_income,
    command = create_inputs_economy_consumption_income(
      raw_data_path = raw_data_inputs_economy_consumption,
      n_households = n_households,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_consumption_shares,
    command = create_inputs_economy_consumption_shares(
      raw_data_path = raw_data_inputs_economy_consumption,
      n_households = n_households,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_demography,
    command = pull_raw_data_inputs_economy_demography(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_demography_skills,
    command = create_inputs_economy_demography_skills(
      raw_data_path = raw_data_inputs_economy_demography,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_finance,
    command = pull_raw_data_inputs_economy_finance(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_finance_wealth,
    command = create_inputs_economy_finance_wealth(
      raw_data_path = raw_data_inputs_economy_finance,
      global_params = global_params
    )
  ),
  tar_target(
    name = raw_data_inputs_economy_government_investment,
    command = pull_raw_data_inputs_economy_government_investment(
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_government_investment,
    command = create_inputs_economy_government_investment(
      raw_data_path = raw_data_inputs_economy_government_investment,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_investments_depreciation,
    command = create_inputs_economy_investments_depreciation(
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_labour_demographics,
    command = pull_raw_data_inputs_economy_labour_demographics(
      global_params = global_params,
      imputation_year = 2020L
    ),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_labour_demographics,
    command = create_inputs_economy_labour_demographics(
      raw_data_path = raw_data_inputs_economy_labour_demographics,
      global_params = global_params,
      imputation_year = 2020L # needs to match that of the pull_raw_data call above
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_labour_hours,
    command = pull_raw_data_inputs_economy_labour_hours(global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_labour_hours,
    command = create_inputs_economy_labour_hours(
      raw_data_path = raw_data_inputs_economy_labour_hours,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = wage_distribution,
    command = create_wage_distribution(
      wage_microdata_path = "source-data/microdata/wages-by-Skill-adjusted-2010_2022-(KUN-VAL-YKS).xlsx",
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_labour_unemployment,
    command = pull_raw_data_inputs_economy_labour_unemployment(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_labour_unemployment,
    command = create_inputs_economy_labour_unemployment(
      raw_data_path = raw_data_inputs_economy_labour_unemployment,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_labour_wages,
    command = pull_raw_data_inputs_economy_labour_wages(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_labour_wages,
    command = create_inputs_economy_labour_wages(
      raw_data_path = raw_data_inputs_economy_labour_wages,
      raw_data_labour_hours_path = raw_data_inputs_economy_labour_hours,
      labour_demographics_path = inputs_economy_labour_demographics,
      labour_unemployment_path = inputs_economy_labour_unemployment,
      wage_distribution_data_path = wage_distribution,
      global_params = global_params
    ),
    format = "file"
  ),
  tar_target(
    name = raw_data_inputs_economy_inputoutput,
    command = pull_raw_data_inputs_economy_inputouput(global_params = global_params),
    format = "file"
  ),
  tar_target(
    name = inputs_economy_finaldemand_npish,
    command = create_inputs_economy_finaldemand_npish(
      global_params = global_params,
      raw_data_path = raw_data_inputs_economy_inputoutput
    ),
    format = "file"
  )
)
