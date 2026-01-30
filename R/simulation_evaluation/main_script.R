# Loading of functions
source(here::here("R/MutaPhy/functions.R"))
source(here::here("R/simulation_evaluation/functions.R"))

# Simulations parameters
param_combinations <- yaml::read_yaml(here::here("R/simulation_evaluation/params.yml"))


# Parameters of simulation
for (params in param_combinations) {

  # for each n_tips
  for (n_tip in params$n_tips) {
    params$n_tips <- n_tip

    output_dir <- here::here(params$output_dir)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    output_file <- here::here(sprintf("%s/outputs/pipeline_simulation_%dseqs.html",
                                      params$output_dir, params$n_tips))
    if (!dir.exists(dirname(output_file))) {
      dir.create(dirname(output_file), recursive = TRUE)
    }

    # Tree + subtrees
    source(here::here("R/simulation_evaluation/pipeline_simulation_trees.R"))

    # Site
    source(here::here("R/simulation_evaluation/pipeline_sites.R"))

    cat(sprintf("Simulation end for : %s and %d tips\n", params$output_dir, params$n_tips))
  }
}

