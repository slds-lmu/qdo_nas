This directory contains all code regarding our benchmarks on the MobileNetV3 search space and Once-for-All comparisons:

* `results/` contains raw benchmark results saved as `.rds` or `.csv` files
* `plots/` contains all plots presented in the paper
* `latency_tools/` contains latency look-up tables provided by Once-for-All (they can be redownloaded via `download.py`)
* `run.R` and `run_2F.R` are the main files used for the benchmark experiments on the MobileNetV3 search space using
  accuracy predictors and look-up tables of Once-for-All; `run_2F.R` contains the experiments for the setting of two
  feature functions (latency and size)
* `analyze.R` contains code needed to analyze the results of the benchmark experiments and create plots and tables
    * note that these files require additional packages not mentioned in the `renv.lock` files (because analyses were not
      done on the clusters); simply install them manually to analyze the results (for those packages we indicate versions via "#..." but those do not matter much)
* `helpers_mobilenet.R`, `niches_overlapping.R`, `niches_overlapping_2F.R`, `optimizers.R`, `optimizers_2F.R`, `scenarios.R`, `scenarios_2F.R` and `prepare_mobilenet_R.py` are helper files that contain code needed by the main files
* `qdo_evo.py` is the main file for the comparison of MAP-Elites to regularized evolution within Once-for-All
  * `evolution_finder_custom.py` contains helper code
* `qdo_evo.R` contains code needed to analyze the MAP-Elites and regularized evolution comparison
* `registry_qdo_nas_ofa_clean.zip` and `registry_qdo_nas_ofa_2Fx_clean.zip` are the original batchtools
    registries as used on the clusters (stripped of the individual results which are collected in `results/` and stripped
    of the logs to save some space); see `run.R` or `run_2F.R` for an example on how to inspect these registries
    after unzipping them; these registries also store the random seeds


In the benchmark experiments R 4.0.2 and python 3.7.10 were used.

To replicate our results, install R and python (note that python must be compiled with the `CONFIGURE_OPTS=--enable-shared` flag due to being used within R via reticulate).

We assume that you work in a terminal and are in the current directory (`ofa`).

Then do the following within R (we assume that you managed to install R 4.0.2 or use a docker environment):

```r
install.packages("renv")  # 0.14.0
install.packages("remotes")  # 2.4.2
remotes::install_github("mlr-org/mlr3tuning", ref = "tunermbo")
remotes::install_local("../bbotk", upgrade = "never")
renv::restore(lockfile = "../requirements/ofa/renv.lock")
remotes::install_local("../mlr3mbo", upgrade = "never")
```

Close the R session, followed by setting up python and a virtualenv (we use pyenv and pipenv here):

```bash
CONFIGURE_OPTS=--enable-shared pyenv install 3.7.10
pipenv --python 3.7.10
pipenv shell
pip install -r ../requirements/ofa/requirements.txt
```

To replicate our results, adapt some paths in `optimizers.R` and `optimizers_2F.R` to point at your newly created virtualenv indicated via `"FIXME_python_path_of_your_virtualenv"` as a placeholder.

