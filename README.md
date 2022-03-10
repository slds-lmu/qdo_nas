In this repository we release all code to replicate all results, tables and figures presented in the paper:
Tackling Neural Architecture Search With Quality Diversity Optimization

The repository is structured as follows:
  * `bbotk/`, `mlr3mbo/` and `naszilla/` are directories of R packages / python modules that are used
  * `model_compression/` contains all code regarding the application of applying qdNAS to model compression
      * please see the README there for more information
  * `results/` contains raw benchmark results saved as `.rds` files
  * `plots/` contains all plots presented in the paper
  * `run.R` and `run_ablation.R` are the main files used for the benchmark experiments and the ablation study
  * `analyze.R` and `analyze_ablation.R` contain code needed to analyze the results of the benchmark experiments and
    ablation study and create plots and tables
    * note that these files require additional packages not mentioned in the `renv.lock` files (because analyses were not
      done on the clusters); simply install them manually to analyze the results (for those packages we indicate versions via "#..." but those do not matter much)
  * `LearnerRegrBananasNN.R`, `helpers.R`, `niches.R`, `niches_overlapping.R`, `nn_ensemble.py`, `optimizers.R` and `scenarios.R` are
    helper files that contain code needed by the main files
  * `pareto_plot.R` is the code for generating Figure 1
  * `registry_qdo_nas_large_clean.zip` and `registry_qdo_nas_ablation_clean.zip` are the original batchtools
    registries as used on the clusters (stripped of the individual results which are collected in `results/` and stripped
    of the logs to save some space); see `run.R` or `run_ablation.R` for an example on how to inspect these registries
    after unzipping them; these registries also store the random seeds
  * `requirements/` contains an `experiments/` and `model_compression/` folder each with an `renv.lock` and `requirements.txt`
    * the `renv.lock` files list the exact R packages that were used on the cluster and analogously the `requirements.txt`
      files list the exact python modules that were used on the cluster

To obtain more information about the optimizers in general, please see:
  * `mlr3mbo/R/OptimizerBOHBQDO.R` for BOP-ElitesHB
  * `mlr3mbo/R/OptimizerHyperbandQDO.R` for qdHB
  * `mlr3mbo/R/bayesopt_bop.R` for BOP-Elites\*
  * `mlr3mbo/R/OptimizerBOHBMO.R` for ParEGOHB
  * `mlr3mbo/R/OptimizerHyperbandMO.R` for moHB\*
  * `mlr3mbo/R/bayesopt_parego.R for` ParEGO\*

In the benchmark experiments and the ablation study, R 4.0.2 and python 3.7.10 were used.

To replicate our results, install R and python (note that python must be compiled with the `CONFIGURE_OPTS=--enable-shared` flag due to being used within R via reticulate).

We assume that you work in a terminal and are in the current directory (`qdo_nas`).

Then do the following within R (we assume that you managed to install R 4.0.2 or use a docker environment):

```r
install.packages("renv")  # 0.14.0
install.packages("remotes")  # 2.4.2
remotes::install_github("mlr-org/mlr3tuning", ref = "tunermbo")
remotes::install_local("bbotk", upgrade = "never")
renv::restore(lockfile = "requirements/experiments/renv.lock")
remotes::install_local("mlr3mbo", upgrade = "never")
```

Close the R session, followed by setting up python and a virtualenv (we use pyenv and pipenv here):

```bash
CONFIGURE_OPTS=--enable-shared pyenv install 3.7.10
pipenv --python 3.7.10
pipenv shell
pip install -r naszilla/requirements.txt
cd naszilla
pip install -e.
```

Then download the following files:
* [nasbench\_only108.tfrecord](https://storage.googleapis.com/nasbench/nasbench_only108.tfrecord)
* [NAS-Bench-201-v1\_1-096897.pth](https://drive.google.com/file/d/16Y0UwGisiouVRxW-W5hEtbxmcHw_0hF_/view)

and put them in `naszilla/nas_benchmark_datasets/`.

Finally, if you want to rerun experiments, make sure to adapt some paths to point at your newly created virtualenv, i.e., in `optimizers.R` and `LearnerRegrBananasNN.R`.
We indicate this via `"FIXME_python_path_of_your_virtualenv"` as a placeholder.

