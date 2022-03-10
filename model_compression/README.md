This directory contains all code regarding the application of applying qdNAS to model compression

* `instance.rds` is the result file
* `run.R` is the main file
* `analyze.R` contains code to analyze the results
* `Compressing MobileNetV2 with NNI Pruners.ipynb`, and `Mobilenet_download_pretrain.ipynb` are notebooks from NNI that are either used or adapted in `MobileNetPruning.ipynb`
* `config_space.json`, `helpers.R`, `param_set.R`, `prepare_R.py`, `preprocess.py`, `pretrain.py`, `pruning_experiments.py`, `test.py`, `utils_modules.py`, and `utils.py` are helper files
* `prepare_data.sh` is needed to prepare data, see `Compressing MobileNetV2 with NNI Pruners.ipynb`
* `data/` will contain preprocessed data after being set up, see instructions below
* `models/` contain models
* `pretrained_*/` contain checkpoints of best models


R 4.1.2 and python 3.8.10 were used.

To replicate our results, install R and python (note that python must be compiled with the `CONFIGURE_OPTS=--enable-shared` flag due to being used within R via reticulate).

We assume that you work in a terminal and are in the current directory (`model_compression`)

Then do the following within R (we assume that you managed to install R 4.1.2 or use a docker environment):

```r
install.packages("renv")  # 0.15.2
install.packages("remotes")  # 2.4.2
remotes::install_github("mlr-org/mlr3tuning", ref = "tunermbo")
remotes::install_local("../bbotk", upgrade = "never")
renv::restore(lockfile = "../requirements/model_compression/renv.lock")
remotes::install_local("../mlr3mbo", upgrade = "never")
```

Close the R session, followed by setting up python and a virtualenv (we use pyenv and pipenv here):

```bash
CONFIGURE_OPTS=--enable-shared pyenv install 3.8.10
pipenv --python 3.8.10
pipenv shell
pip install -r ../requirements/model_compression/requirements.txt
```

Note that we rely on `torch==1.10.2+cu113` due to using an NVIDIA DGX A100 instance.

Then, prepare the pruning application:

1. Run `Compressing MobileNetPruning.ipynb` to download data and obtain the pretrained model(s) (only do this up to including the `# Run prepare_data.sh` part, all parts following thereafter are not needed)
2. You should now be able to run `MobileNetPruning.ipynb`

To replicate our results, adapt some paths in `run.R` to point at your newly created virtualenv indicated via `"FIXME_python_path_of_your_virtualenv"` as a placeholder.

