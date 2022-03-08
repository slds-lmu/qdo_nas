In this repository we release all code to replicate all results, tables and figures presented in the paper:
Tackling Neural Architecture Search With Quality Diversity Optimization

The repository ist structured as follows:
  * bbotk, mlr3mbo and naszilla are directories of R packages / python modules that are used
  * model\_compression contains all code regarding the application of applying qdNAS to model compression
      * please see the README there for more information
  * results contains raw benchmark results saved as .rds files
  * plots contains all plots presented in the paper
  * run.R and run\_ablation.R are the main files used for the benchmark experiments and the ablation study
  * analyze.R and analyze\_ablation.R contain code needed to analyze the results of the benchmark experiments and
    ablation study and create plots and tables
    * note that these files require additional packges not mentioned in the renv.lock files (because analyses were not
      done on the clusters); simply install them manually to analyze the results
  * LearnerRegrBananasNN.R, helpers.R, niches.R, niches\_overlapping.R, nn\_ensemble.py, optimizers.R and scenarios.R are
    helper files that contain code needed by the main files
  * pareto\_plot.R is the code for generating Figure 1
  * registry\_qdo\_nas\_large\_clean.zip and registry\_qdo\_nas\_ablation\_clean.zip are the original batchtool
    registries as used on the clusters (stripped of the individual results which are collected in results/ and stripped
    of the logs to save some space); see run.R or run\_ablation.R for an example on how to inspect these registries
    after unzipping them (i.e., to obtain random seeds that were used)
  * requirements contains an experiments and model\_compression folder each with an renv.lock and requirements.txt
    * the renv.lock files list the exact R packages that were used on the cluster and analogously the reqyirements.txt
      files list the exact python modules that were used on the cluster

To obtain more information about the optimizers, please see:
  * mlr3mbo/R/OptimizerBOHBQDO.R for BOP-ElitesHB
  * mlr3mbo/R/OptimizerHyperbandQDO.R for qdHB
  * mlr3mbo/R/bayesopt\_bop.R for BOP-Elites\*
  * mlr3mbo/R/OptimizerBOHBMO.R for ParEGOHB
  * mlr3mbo/R/OptimizerHyperbandMO.R for moHB\*
  * mlr3mbo/R/bayesopt\_parego.R for ParEGO\*

In the benchmark experiments and the ablation study, R 4.0.2 and python 3.7.10 were used.

To replicate our results, install R and python (note that python must be compipled with the CONFIGURE\_OPTS=--enable-shared flag due to being used within R via reticulte).

We assume that you work in a terminal and are in the current directory (qdo\_nas)

Then do the following within R (we assume that you managed to install R 4.0.2 or used a docker environment):

```r
install.packages("renv")  # 0.15.4
install.packages("remotes")  # 2.4.2
remotes::install_github("mlr-org/mlr3tuning", ref = "tunermbo")
remotes::install_local("bbotk", upgrade = "never")
renv::restore(lockfile = "requirements/experiments/renv.lock")  # y to proceed
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
* nasbench\_only108.tfrecord (https://storage.googleapis.com/nasbench/nasbench_only108.tfrecord)
* NAS-Bench-201-v1\_1-096897.pth (https://drive.google.com/open?id=16Y0UwGisiouVRxW-W5hEtbxmcHw_0hF_)
and put them in naszilla/nas\_benchmark\_datasets/

Finally, if you want to rerun experiments, make sure to adapt some paths to point at your newly created virtualenv,
i.e., in optimizers.R and LearnerRegrBananasNN.R.
We indicate this via "FIXME\_python\_path\_of\_your\_virtualenv" as a placeholder


FIXME:

test if this works

note in all analyze scripts which versions were used

naszilla apache v2 show what changed

unzip registry_qdo_nas_large_clean.zip etc.

run_ablation.R same last lines like run.R

model_compression
Compressing MobileNetPruning.ipynb
MobileNetPruning.ipynb

pip install nni==2.6
pytorch
tensorboard
ConfigSpace
