R: 4.0.2 for packages that were used during benchmarks see renv.lock
dir.create("/tmp/experiments")
.libPaths("/tmp/experiments")
install.packages("renv")
renv::restore()
install.packages("remotes")
remotes::install_local("../../bbotk", upgrade = "never")  # skip updates
remotes::install_local("../../mlr3mbo", upgrade = "never")  # skip updates

python: 3.7.10 for modules that were used during benchmarks see requirements.txt
note python must be compipled with the CONFIGURE_OPTS=--enable-shared flag

CONFIGURE_OPTS=--enable-shared pyenv install 3.7.10
pipenv --python 3.7.10
pipenv shell
pip install -r naszilla/requirements.txt
cd naszilla
pip install -e.

download benchmark datasets and put them into nas_benchmark_datasets
nas-bench-101: nasbench_full.tfrecord and nasbench_only108.tfrecord
nas-bench-201: NAS-Bench-201-v1_1-096897.pth

change paths in optimizers.R to point to your newly created virtualenv

naszialla apache v2 show what changed

unzip registry_qdo_nas_large_clean.zip

in doubt change line 15 in naszilla/naszilla/nas_benchmarks.py to the correct absolute path
so that this can be reached from outside of nasizlla

