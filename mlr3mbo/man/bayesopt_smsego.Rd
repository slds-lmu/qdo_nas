% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesopt_smsego.R
\name{bayesopt_smsego}
\alias{bayesopt_smsego}
\title{Sequential Multicriteria Bayesian Optimization Via SmsEGO.}
\usage{
bayesopt_smsego(
  instance,
  init_design_size = NULL,
  surrogate = NULL,
  acq_function = NULL,
  acq_optimizer = NULL,
  sampler = NULL
)
}
\arguments{
\item{instance}{(\link[bbotk:OptimInstanceMultiCrit]{bbotk::OptimInstanceMultiCrit})\cr
The \link[bbotk:OptimInstanceMultiCrit]{bbotk::OptimInstanceMultiCrit} to be optimized.}

\item{init_design_size}{(\code{NULL} | \code{integer(1)})\cr
Size of the initial design.
If \code{NULL} \code{4 * d} is used with \code{d} being the dimensionality of the search space.
Points are drawn uniformly at random.}

\item{surrogate}{(\code{NULL} | \link{SurrogateLearners})\cr
\link{SurrogateLearners} to be used as a surrogate.
If \code{NULL} \code{default_surrogate(instance)} is used.}

\item{acq_function}{(\code{NULL} | \link{AcqFunctionSmsEgo}).\cr
\link{AcqFunctionSmsEgo} to be used as acquisition function.
If \code{NULL} an \link{AcqFunctionSmsEgo} is used.}

\item{acq_optimizer}{(\link{AcqOptimizer})\cr
\link{AcqOptimizer} to be used as acquisition function optimizer.
If \code{NULL} \code{default_acqopt(acqfun)} is used.}

\item{sampler}{(\code{NULL} | any)\cr
Any optional sampler holding a \verb{$sample()} function.}
}
\value{
invisible(instance)\cr
The original instance is modified in-place and returned invisible.
}
\description{
MBO loop function for sequential multicriteria Bayesian optimization via SmsEGO.
Normally used inside an \link{OptimizerMbo}.
}
\note{
\itemize{
\item If \code{surrogate} is \code{NULL} but \code{acq_function} is given and contains a \verb{$surrogate}, this
\link{SurrogateLearners} is used.
\item You can pass a \code{surrogate} that was not given the \link[bbotk:Archive]{bbotk::Archive} of the
\code{instance} during initialization.
In this case, the \link[bbotk:Archive]{bbotk::Archive} of the given \code{instance} is set during execution.
\item Similarly, you can pass an \code{acq_function} that was not given the \code{surrogate} during initialization
and an \code{acq_optimizer} that was not given the \code{acq_function}, i.e., delayed initialization is
handled automatically.
}
}
\examples{
library(bbotk)
library(paradox)
library(mlr3learners)

fun = function(xs) {
  list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
}
domain = ps(x = p_dbl(lower = -10, upper = 10))
codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

terminator = trm("evals", n_evals = 10)

instance = OptimInstanceMultiCrit$new(
  objective = objective,
  terminator = terminator
)

bayesopt_smsego(instance)
}
\references{
\itemize{
\item Beume N, Naujoks B, Emmerich M (2007).
\dQuote{SMS-EMOA: Multiobjective selection based on dominated hypervolume.}
\emph{European Journal of Operational Research}, \bold{181}(3), 1653--1669.
\item Ponweiser, Wolfgang, Wagner, Tobias, Biermann, Dirk, Vincze, Markus (2008).
\dQuote{Multiobjective Optimization on a Limited Budget of Evaluations Using Model-Assisted S-Metric Selection.}
In \emph{Proceedings of the 10th International Conference on Parallel Problem Solving from Nature}, 784--794.
}
}
\seealso{
Other Loop Function: 
\code{\link{bayesopt_bop}()},
\code{\link{bayesopt_ego}()},
\code{\link{bayesopt_ehvi}()},
\code{\link{bayesopt_mego}()},
\code{\link{bayesopt_mpcl}()},
\code{\link{bayesopt_parego}()}
}
\concept{Loop Function}
