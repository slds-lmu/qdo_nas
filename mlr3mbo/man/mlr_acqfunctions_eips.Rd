% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AcqFunctionEIPS.R
\name{mlr_acqfunctions_eips}
\alias{mlr_acqfunctions_eips}
\alias{AcqFunctionEIPS}
\title{Acquisition Function Expected Improvement Per Second}
\description{
Expected improvement per second.

It is assumed that calculations are performed on an \link[bbotk:OptimInstanceSingleCrit]{bbotk::OptimInstanceSingleCrit}.
Additionally to target values of the codomain that should be minimized or maximized, the
\link[bbotk:Objective]{bbotk::Objective} of the \link[bbotk:OptimInstanceSingleCrit]{bbotk::OptimInstanceSingleCrit} should return time values.
The column names of the target variable and time variable must be passed as \code{y_cols} in the
order \verb{(target, time)} when constructing the \link{SurrogateLearners} that is being used as a
surrogate.
}
\section{Dictionary}{

This \link{AcqFunction} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link{mlr_acqfunctions} or with the associated sugar function \code{\link[=acqf]{acqf()}}:\preformatted{mlr_acqfunctions$get("eips")
acqf("eips")
}
}

\references{
Snoek, Jasper, Larochelle, Hugo, Adams, P R (2012).
\dQuote{Practical Bayesian Optimization of Machine Learning Algorithms.}
In Pereira F, Burges CJC, Bottou L, Weinberger KQ (eds.), \emph{Advances in Neural Information Processing Systems}, volume 25, 2951--2959.
}
\seealso{
Other Acquisition Function: 
\code{\link{AcqFunction}},
\code{\link{mlr_acqfunctions_aei}},
\code{\link{mlr_acqfunctions_cb}},
\code{\link{mlr_acqfunctions_ehvi}},
\code{\link{mlr_acqfunctions_ei}},
\code{\link{mlr_acqfunctions_ejie}},
\code{\link{mlr_acqfunctions_fuct}},
\code{\link{mlr_acqfunctions_mesn}},
\code{\link{mlr_acqfunctions_pent}},
\code{\link{mlr_acqfunctions_pi}},
\code{\link{mlr_acqfunctions_sms_ego}},
\code{\link{mlr_acqfunctions_ts}}
}
\concept{Acquisition Function}
\section{Super classes}{
\code{\link[bbotk:Objective]{bbotk::Objective}} -> \code{\link[mlr3mbo:AcqFunction]{mlr3mbo::AcqFunction}} -> \code{AcqFunctionEIPS}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{y_best}}{(\code{numeric(1)}).}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{y_col}}{(\code{character(1)}).}

\item{\code{time_col}}{(\code{character(1)}).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{AcqFunctionEIPS$new()}}
\item \href{#method-update}{\code{AcqFunctionEIPS$update()}}
\item \href{#method-clone}{\code{AcqFunctionEIPS$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="eval">}\href{../../bbotk/html/Objective.html#method-eval}{\code{bbotk::Objective$eval()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="format">}\href{../../bbotk/html/Objective.html#method-format}{\code{bbotk::Objective$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="print">}\href{../../bbotk/html/Objective.html#method-print}{\code{bbotk::Objective$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3mbo" data-topic="AcqFunction" data-id="eval_dt">}\href{../../mlr3mbo/html/AcqFunction.html#method-eval_dt}{\code{mlr3mbo::AcqFunction$eval_dt()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3mbo" data-topic="AcqFunction" data-id="eval_many">}\href{../../mlr3mbo/html/AcqFunction.html#method-eval_many}{\code{mlr3mbo::AcqFunction$eval_many()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionEIPS$new(surrogate = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{surrogate}}{(\code{NULL} | \link{SurrogateLearners}).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-update"></a>}}
\if{latex}{\out{\hypertarget{method-update}{}}}
\subsection{Method \code{update()}}{
Updates acquisition function and sets \code{y_best}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionEIPS$update()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionEIPS$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
