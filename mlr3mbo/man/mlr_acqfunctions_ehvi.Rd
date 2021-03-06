% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AcqFunctionEHVI.R
\name{mlr_acqfunctions_ehvi}
\alias{mlr_acqfunctions_ehvi}
\alias{AcqFunctionEHVI}
\title{Acquisition Function Expected Hypervolume Improvement}
\description{
Expected Hypervolume Improvement.
}
\section{Parameters}{

\itemize{
\item \code{"n_mc"} (\code{integer(1)})\cr
Number of MC samples to approximate the expectation.
}
}

\seealso{
Other Acquisition Function: 
\code{\link{AcqFunction}},
\code{\link{mlr_acqfunctions_aei}},
\code{\link{mlr_acqfunctions_cb}},
\code{\link{mlr_acqfunctions_eips}},
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
\code{\link[bbotk:Objective]{bbotk::Objective}} -> \code{\link[mlr3mbo:AcqFunction]{mlr3mbo::AcqFunction}} -> \code{AcqFunctionEHVI}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{ref_point}}{(\code{numeric()}).}

\item{\code{hypervolume}}{(\code{numeric(1)}).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{AcqFunctionEHVI$new()}}
\item \href{#method-update}{\code{AcqFunctionEHVI$update()}}
\item \href{#method-clone}{\code{AcqFunctionEHVI$clone()}}
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
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionEHVI$new(surrogate = NULL, n_mc = 100L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{surrogate}}{(\code{NULL} | \link{SurrogateLearners}).}

\item{\code{n_mc}}{(\code{integer(1)}).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-update"></a>}}
\if{latex}{\out{\hypertarget{method-update}{}}}
\subsection{Method \code{update()}}{
Updates acquisition function and sets \code{ys_front}, \code{ref_point}, \code{epsilon}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionEHVI$update()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionEHVI$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
