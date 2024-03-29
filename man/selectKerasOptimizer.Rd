% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kerasOptimizer.R
\name{selectKerasOptimizer}
\alias{selectKerasOptimizer}
\title{Select keras optimizer}
\usage{
selectKerasOptimizer(
  optimizer,
  learning_rate = 0.01,
  momentum = 0,
  decay = 0,
  nesterov = FALSE,
  clipnorm = NULL,
  clipvalue = NULL,
  rho = 0.9,
  epsilon = NULL,
  beta_1 = 0.9,
  beta_2 = 0.999,
  amsgrad = FALSE,
  ...
)
}
\arguments{
\item{optimizer}{integer specifying the algorithm. Can be one of the following:
\code{1=SDG}, \code{2=RMSPROP}, \code{3=ADAGRAD}, \code{4=ADADELTA},
\code{5=ADAM}, \code{6=ADAMAX}, or \code{7=NADAM}.

## SGD:}

\item{learning_rate}{float >= 0. Learning rate.}

\item{momentum}{float >= 0. Parameter that accelerates SGD in the relevant
direction and dampens oscillations.}

\item{decay}{float >= 0. Learning rate decay over each update.}

\item{nesterov}{boolean. Whether to apply Nesterov momentum.}

\item{clipnorm}{Gradients will be clipped when their L2 norm exceeds this
value.}

\item{clipvalue}{Gradients will be clipped when their absolute value exceeds
  this value.

### RMS:}

\item{rho}{float >= 0. Decay factor.}

\item{epsilon}{float >= 0. Fuzz factor. If `NULL`, defaults to `k_epsilon()`.

### ADAM:}

\item{beta_1}{The exponential decay rate for the 1st moment estimates. float,
0 < beta < 1. Generally close to 1.}

\item{beta_2}{The exponential decay rate for the 2nd moment estimates. float,
0 < beta < 1. Generally close to 1.}

\item{amsgrad}{Whether to apply the AMSGrad variant of this algorithm from
the paper "On the Convergence of Adam and Beyond".}

\item{...}{Unused, present only for backwards compatability}
}
\value{
Optimizer for use with \code{\link{compile.keras.engine.training.Model}}.
}
\description{
Select one of the following optimizers:
"SDG", "RMSPROP", "ADAGRAD", "ADADELTA", "ADAM", "ADAMAX", "NADAM".
}
