% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kerasOptimizer.R
\name{optimizer_sgd}
\alias{optimizer_sgd}
\title{Stochastic gradient descent (SGD) optimizer}
\usage{
optimizer_sgd(
  learning_rate = 0.01,
  momentum = 0,
  decay = 0,
  nesterov = FALSE,
  clipnorm = NULL,
  clipvalue = NULL,
  ...
)
}
\arguments{
\item{learning_rate}{float >= 0. Learning rate.}

\item{momentum}{float >= 0. Parameter that accelerates SGD in the relevant
direction and dampens oscillations.}

\item{decay}{float >= 0. Learning rate decay over each update.}

\item{nesterov}{boolean. Whether to apply Nesterov momentum.}

\item{clipnorm}{Gradients will be clipped when their L2 norm exceeds this
value.}

\item{clipvalue}{Gradients will be clipped when their absolute value exceeds
this value.}

\item{...}{Unused, present only for backwards compatability}
}
\value{
Optimizer for use with \code{\link{compile.keras.engine.training.Model}}.
}
\description{
Stochastic gradient descent optimizer with support for momentum, learning
rate decay, and Nesterov momentum.
}
\details{
Based on:
[keras/R/optimizers.R](https://github.com/rstudio/keras/blob/main/R/optimizers.R).
The following code is commented:
\code{backcompat_fix_rename_lr_to_learning_rate(...)}
}
\note{
To enable compatibility with the ranges of the learning rates
of the other optimizers, the learning rate \code{learning_rate}
is internally mapped to \code{10 * learning_rate}. That is,
a learning rat of 0.001 will be mapped to 0.01 (which is the default.)
}
\seealso{
Other optimizers: 
\code{\link{optimizer_adadelta}()},
\code{\link{optimizer_adagrad}()},
\code{\link{optimizer_adamax}()},
\code{\link{optimizer_adam}()},
\code{\link{optimizer_nadam}()},
\code{\link{optimizer_rmsprop}()}
}
\concept{optimizers}
