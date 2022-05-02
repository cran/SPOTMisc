#' Keras optimizers.
#'
#' Based on:
#' [keras/R/optimizers.R](https://github.com/rstudio/keras/blob/main/R/optimizers.R)
#'
#' The following code is commented:
#'  backcompat_fix_rename_lr_to_learning_rate(...)
#'
#'
#' Stochastic gradient descent optimizer
#'
#' Stochastic gradient descent optimizer with support for momentum, learning
#' rate decay, and Nesterov momentum.
#'
#' @importFrom keras keras
#'
#' @param learning_rate float >= 0. Learning rate.
#' @param momentum float >= 0. Parameter that accelerates SGD in the relevant
#'   direction and dampens oscillations.
#' @param decay float >= 0. Learning rate decay over each update.
#' @param nesterov boolean. Whether to apply Nesterov momentum.
#' @param clipnorm Gradients will be clipped when their L2 norm exceeds this
#'   value.
#' @param clipvalue Gradients will be clipped when their absolute value exceeds
#'   this value.
#' @param ... Unused, present only for backwards compatability
#'
#' @note To enable compatibility with the ranges of the learning rates
#' of the other optimizers, the learning rate \code{learning_rate}
#' is internally mapped to \code{10 * learning_rate}. That is,
#' a learning rat of 0.001 will be mapped to 0.01 (which is the default.)
#'
#' @return Optimizer for use with \code{\link{compile.keras.engine.training.Model}}.
#'
#' @family optimizers
#'
#' @export
optimizer_sgd <- function(learning_rate = 0.01, momentum = 0.0, decay = 0.0, nesterov = FALSE,
                          clipnorm = NULL, clipvalue = NULL, ...) {
  learning_rate = 10*learning_rate

 #  backcompat_fix_rename_lr_to_learning_rate(...)

  # compose args using list so that clipnorm and clipvalue are excluded
  # from the call when they aren't specified
  args <- list(
    learning_rate = learning_rate,
    momentum = momentum,
    decay = decay,
    nesterov = nesterov
  )
  args$clipnorm <- clipnorm
  args$clipvalue <- clipvalue
  do.call(keras$optimizers$SGD, args)
}

#' RMSProp optimizer
#'
#' @importFrom keras keras
#'
#' @inheritParams optimizer_sgd
#' @param rho float >= 0. Decay factor.
#' @param epsilon float >= 0. Fuzz factor. If `NULL`, defaults to `k_epsilon()`.
#'
#' @note It is recommended to leave the parameters of this optimizer at their
#' default values (except the learning rate, which can be freely tuned).
#'
#' This optimizer is usually a good choice for recurrent neural networks.
#'
#' @family optimizers
#'
#' @export
optimizer_rmsprop <- function(learning_rate = 0.001, rho = 0.9, epsilon = NULL, decay = 0.0,
                              clipnorm = NULL, clipvalue = NULL, ...) {

 #  backcompat_fix_rename_lr_to_learning_rate(...)

  # compose args using list so that clipnorm and clipvalue are excluded
  # from the call when they aren't specified

  args <- list(
    learning_rate = learning_rate,
    rho = rho,
    epsilon = epsilon,
    decay = decay
  )
  args$clipnorm <- clipnorm
  args$clipvalue <- clipvalue
  do.call(keras$optimizers$RMSprop, args)
}


#' Adagrad optimizer.
#'
#' Adagrad optimizer as described in [Adaptive Subgradient Methods for Online
#' Learning and Stochastic
#' Optimization](https://www.jmlr.org/papers/volume12/duchi11a/duchi11a.pdf).
#' @importFrom keras keras
#' @inheritParams optimizer_rmsprop
#'
#' @note To enable compatibility with the ranges of the learning rates
#' of the other optimizers, the learning rate \code{learning_rate}
#' is internally mapped to \code{10 * learning_rate}. That is,
#' a learning rat of 0.001 will be mapped to 0.01 (which is the default.)
#' @family optimizers
#'
#' @export
optimizer_adagrad <- function(learning_rate = 0.01, epsilon = NULL, decay = 0.0,
                              clipnorm = NULL, clipvalue = NULL, ...) {
  learning_rate = 10*learning_rate

 #  backcompat_fix_rename_lr_to_learning_rate(...)

  # compose args using list so that clipnorm and clipvalue are excluded
  # from the call when they aren't specified
  args <- list(
    learning_rate = learning_rate,
    epsilon = epsilon,
    decay = decay
  )
  args$clipnorm <- clipnorm
  args$clipvalue <- clipvalue
  do.call(keras$optimizers$Adagrad, args)
}

#' Adadelta optimizer.
#'
#' Adadelta optimizer as described in [ADADELTA: An Adaptive Learning Rate
#' Method](https://arxiv.org/abs/1212.5701).
#'
#' @importFrom keras keras
#'
#' @inheritParams optimizer_rmsprop
#'
#' @note To enbale compatibility with the ranges of the learning rates
#' of the other optimizers, the learning rate \code{learning_rate}
#' is internally mapped to \code{1- learning_rate}. That is,
#' a learning rat of 0 will be mapped to 1 (which is the default.)
#' It is recommended to leave the parameters of this optimizer at their
#'   default values.
#'
#' @family optimizers
#'
#' @export
optimizer_adadelta <- function(learning_rate = 0, rho = 0.95, epsilon = NULL, decay = 0.0,
                               clipnorm = NULL, clipvalue = NULL, ...) {

  # Mapping to get similar intervals
  learning_rate <- 1 - learning_rate

 #  backcompat_fix_rename_lr_to_learning_rate(...)

  # compose args using list so that clipnorm and clipvalue are excluded
  # from the call when they aren't specified
  args <- list(
    learning_rate = learning_rate,
    rho = rho,
    epsilon = epsilon,
    decay = decay
  )
  args$clipnorm <- clipnorm
  args$clipvalue <- clipvalue
  do.call(keras$optimizers$Adadelta, args)
}

#' Adam optimizer
#'
#' Adam optimizer as described in [Adam - A Method for Stochastic
#' Optimization](https://arxiv.org/abs/1412.6980v8).
#' @importFrom keras keras
#' @inheritParams optimizer_rmsprop
#' @param beta_1 The exponential decay rate for the 1st moment estimates. float,
#'   0 < beta < 1. Generally close to 1.
#' @param beta_2 The exponential decay rate for the 2nd moment estimates. float,
#'   0 < beta < 1. Generally close to 1.
#' @param amsgrad Whether to apply the AMSGrad variant of this algorithm from
#'   the paper "On the Convergence of Adam and Beyond".
#'
#' @note Default parameters follow those provided in the original paper.
#'
#' @section References:
#'   - [Adam - A Method for Stochastic Optimization](https://arxiv.org/abs/1412.6980v8)
#'   - [On the Convergence of Adam and Beyond](https://openreview.net/forum?id=ryQu7f-RZ)
#'
#' @family optimizers
#'
#' @export
optimizer_adam <- function(learning_rate = 0.001, beta_1 = 0.9, beta_2 = 0.999, epsilon = NULL, decay = 0.0,
                           amsgrad = FALSE, clipnorm = NULL, clipvalue = NULL, ...) {

 #  backcompat_fix_rename_lr_to_learning_rate(...)

  # compose args using list so that clipnorm and clipvalue are excluded
  # from the call when they aren't specified
  args <- list(
    learning_rate = learning_rate,
    beta_1 = beta_1,
    beta_2 = beta_2,
    epsilon = epsilon
  )
  args$amsgrad <- amsgrad
  args$clipnorm <- clipnorm
  args$clipvalue <- clipvalue
  args$decay <- decay

  do.call(keras$optimizers$Adam, args)
}

# TODO: decay position moved
#   tf.keras.optimizers.Adam(
#     learning_rate=0.001,
#     beta_1=0.9,
#     beta_2=0.999,
#     epsilon=1e-07,
#     amsgrad=False,
#     name='Adam',
#     **kwargs
# )

#' Adamax optimizer
#'
#' Adamax optimizer from Section 7 of the [Adam paper](https://arxiv.org/abs/1412.6980v8).
#' It is a variant of Adam based on the infinity norm.
#'
#' @importFrom keras keras
#'
#' @inheritParams optimizer_adam
#' @note To enable compatibility with the ranges of the learning rates
#' of the other optimizers, the learning rate \code{learning_rate}
#' is internally mapped to \code{2 * learning_rate}. That is,
#' a learning rat of 0.001 will be mapped to 0.002 (which is the default.)
#'
#' @family optimizers
#'
#' @export
optimizer_adamax <- function(learning_rate = 0.002, beta_1 = 0.9, beta_2 = 0.999, epsilon = NULL, decay = 0.0,
                             clipnorm = NULL, clipvalue = NULL, ...) {
  learning_rate = 2*learning_rate

 #  backcompat_fix_rename_lr_to_learning_rate(...)

  # compose args using list so that clipnorm and clipvalue are excluded
  # from the call when they aren't specified
  args <- list(
    learning_rate = learning_rate,
    beta_1 = beta_1,
    beta_2 = beta_2,
    epsilon = epsilon,
    decay = decay
  )
  args$clipnorm <- clipnorm
  args$clipvalue <- clipvalue

  do.call(keras$optimizers$Adamax, args)
}

#' Nesterov Adam optimizer
#'
#' Much like Adam is essentially RMSprop with momentum, Nadam is Adam RMSprop
#' with Nesterov momentum.
#'
#' @importFrom keras keras
#' @importFrom keras k_epsilon
#'
#' @inheritParams optimizer_adam
#' @param schedule_decay Schedule deacy.
#'
#' @details Default parameters follow those provided in the paper. It is
#'   recommended to leave the parameters of this optimizer at their default
#'   values.
#'
#' @note To enable compatibility with the ranges of the learning rates
#' of the other optimizers, the learning rate \code{learning_rate}
#' is internally mapped to \code{2 * learning_rate}. That is,
#' a learning rat of 0.001 will be mapped to 0.002 (which is the default.)
#'
#' @seealso [On the importance of initialization and momentum in deep
#'   learning](https://www.cs.toronto.edu/~fritz/absps/momentum.pdf).
#'
#' @family optimizers
#'
#' @export
optimizer_nadam <- function(learning_rate = 0.002, beta_1 = 0.9, beta_2 = 0.999, epsilon = NULL,
                            schedule_decay = 0.004, clipnorm = NULL, clipvalue = NULL, ...) {
  learning_rate = 2*learning_rate

 #  backcompat_fix_rename_lr_to_learning_rate(...)

  # compose args using list so that clipnorm and clipvalue are excluded
  # from the call when they aren't specified
  args <- list(
    learning_rate = learning_rate,
    beta_1 = beta_1,
    beta_2 = beta_2,
    epsilon = epsilon,
    schedule_decay = schedule_decay
  )
  args$clipnorm <- clipnorm
  args$clipvalue <- clipvalue

  do.call(keras$optimizers$Nadam, args)
}

# resolve_epsilon <- function(epsilon) {
#   if (is.null(epsilon) && keras_version() < "2.1.3")
#     k_epsilon()
#   else
#     epsilon
# }

# backcompat_fix_rename_lr_to_learning_rate <- function(..., lr) {
#   if (!missing(lr)) {
#     warning("the `lr` argument has been renamed to `learning_rate`.")
#     if (!eval.parent(quote(missing(learning_rate))))
#       stop("You can't supply both `lr` and `learning_rate`")
#     assign("learning_rate", lr, parent.frame())
#   }
#   ellipsis::check_dots_empty()
# }

#' @title Select keras optimizer
#'
#' @param name integer specifying the algorithm. Can be one of the following:
#' \code{1=SDG}, \code{2=RMSPROP}, \code{3=ADAGRAD}, \code{4=ADADELTA},
#' \code{5=ADAM}, \code{6=ADAMAX}, or \code{7=NADAM}.
#'
#' ## SGD:
#' @param learning_rate float >= 0. Learning rate.
#' @param momentum float >= 0. Parameter that accelerates SGD in the relevant
#'   direction and dampens oscillations.
#' @param decay float >= 0. Learning rate decay over each update.
#' @param nesterov boolean. Whether to apply Nesterov momentum.
#' @param clipnorm Gradients will be clipped when their L2 norm exceeds this
#'   value.
#' @param clipvalue Gradients will be clipped when their absolute value exceeds
#'   this value.
#'
#' ### RMS:
#' @param rho float >= 0. Decay factor.
#' @param epsilon float >= 0. Fuzz factor. If `NULL`, defaults to `k_epsilon()`.
#'
#' ### ADAM:
#' @param beta_1 The exponential decay rate for the 1st moment estimates. float,
#'   0 < beta < 1. Generally close to 1.
#' @param beta_2 The exponential decay rate for the 2nd moment estimates. float,
#'   0 < beta < 1. Generally close to 1.
#' @param amsgrad Whether to apply the AMSGrad variant of this algorithm from
#'   the paper "On the Convergence of Adam and Beyond".
#' @param ... Unused, present only for backwards compatability
#'
#' @return Optimizer for use with \code{\link{compile.keras.engine.training.Model}}.
#'
#' @export
#'
selectKerasOptimizer <- function(name,
                                 learning_rate = 0.01,
                                 momentum = 0.0,
                                 decay = 0.0,
                                 nesterov = FALSE,
                                 clipnorm = NULL,
                                 clipvalue = NULL,
                                 rho = 0.9,
                                 epsilon = NULL,
                                 beta_1 = 0.9,
                                 beta_2 = 0.999,
                                 amsgrad = FALSE,
                                 ...){
  names <- list("SDG", "RMSPROP", "ADAGRAD", "ADADELTA",
    "ADAM", "ADAMAX", "NADAM")
  # FIXME: remove after testing:
  message("selectKerasOptimizer using optimizer:")
  print(names[[name]])

  switch(names[[name]],
         SDG = {optimizer_sgd(learning_rate = learning_rate,
                            momentum = 0.0,
                            decay = 0.0,
                            nesterov = FALSE,
                            clipnorm = NULL,
                            clipvalue = NULL,
                            ...)},
         RMSPROP = {optimizer_rmsprop(learning_rate = learning_rate,
                                      rho = 0.9,
                                      epsilon = epsilon,
                                      decay = 0.0,
                                      clipnorm = NULL,
                                      clipvalue = NULL, ...)},
         ADAGRAD = {optimizer_adagrad(learning_rate = learning_rate,
                                      epsilon = epsilon,
                                      decay = 0.0,
                                      clipnorm = NULL,
                                      clipvalue = NULL, ...)},
         ADADELTA = {optimizer_adadelta(learning_rate = learning_rate,
                                        rho = 0.95,
                                        epsilon = epsilon,
                                        decay = 0.0,
                                        clipnorm = NULL,
                                        clipvalue = NULL,
                                        ...)},
         ADAM = {
           optimizer_adam(
             # learning rate (default 1e-3)
             learning_rate = learning_rate,
             # The exponential decay rate for the 1st moment estimates. float, 0 < beta < 1.
             # Generally close to 1. Default: 0.9
             beta_1 = beta_1,
             # The exponential decay rate for the 2nd moment estimates.
             # float, 0 < beta < 1. Generally close to 1. Default: 0.99
             beta_2 = beta_2,
             # Fuzz factor. If NULL, defaults to k_epsilon().
             # (default k_epsilon = 1e-7)
             epsilon = epsilon,
             # Learning rate decay over each update. (default 0)
             decay = 0,
             # Whether to apply the AMSGrad variant of this algorithm
             # from the paper "On the Convergence of Adam and Beyond"
             amsgrad = FALSE,
             # Gradients will be clipped when their L2 norm exceeds this value.
             clipnorm = NULL,
             # Gradients will be clipped when their absolute value exceeds this value.
             clipvalue = NULL,
             ...
           )
           },
         ADAMAX = {optimizer_adamax(learning_rate = learning_rate,
                                    beta_1 = beta_1,
                                    beta_2 = beta_2,
                                    epsilon = epsilon,
                                    decay = 0.0,
                                    clipnorm = NULL,
                                    clipvalue = NULL,
                                    ...)},
        NADAM = {optimizer_nadam(learning_rate = learning_rate,
                                 beta_1 = beta_1,
                                 beta_2 = beta_2,
                                 epsilon = epsilon,
                                 schedule_decay = 0.004,
                                 clipnorm = NULL,
                                 clipvalue = NULL, ...)},
         stop("selectKerasOptimizer: Wrong optimizer.")
  )
}




