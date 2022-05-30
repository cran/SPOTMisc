context("spotKeras")
skip_on_cran()

test_that("test that spotKeras and spot return similar results (tol=1e-1)", {
  set.seed(1)
  library("SPOT")
  ## Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  #kerasConf$resDummy <- TRUE

  cfg <-  getModelConf(list(model="dl"))
  lower <- cfg$lower
  upper <- cfg$upper
  types <- cfg$type

  lower[3] <- 8
  upper[3] <- 16
  lower[6] <- 1
  upper[6] <- 2
  upper[9] <- 2

  kerasData <- getMnistData(kerasConf)

  res1 <- spot(x = NULL,
              fun = funKerasMnist,
              lower = lower,
              upper = upper,
              control = list(funEvals = 2,
                             noise = TRUE,
                             types = types,
                             plots = FALSE,
                             progress = TRUE,
                             seedFun = 1,
                             seedSPOT = 1,
                             model = buildTreeModel,
                             designControl = list(size = 1)),
              kerasConf = kerasConf,
              data = kerasData)

  firstX <- res1$xbest
  firstY <- res1$ybest

  set.seed(1)
  res2 <- spotKeras(x = NULL,
                   fun = funKerasMnist,
                   lower = lower,
                   upper = upper,
                   control = list(funEvals = 2,
                                  noise = TRUE,
                                  types = types,
                                  plots = FALSE,
                                  progress = TRUE,
                                  seedFun = 1,
                                  seedSPOT = 1,
                                  model = buildTreeModel,
                                  designControl = list(size = 1)),
                   kerasConf = kerasConf,
                   kerasData = kerasData)

  expect_equal(res1$x[1,], res2$x[1,])
  ## test might fail with some small prob.
  ## FIXME: modify setup so that y values can be tested
  ## expect_equal(res1$ybest, res2$ybest, tol=1e-1)
})

test_that("test spotKeras works with fewer parameters", {
  library("SPOT")
  ## Set CUDA cores to 0, hence disabling GPU indirectly:
  ## Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  model <- "dl"
  cfg <-  getModelConf(list(model=model))
  # Setting lower and upper to default! Thus all parameters now cant be changed
  lower <- cfg$defaults
  upper <- cfg$defaults
  # Now only allowing change for some parameters, manually forcing the "activeness"
  activeVars <- c("layers", "units", "epochs")
  activeInds <- getIndices(model="dl", a = activeVars)
  lower[activeInds] <- cfg$lower[activeInds]
  upper[activeInds] <- cfg$upper[activeInds]
  types <- cfg$type
  set.seed(1)
  res1 <- spotKeras(x = NULL,
                   fun = funKerasMnist,
                   lower = lower,
                   upper = upper,
                   control = list(funEvals = 2,
                                  noise = TRUE,
                                  types = types,
                                  plots = FALSE,
                                  progress = TRUE,
                                  seedFun = 1,
                                  seedSPOT = 1,
                                  designControl = list(size = 1)),
                   kerasConf = kerasConf,
                   kerasData = getMnistData(kerasConf))

  x1st <- matrix(res1$xbest[activeInds], nrow=1)

  kerasConf$active <-  activeVars

  cfg <-  getModelConf(list(model="dl", active = kerasConf$active))
  lower <- cfg$lower
  upper <- cfg$upper
  types <- cfg$type
  set.seed(1)
  res2 <- spotKeras(x = NULL,
                   fun = funKerasMnist,
                   lower = lower,
                   upper = upper,
                   control = list(funEvals = 2,
                                  noise = TRUE,
                                  types = types,
                                  plots = FALSE,
                                  progress = TRUE,
                                  seedFun = 1,
                                  seedSPOT = 1,
                                  designControl = list(size = 1)),
                   kerasConf = kerasConf,
                   kerasData = getMnistData(kerasConf))

  x2nd <- res2$xbest
  ## test might fail with some small prob.
  ## This is due to the different way LHDs are generated. For example, a
  ## 2-dim LHD, which is extended to a 3-dim LHD by adding a constant value
  ## for the 3rd dim is not the same as a 3-dim LHD, where the 3rd dim is set
  ## to a constant value.
  ## expect_equal(res1$ybest, res2$ybest, tol=1e-1)
  expect_equal(dim(x1st) ,dim(x2nd))
})

