library(ranger)
context("ranger_hierarchical")

test_that("mixed activation nodes can split on shared predictors", {
  set.seed(10)
  n <- 60
  dat <- data.frame(
    Z = factor(rep(c("A", "B"), each = n / 2)),
    x = rnorm(n),
    xA = rnorm(n),
    xB = rnorm(n)
  )
  dat$y <- dat$x

  rf <- ranger(
    y ~ ., data = dat, num.trees = 5, replace = FALSE, sample.fraction = 1,
    mtry = 2,
    activation.var = "Z",
    hierarchical.variable.groups = list(A = c("x", "xA"), B = c("x", "xB")),
    respect.unordered.factors = "partition",
    verbose = FALSE
  )

  activation.id <- match("Z", rf$forest$independent.variable.names) - 1L
  shared.id <- match("x", rf$forest$independent.variable.names) - 1L
  root.ids <- vapply(rf$forest$split.varIDs, `[`, numeric(1), 1)
  expect_true(all(root.ids %in% c(activation.id, shared.id)))
  expect_true(any(root.ids == shared.id))
})

test_that("only branch-assigned variables are eligible after activation", {
  set.seed(11)
  n <- 200
  dat <- data.frame(
    Z = factor(rep(c("A", "B"), each = n / 2)),
    xA = rnorm(n),
    xB = rnorm(n)
  )
  dat$y <- ifelse(dat$Z == "A", dat$xA, dat$xB) + rnorm(n, sd = 0.1)

  rf <- ranger(
    y ~ ., data = dat, num.trees = 10, mtry = 1,
    activation.var = "Z",
    hierarchical.variable.groups = list(A = character(0), B = "xB"),
    verbose = FALSE
  )

  split.ids <- unlist(rf$forest$split.varIDs)
  split.names <- rf$forest$independent.variable.names[split.ids + 1L]
  expect_true(all(split.names %in% c("Z", "xB")))
  expect_true(any(split.names == "xB"))
})

test_that("activation partitions are selected by response rather than factor order", {
  make_data <- function(levels) {
    activation <- factor(rep(c("A", "B", "C"), each = 20), levels = levels)
    data.frame(Z = activation, y = ifelse(activation == "B", 10, 0))
  }
  fit <- function(data) {
    ranger(
      y ~ Z, data = data, num.trees = 1, replace = FALSE, sample.fraction = 1,
      activation.var = "Z",
      hierarchical.variable.groups = list(A = character(0), B = character(0), C = character(0)),
      respect.unordered.factors = "ignore", verbose = FALSE
    )
  }

  for (levels in list(c("A", "B", "C"), c("C", "A", "B"))) {
    data <- make_data(levels)
    forest <- fit(data)$forest
    split <- as.integer(forest$split.values[[1]][1])
    in_right_child <- vapply(c("A", "B", "C"), function(level) {
      factor_id <- match(level, levels)
      bitwAnd(split, bitwShiftL(1L, factor_id - 1L)) != 0L
    }, logical(1))
    expect_identical(in_right_child[["A"]], in_right_child[["C"]])
    expect_false(in_right_child[["A"]] == in_right_child[["B"]])
  }
})

test_that("ordinary stopping rules apply at mixed activation nodes", {
  n <- 30
  dat <- data.frame(
    Z = factor(rep(c("A", "B"), each = n / 2)),
    x = rnorm(n),
    y = rnorm(n)
  )

  rf <- ranger(
    y ~ ., data = dat, num.trees = 1,
    min.node.size = n, min.bucket = n, max.depth = 1,
    activation.var = "Z",
    hierarchical.variable.groups = list(A = "x", B = "x"),
    verbose = FALSE
  )

  expect_length(rf$forest$split.varIDs[[1]], 1)
})

test_that("hierarchical mode validates activation groups and prediction values", {
  dat <- data.frame(
    Z = factor(rep(c("A", "B"), each = 10)),
    x = rnorm(20),
    y = rnorm(20)
  )

  expect_error(ranger(y ~ ., dat, activation.var = "Z"))
  expect_error(ranger(y ~ ., dat, activation.var = "missing",
                      hierarchical.variable.groups = list(A = "x", B = "x")))
  expect_error(ranger(y ~ ., dat, activation.var = "Z",
                      hierarchical.variable.groups = list(A = "x")))

  rf <- ranger(
    y ~ ., data = dat, num.trees = 2,
    activation.var = "Z",
    hierarchical.variable.groups = list(A = "x", B = "x"),
    verbose = FALSE
  )
  expect_error(predict(rf, data.frame(Z = factor("C"), x = 0)))
  expect_error(predict(rf, data.frame(Z = NA, x = 0)))
  expect_error(predict(rf$forest, data.frame(Z = factor("C"), x = 0)))
})

test_that("logical activation predictors are rejected", {
  dat <- data.frame(
    Z = rep(c(FALSE, TRUE), each = 20),
    x = rnorm(40),
    y = rnorm(40)
  )
  groups <- setNames(list("x", "x"), c("FALSE", "TRUE"))
  expect_error(ranger(
    y ~ ., data = dat, num.trees = 3,
    activation.var = "Z", hierarchical.variable.groups = groups,
    verbose = FALSE
  ), "factor or character")
})
