# Hierarchical Random Forests

This repository contains a research modification of
[`ranger`](https://github.com/imbs-hl/ranger), the fast random-forest
implementation for R. It adds hierarchical predictor groups to the ranger
package; it is not the official upstream `ranger` repository.

## Hierarchical predictors

Set `activation.var` to a factor or character predictor whose values select
which predictors are available to the model. Supply a named
`hierarchical.variable.groups` list with one predictor-name vector per
activation value:

```r
fit <- ranger(
  y ~ .,
  data = training_data,
  activation.var = "Z",
  hierarchical.variable.groups = list(
    A = c("x1", "x2"),
    B = c("x3", "x4")
  )
)
```

When a node contains more than one activation value, the activation variable
and predictors shared by all activation values in that node are eligible for
splitting. Once a node contains one activation value, only the predictors
assigned to that value are eligible. Ordinary tree stopping rules apply at
every node, and activation splits can separate any subset of the levels.

The activation variable must be categorical, must have no missing values, and
must not also appear in a hierarchical predictor group. In hierarchical mode,
impurity importance and `node.stats` are not available; permutation importance
can be used. See `?ranger` for the full argument details and limitations.

## Install from source

Install the package from this directory with:

```r
install.packages(".", repos = NULL, type = "source")
```

The package requires R, Rcpp, RcppEigen, and Matrix. On Windows, an Rtools
installation is needed to compile packages from source.

## Tests

Run the package tests from this directory with:

```r
devtools::test()
```

The hierarchical-specific cases are in
`tests/testthat/test_hierarchical.R`.

## Provenance and license

This work is based on the `ranger` R package by Marvin N. Wright, Stefan Wager,
and Philipp Probst. Upstream package details and the source license are retained
in `DESCRIPTION`, `NEWS`, and the source-file headers. The R package is
distributed under GPL-3; the C++ core files retain their upstream MIT license
notices.
