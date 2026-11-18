# Hierarchical Random Forests

The Ranger R package source is in [`ranger/`](./ranger/). It adds
hierarchical predictor groups selected by a categorical activation variable.
The package README and help page describe the feature and its usage.

## Development

From this repository root, load the local package with:

```r
devtools::load_all("ranger")
```

Run the package tests with:

```r
devtools::test("ranger")
```

To install the local package into an R library:

```r
install.packages("ranger", repos = NULL, type = "source")
```

The package declares GPL-3; the C++ core retains its upstream MIT notices.
