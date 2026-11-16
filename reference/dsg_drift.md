# Induce a drift in a benchmark source

Moves a
[`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md)
to another concept and verifies the drift: a virtual drift must make the
kernel two-sample test
([`dsg_mmd_test()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_mmd_test.md))
reject at `virtual_p_value`, and a real drift must change at least
`real_drift_level` of the labels of the same points. Candidates that
fail are discarded; after `max_tries` candidates, the strongest real
drift found is used with a warning.

## Usage

``` r
dsg_drift(source, type = "a")
```

## Arguments

- source:

  `dsg_source` object

- type:

  `"-"` (or `"_"`) no drift, `"v"` pure virtual drift, `"r"` pure real
  drift, `"c"` both, `"a"` any of them.

## Value

list with the updated `source`, the logical flags `virtual` and `real`,
and the verification statistics `p_value` (two-sample test, `NA` without
virtual drift) and `label_change` (share of changed labels, `NA` without
real drift)

## Examples

``` r
set.seed(2)
drift <- dsg_drift(dsg_sine(), "r")
drift$real
#> [1] TRUE
drift$label_change
#> [1] 0.748
```
