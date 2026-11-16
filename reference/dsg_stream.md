# Generate a benchmark stream with controlled drift

Builds a stream segment by segment. Each character of `pattern` first
induces a drift with
[`dsg_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_drift.md)
(`"-"` or `"_"` none, `"v"` virtual, `"r"` real, `"c"` both, `"a"` any)
and then draws `batch_size` samples from the resulting concept.

## Usage

``` r
dsg_stream(source, pattern, batch_size = 500)
```

## Arguments

- source:

  `dsg_source` object

- pattern:

  Drift pattern, such as `"--v--r-"`.

- batch_size:

  Number of samples per segment.

## Value

list with

- `data`: data frame with the features, the label `y`, the `segment` and
  the `drift` code of the segment (0 none, 1 virtual, 2 real, 3 both, as
  in the reference implementation);

- `segments`: one row per segment with the drift type, the concept and
  the verification statistics;

- `source`: the source in its final concept.

## Examples

``` r
set.seed(3)
stream <- dsg_stream(dsg_sea(), "--v--r", batch_size = 200)
head(stream$data)
#>         x1       x2        x3 y segment drift
#> 1 3.830383 0.290185 2.5192418 0       1     0
#> 2 5.245535 4.662464 0.1638364 1       1     0
#> 3 8.853655 1.666801 3.5478789 1       1     0
#> 4 9.051492 7.012420 7.3198815 1       1     0
#> 5 1.769911 9.263339 9.4843713 1       1     0
#> 6 5.799461 4.700343 1.9476484 1       1     0
stream$segments
#>   segment type drift virtual_state real_state p_value label_change
#> 1       1    -     0             1          2      NA           NA
#> 2       2    -     0             1          2      NA           NA
#> 3       3    v     1             2          2       0           NA
#> 4       4    -     0             2          2      NA           NA
#> 5       5    -     0             2          2      NA           NA
#> 6       6    r     2             2          3      NA        0.148
```
