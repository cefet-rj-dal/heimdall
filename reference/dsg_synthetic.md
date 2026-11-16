# Synthetic benchmark sources

Synthetic streams commonly used in stream learning, as defined in the
`river` library, turned into
[`dsg_source_regions()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source_regions.md)
sources: their concept functions give the real drift states, and
regional sub-sampling gives the virtual drift states (Hinder et al.,
2026).

- `dsg_sea()`: three features uniform in \[0, 10\]; label
  `x1 + x2 > theta` with `theta` in 8, 9, 7 and 9.5 (Street and Kim,
  2001).

- `dsg_agrawal()`: the nine features of the loan application generator
  of Agrawal et al. (1993) and its classification functions 0 to 8.

- `dsg_sine()`: two features uniform in \[0, 1\] and the four SINE
  functions.

- `dsg_mixed()`: two Boolean (`x1`, `x2`) and two numeric (`x3`, `x4`)
  features and the two MIXED functions.

## Usage

``` r
dsg_sea(...)

dsg_agrawal(...)

dsg_sine(...)

dsg_mixed(...)
```

## Arguments

- ...:

  Arguments for
  [`dsg_source_regions()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source_regions.md)
  and
  [`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md),
  such as `n_clusters`, `n_variants` or `test_size`.

## Value

`dsg_source` object

## References

Agrawal, R., Imielinski, T., and Swami, A. (1993). Database mining: a
performance perspective. *IEEE Transactions on Knowledge and Data
Engineering*, 5(6), 914-925.

Street, W. N., and Kim, Y. (2001). A streaming ensemble algorithm (SEA)
for large-scale classification. In *KDD 2001*, 377-382.

Gama, J., Medas, P., Castillo, G., and Rodrigues, P. (2004). Learning
with drift detection. In *SBIA 2004*, 286-295.

Montiel, J. et al. (2021). River: machine learning for streaming data in
Python. *Journal of Machine Learning Research*, 22(110), 1-8.

## Examples

``` r
set.seed(1)
dsg_take(dsg_agrawal(), 3)
#> $x
#>      salary commission age elevel car zipcode   hvalue hyears      loan
#> 1 118267.97       0.00  31      3   1       5 340522.1     13 143080.60
#> 2  31535.43   62586.92  55      0  10       2 804254.7     30 313643.36
#> 3  69847.39   82028.01  20      1   8       2 848886.4      5  57905.65
#> 
#> $y
#> [1] 1 0 1
#> 
```
