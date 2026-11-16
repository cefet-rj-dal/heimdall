# Validation: Population Stability Index (PSI)

Checks `psi()` and `dfr_psi()` against Kurian and Allali (2024), *Detecting
drifts in data streams using Kullback-Leibler (KL) divergence measure for
data engineering applications* (Journal of Data, Information and Management
6, 207-216), and its reference code (<https://github.com/FrancisKurian/Kullback>:
`Data_Simulation.sas` and `PSI_computation.sas`).

```sh
# inside the devcontainer, from the package root
git clone https://github.com/FrancisKurian/Kullback data-real/psi/reference   # optional: the SAS code
OPENBLAS_NUM_THREADS=1 Rscript development/validation/psi/run_psi.R 6 > \
  development/validation/results/psi/run.log
```

Results go to `development/validation/results/psi/`.

## Data

The paper's data are simulated: a base sample of 100,000 records with four
fields, and a "through the door" (TTD) copy with one data issue per field.
SAS is not available here, and its random streams (`rand`, `ranuni`) cannot be
reproduced in R. So the data are drawn in R with the rules of
`Data_Simulation.sas`, with 20 seeds.

| field | base | data issue in the TTD sample |
|---|---|---|
| AdResponse | N(8000, 800), rounded to 10 | 10% of the records set to 0 |
| SalesVolume | N(350000, 13000), rounded to 10 | half of the records up to 341,224 increased by 10% |
| Deposits | N(75000, 20000), rounded to 10 | 20% of the records divided by 1000 |
| CBM | N(610, 50), rounded | half of the records above 643 decreased by 200 |

The SAS code differs from the text of the paper. The text (Tables 2-3) gives
a standard deviation of 1000 for AdResponse, missing values instead of zeros,
and a 50-point drop for 10% of the top quartile of CBM. The published tables
come from the code: the decile ranges of the Appendix (AdResponse decile 1 up
to 6970; CBM decile 1 from 378 to 545, with 22% of the TTD sample) match the
code's rules and not the text.

## 1. Tables 4-6: total PSI (`tables_4_6.csv`)

Mean over 20 seeds, against the paper. `z` is the difference over the
standard deviation across seeds.

| field | deciles | paper | z | demi-deciles | paper | z |
|---|---|---|---|---|---|---|
| AdResponse | 0.0672 | 0.0672 | -0.02 | 0.1111 | 0.1111 | 0.04 |
| SalesVolume | 0.1249 | 0.1245 | 0.15 | 0.1350 | 0.1343 | 0.24 |
| Deposits | 0.2260 | 0.2253 | 0.23 | 0.3413 | 0.3408 | 0.11 |
| CBM | 0.1769 | 0.1711 | 2.48 | 0.2357 | 0.2276 | 2.39 |

Per bin (Tables 4-5, `tables_4_5_bins.csv`), the largest difference from the
paper is below 0.0012 for AdResponse, SalesVolume and Deposits. For CBM it is
in bin 1: 0.0033 for deciles, 0.0060 for demi-deciles.

CBM is high because of the paper's draw. In its Table 10, the records that
leave deciles 8-10 (the sum of their P - Q) are 12.29% of the TTD sample. The
rule moves on average 12.57% (half of the 25.1% above 643), with a standard
deviation of 0.10 points for 100,000 records, so the paper's draw is 2.7
standard deviations low. The PSI of a field depends mostly on the share of
records its data issue moved. For three fields the paper's tables give that
share:

* AdResponse and Deposits send the moved records to bin 1, so the share is
  P - Q of bin 1 over 0.9.
* CBM empties part of bins 8-10, so the share is the sum of their P - Q.

A linear fit over the seeds of the PSI on the share then predicts the PSI of
the paper's draw (`tables_4_6_conditional.csv`):

| field | share in the paper | predicted, deciles | paper | predicted, demi-deciles | paper |
|---|---|---|---|---|---|
| AdResponse | 10.02% | 0.0675 | 0.0672 | 0.1117 | 0.1111 |
| Deposits | 19.99% | 0.2252 | 0.2253 | 0.3402 | 0.3408 |
| CBM | 12.29% | 0.1713 | 0.1711 | 0.2279 | 0.2276 |

All are within 0.0007 of the paper. SalesVolume moves records both ways, so
its share cannot be read from the tables; its unconditional mean is already
within 0.25 standard deviations.

The drift levels are those of the paper's conclusions (little < 0.1 <
moderate < 0.25 < significant): AdResponse is "little" at deciles and
"moderate" at demi-deciles, as the paper discusses, and Deposits at
demi-deciles is the only "significant" one. The paper's Section 3.1 gives 0.2
as the second limit, which would also flag Deposits at deciles and CBM at
demi-deciles.

## 2. Implementation against the SAS macro

`psi()` follows `%divergence_estimator`:

* The baseline is split by `PROC RANK GROUPS=`: group
  `floor(rank * bins / (n + 1))`, with tied values given their mean rank, so a
  group never splits ties.
* Each bin covers the baseline values from its smallest to its largest.
* New values outside the baseline range are moved to its limits.
* The shares are counts over the sample sizes; missing new values count in
  the size.
* An empty bin contributes 0, because the SAS logarithm of 0 is missing and
  set to 0.

The one deliberate difference is about gaps. In the macro, a new value that
falls strictly between two bins (between the largest value of one and the
smallest of the next) matches no range of the SAS format and drops out of the
counts. `psi()` puts it in the lower bin instead. In the simulation this
concerns only SalesVolume, whose increased values are not rounded: 14 of
100,000 records at deciles and 33 at demi-deciles (`gaps` in `psi_runs.csv`),
so at most 0.03% of the TTD sample is counted in a neighbouring bin.

## 3. Stream detector (`detector_windows.csv`)

`dfr_psi()` compares windows with a baseline. The validation uses a baseline
of 10,000 base records, 20 windows from the base sample (no drift) and 20
from the TTD sample (9 of each for windows of 5,000), over 20 seeds. On one
stream, `dfr_psi()` gives the same PSI as `psi()` on every window.

Without drift the PSI is not 0. Its mean is close to
`(bins - 1)(1/n_baseline + 1/n_window)`:

| window | mean PSI without drift | formula | windows above 0.1 | windows above 0.25 |
|---|---|---|---|---|
| 100 | 0.096 | 0.091 | 41% | 0.6% |
| 250 | 0.038 | 0.037 | 0.4% | 0 |
| 500 | 0.019 | 0.019 | 0 | 0 |
| 1000 | 0.010 | 0.010 | 0 | 0 |

The paper's limits therefore assume large samples. With windows of a few
hundred records, 0.1 already gives false alarms. With the TTD windows and a
threshold of 0.1 and windows of 1000:

* Deposits and CBM are flagged in every window.
* SalesVolume is flagged in 90% of the windows.
* AdResponse (PSI 0.078) is flagged in 11%.

At 0.25, only Deposits is flagged (in 36% of the windows).

## Not reproduced

The paper has no other experiment: Tables 4-12 and Figures 2-5 all come from
the four simulated fields. Figures 2-5 are histograms of the data; they are not
redrawn.
