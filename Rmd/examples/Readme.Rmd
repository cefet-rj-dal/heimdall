# Heimdall Rendered Examples

This folder is the practical learning area of `heimdall`. Each rendered example is meant to reduce the distance between the theory of concept drift detection and the code needed to run a detector on a stream.

The main idea is simple: open one example, understand what signal is being monitored, inspect the sequential update loop, and then adapt the same pattern to your own data. For that reason, the examples are intentionally small and visual.

## Why browse these examples

Concept drift detectors are often presented only as formulas or as constructor arguments. That makes them harder to learn than they need to be. The examples in this folder are designed to answer three practical questions:

1. What kind of signal should I monitor with this detector?
2. Is this detector more appropriate for real concept drift or virtual concept drift?
3. What is the minimal Heimdall workflow to get from raw stream to detected drift points?

## Folder structure

- `1_detection/`: each detector monitoring a stream on its own, through the `update_state()` loop.
- `2_online_prediction/`: each detector plugged into `stealthy()`, which retrains a classifier whenever a drift is reported.

Inside each folder:

- `*.md`: rendered walkthroughs.
- `doc/`: Word versions of the same examples.
- `fig/`: figures generated during rendering.
- `r/`: extracted R scripts for users who prefer a plain script version.

The sources are the `.Rmd` files under `Rmd/examples/`; `Rmd/conversor.R` renders them into this folder.

## Examples by drifter

Every drifter has a detection example and an online prediction example.

| Drifter | Kind | Detection | Online prediction | What it shows |
|---|---|---|---|---|
| `dfr_cusum` | real (error-based) | [detection](./1_detection/dfr_cusum.md) | [online prediction](./2_online_prediction/dfr_cusum.md) | Sequential error monitoring with a cumulative sum. |
| `dfr_ddm` | real (error-based) | [detection](./1_detection/dfr_ddm.md) | [online prediction](./2_online_prediction/dfr_ddm.md) | A classic error-rate drift detector. |
| `dfr_ecdd` | real (error-based) | [detection](./1_detection/dfr_ecdd.md) | [online prediction](./2_online_prediction/dfr_ecdd.md) | EWMA control-chart monitoring of the errors. |
| `dfr_eddm` | real (error-based) | [detection](./1_detection/dfr_eddm.md) | [online prediction](./2_online_prediction/dfr_eddm.md) | Gradual degradation, through the distance between errors. |
| `dfr_hddm` | real (error-based) | [detection](./1_detection/dfr_hddm.md) | [online prediction](./2_online_prediction/dfr_hddm.md) | Error-rate monitoring based on Hoeffding bounds. |
| `dfr_adwin` | virtual (univariate) | [detection](./1_detection/dfr_adwin.md) | [online prediction](./2_online_prediction/dfr_adwin.md) | A simple first example with an adaptive window. |
| `dfr_page_hinkley` | virtual (univariate) | [detection](./1_detection/dfr_page_hinkley.md) | [online prediction](./2_online_prediction/dfr_page_hinkley.md) | A classic sequential change-point detector. |
| `dfr_kswin` | virtual (univariate) | [detection](./1_detection/dfr_kswin.md) | [online prediction](./2_online_prediction/dfr_kswin.md) | Window comparison with the Kolmogorov-Smirnov test. |
| `dfr_lbdd` | virtual (univariate) | [detection](./1_detection/dfr_lbdd.md) | [online prediction](./2_online_prediction/dfr_lbdd.md) | Changes of spread across windows. |
| `dfr_mcdd` | virtual (univariate) | [detection](./1_detection/dfr_mcdd.md) | [online prediction](./2_online_prediction/dfr_mcdd.md) | Changes of location across windows. |
| `dfr_kldist` | virtual (multivariate) | [detection](./1_detection/dfr_kldist.md) | [online prediction](./2_online_prediction/dfr_kldist.md) | Distribution comparison with a binned divergence. |
| `dfr_aedd` | virtual (multivariate) | [detection](./1_detection/dfr_aedd.md) | [online prediction](./2_online_prediction/dfr_aedd.md) | Autoencoder reconstruction error, unsupervised. |
| `dfr_multi_criteria` | combined | [detection](./1_detection/dfr_multi_criteria.md) | [online prediction](./2_online_prediction/dfr_multi_criteria.md) | Several detectors combined into one decision. |
| `dfr_inactive` | baseline | [detection](./1_detection/dfr_inactive.md) | [online prediction](./2_online_prediction/dfr_inactive.md) | Never reports a drift: the model is never retrained. |
| `dfr_passive` | baseline | [detection](./1_detection/dfr_passive.md) | [online prediction](./2_online_prediction/dfr_passive.md) | Always reports a drift: the model is retrained on every batch. |

## Suggested learning path

If you are new to Heimdall, a practical order is:

1. start with the detection examples of `dfr_adwin` or `dfr_ddm`;
2. compare them with `dfr_kswin` or `dfr_ecdd`;
3. move to the online prediction examples, and compare a detector with the `dfr_inactive` and `dfr_passive` baselines;
4. move to `dfr_aedd` when you want a multivariate unsupervised workflow.
