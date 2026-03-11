# Heimdall Rendered Examples

This folder is the practical learning area of `heimdall`. Each rendered example is meant to reduce the distance between the theory of concept drift detection and the code needed to run a detector on a stream.

The main idea is simple: open one example, understand what signal is being monitored, inspect the sequential update loop, and then adapt the same pattern to your own data. For that reason, the examples are intentionally small and visual.

## Why browse these examples

Concept drift detectors are often presented only as formulas or as constructor arguments. That makes them harder to learn than they need to be. The examples in this folder are designed to answer three practical questions:

1. What kind of signal should I monitor with this detector?
2. Is this detector more appropriate for real concept drift or virtual concept drift?
3. What is the minimal Heimdall workflow to get from raw stream to detected drift points?

## Folder structure

- `*.md`: rendered detector walkthroughs.
- `doc/`: Word versions of the same examples.
- `fig/`: figures generated during rendering.
- `r/`: extracted R scripts for users who prefer a plain script version.

## Real concept drift examples

These examples are recommended when you want to monitor predictive behavior through an error stream or another performance-related signal.

- [dfr_cusum.md](./dfr_cusum.md): A compact introduction to sequential error monitoring with CUSUM.
- [dfr_ddm.md](./dfr_ddm.md): A practical starting point for users who want a classic error-rate drift detector.
- [dfr_ecdd.md](./dfr_ecdd.md): An EWMA-style example for users interested in control-chart monitoring.
- [dfr_eddm.md](./dfr_eddm.md): A useful example when gradual degradation matters more than abrupt jumps.
- [dfr_hddm.md](./dfr_hddm.md): A statistically grounded example based on Hoeffding bounds.

## Virtual concept drift examples

These examples are recommended when you want to monitor changes in the feature distribution directly, even without labels.

- [dfr_adwin.md](./dfr_adwin.md): A simple first example for univariate streaming data.
- [dfr_aedd.md](./dfr_aedd.md): A multivariate unsupervised example based on autoencoder reconstruction error.
- [dfr_kldist.md](./dfr_kldist.md): A direct distribution-comparison example based on KL divergence.
- [dfr_kswin.md](./dfr_kswin.md): A window-based statistical test example using the Kolmogorov-Smirnov test.
- [dfr_lbdd.md](./dfr_lbdd.md): A detector focused on variance shifts across windows.
- [dfr_mcdd.md](./dfr_mcdd.md): A detector focused on central-tendency changes across windows.

## Flexible sequential detector

The detector below can be used with different monitored signals. In the rendered example, it is applied to a numeric stream so that readers can understand it as a feature-monitoring workflow.

- [dfr_page_hinkley.md](./dfr_page_hinkley.md): A classic sequential change-point detector with a compact API and an easy-to-follow example.

## Suggested learning path

If you are new to Heimdall, a practical order is:

1. start with [dfr_adwin.md](./dfr_adwin.md) or [dfr_ddm.md](./dfr_ddm.md);
2. compare them with [dfr_kswin.md](./dfr_kswin.md) or [dfr_ecdd.md](./dfr_ecdd.md);
3. move to [dfr_aedd.md](./dfr_aedd.md) when you want a multivariate unsupervised workflow.
