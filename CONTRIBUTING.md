# Contributing to heimdall

## Development environment

The repository has a development container (`.devcontainer/devcontainer.json`)
based on R 4.6.1 with the Python backend of `daltoolboxdp` (torch, numpy,
pandas, scikit-learn). In VS Code, run **Dev Containers: Reopen in Container**.

## Git hook

Enable the pre-commit hook once per clone:

```sh
git config core.hooksPath .githooks
```

It rejects staged files whose names cannot be checked out on Windows (for
example names containing `:`) and files that still hold merge conflict
markers. The `repository-checks` workflow runs the same checks on every push.

## Code style and documentation

- Style the code with `styler::style_pkg()`; `lintr::lint_package()` must
  report no lints, since the `lint` workflow fails on any.
- Documentation is written with roxygen2 8.1.1 (pinned in `DESCRIPTION`).
  Run `roxygen2::roxygenise()` after changing roxygen comments and commit the
  regenerated `man/` and `NAMESPACE`.
- `README.md` is generated from `README.Rmd` with
  `rmarkdown::render("README.Rmd")`.

## Examples

Every drifter has a detection example and an online prediction example. The
sources are the `.Rmd` files under `Rmd/examples/`; the files under
`examples/` (`.md`, `doc/`, `fig/`, `r/`) are generated from them. Edit the
sources and regenerate everything with:

```sh
Rscript Rmd/conversor.R
```

The AEDD examples need the Python backend, so run the conversion in the
development container. The `python-backend` workflow renders every example on
each push and fails if one of them breaks.

## Tests

Run the test suite with `testthat::test_local()`. The statistical tests (false
alarms and detection delay across seeds) and the AEDD tests with the real
autoencoder are skipped on CRAN; set `NOT_CRAN=true` to run them.

## Data

The raw flight datasets used by `data-gen/` are not versioned. Place them in
`data-real/` before running the generator scripts.
