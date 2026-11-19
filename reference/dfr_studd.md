# Student-Teacher Unsupervised Drift Detection (STUDD)

Unsupervised drift detector of Cerqueira, Gomes, Bifet and Torgo (2023).
A classifier (the teacher) is trained on an initial window of
`window_size` labelled observations, and a second classifier (the
student) is trained on the same observations to mimic the teacher, with
the teacher's predictions as its target. After that no label is used:
every new observation is predicted by both models, and the mimicking
error (1 when the student disagrees with the teacher, 0 otherwise) feeds
a change detector, by default the Page-Hinkley test. When the detector
raises an alarm, the labels of the last `window_size` observations are
requested to retrain the teacher, and the student is retrained on the
new teacher's predictions. Since the mimicking error does not depend on
the labels, it is used for **virtual concept drift**.

## Usage

``` r
dfr_studd(
  teacher,
  student = teacher,
  detector = dfr_page_hinkley(delta = 0.001),
  window_size = 500,
  student_memory = "all",
  fit_teacher = TRUE,
  target_feat = NULL,
  seed = NULL
)
```

## Arguments

- teacher:

  A `daltoolbox` classification object (for example
  `daltoolbox::cla_rf("class", levels)`), whose `attribute` is the label
  column of the stream.

- student:

  A `daltoolbox` classification object for the student, with the same
  `attribute` and levels. By default the same algorithm as the teacher.

- detector:

  Univariate detector fed with the mimicking errors (a `dist_based` or
  `error_based` object of this package). The default is the Page-Hinkley
  test with `delta = 0.001`, as in the paper.

- window_size:

  Number of observations of the initial training window and of the
  window of labels requested after every drift (`W` in the paper).

- student_memory:

  Observations used to retrain the student after a drift: `"all"` (the
  default, as in the reference code) uses every observation seen since
  the end of the initial window; `"window"` only the last `window_size`
  ones, which bounds memory.

- fit_teacher:

  If `TRUE` (the default), the teacher is trained by the detector on the
  labels of the initial window and retrained after every drift. If
  `FALSE`, the teacher must already be fitted, it is never retrained and
  the stream needs no label column: only the student is trained, on the
  teacher's predictions.

- target_feat:

  Names of the feature columns. `NULL` (the default) uses every numeric
  column except the label column.

- seed:

  Seed of the random number generator used when the models are trained.
  `NULL` draws it from the global generator when the detector is
  created; the global random state is left unchanged.

## Value

`dfr_studd` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds, for every monitored row, the class predicted by the teacher
(`teacher`) and by the student (`student`), the mimicking error
(`error`) and the detector statistic (`statistic`), and whether a drift
was reported (`drift`). The classes are given by their position in the
levels of the teacher. `obj$labels_used` counts the labels used to train
the teacher.

## Details

Labels are read only for the rows used to (re)train the teacher; every
other row may have a missing label. The initial window is used only for
training: no drift is checked on it. The rows of a drift alarm are
included in the window used to retrain the teacher, as in the reference
implementation (<https://github.com/vcerqueira/studd>,
`studd_batch.py`).

A model is always trained on the classes present in its training data,
so that, as in scikit-learn, a class missing from a window does not stop
the training. When a single class is present, the model predicts that
class. Predictions are the class of highest probability (the first one
in case of ties).
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
resets the detector and keeps the models.

## References

Cerqueira, V., Gomes, H. M., Bifet, A. and Torgo, L. (2023). STUDD: a
student-teacher method for unsupervised concept drift detection.
*Machine Learning*, 112, 4351-4378. <doi:10.1007/s10994-022-06188-7>

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
# Two classes separated by x1 + x2 = 0; from row 1201 on, the observations
# concentrate along the class boundary, where the student imitates the
# teacher worse.
n <- 2400
x <- matrix(stats::rnorm(n * 2), ncol = 2)
x[1201:n, 2] <- -x[1201:n, 1] + stats::rnorm(1200, 0, 0.2)
y <- ifelse(x[, 1] + x[, 2] > 0, "a", "b")
stream <- data.frame(x1 = x[, 1], x2 = x[, 2], class = y)
teacher <- cla_rf("class", c("a", "b"), ntree = 20)
detector <- dfr_studd(teacher, window_size = 400, seed = 1)
model <- fit(detector, stream)
which(model$drifter_output$drift == 1)
#> integer(0)
model$labels_used
#> [1] 400
```
