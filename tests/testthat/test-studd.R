studd_stream <- function(n, shift_from = n + 1, seed = 1) {
  set.seed(seed)
  x <- matrix(stats::rnorm(n * 2), ncol = 2)
  if (shift_from <= n) {
    rows <- shift_from:n
    x[rows, 2] <- -x[rows, 1] + stats::rnorm(length(rows), 0, 0.2)
  }
  data.frame(x1 = x[, 1], x2 = x[, 2], class = ifelse(x[, 1] + x[, 2] > 0, "a", "b"))
}

test_that("the STUDD models are trained on the classes present", {
  x <- matrix(stats::rnorm(40), 20, dimnames = list(NULL, c("x1", "x2")))
  template <- daltoolbox::cla_dtree("class", c("a", "b", "c"))
  fitted <- heimdall:::.studd_fit(template, x, rep(c("a", "c"), 10), "class")
  expect_equal(fitted$model$slevels, c("a", "c"))
  expect_true(all(heimdall:::.studd_predict(fitted, x) %in% c("a", "c")))
  single <- heimdall:::.studd_fit(template, x, c(rep("b", 19), NA), "class")
  expect_equal(single$constant, "b")
  expect_equal(heimdall:::.studd_predict(single, x[1:3, ]), rep("b", 3))
  expect_error(heimdall:::.studd_fit(template, x, rep("z", 20), "class"), "levels")
  memory <- heimdall:::.studd_memory()
  for (i in 1:300) memory <- heimdall:::.studd_append(memory, c(v = i), as.character(i), keep = 50)
  expect_lte(memory$n, 128)
  expect_equal(heimdall:::.studd_rows(memory, 50)$y, as.character(251:300))
})

test_that("dfr_studd monitors the mimicking error of the student", {
  forest <- daltoolbox::cla_rf("class", c("a", "b"), ntree = 15)
  quiet <- fit(dfr_studd(forest, window_size = 300, seed = 1), studd_stream(1200))
  output <- quiet$drifter_output
  expect_equal(names(output), c("teacher", "student", "error", "statistic", "drift"))
  expect_true(all(is.na(output$error[1:300])))
  expect_false(anyNA(output$error[301:1200]))
  expect_false(quiet$drifted)
  expect_equal(quiet$labels_used, 300)
  model <- fit(dfr_studd(forest, window_size = 300, seed = 1), studd_stream(2400, 1201))
  expect_true(model$drifted)
  drifts <- which(model$drifter_output$drift == 1)
  expect_true(all(drifts > 1200))
  expect_equal(model$labels_used, 300 * (1 + length(drifts)))
  again <- fit(dfr_studd(forest, window_size = 300, seed = 1), studd_stream(2400, 1201))
  expect_identical(again$drifter_output, model$drifter_output)
  model <- reset_state(model)
  expect_false(model$drifted)
  # Labels are only read for the training windows.
  stream <- studd_stream(1200)
  stream$class[301:1200] <- NA
  expect_equal(fit(dfr_studd(forest, window_size = 300, seed = 1), stream)$drifter_output$teacher,
               output$teacher)
})

test_that("dfr_studd can monitor a fitted teacher without labels", {
  stream <- studd_stream(2400, 1201)
  training <- stream[1:300, ]
  training$class <- factor(training$class, c("a", "b"))
  teacher <- fit(daltoolbox::cla_rf("class", c("a", "b"), ntree = 15), training)
  student <- daltoolbox::cla_dtree("class", c("a", "b"))
  model <- fit(dfr_studd(teacher, student, window_size = 300, fit_teacher = FALSE, student_memory = "window",
                         seed = 1), stream[c("x1", "x2")])
  expect_equal(model$labels_used, 0)
  expect_identical(model$teacher$model, teacher)
  expect_lte(model$state$memory$n, 600)
  expect_error(fit(dfr_studd(daltoolbox::cla_dtree("class", c("a", "b"))), stream[c("x1", "x2")]), "label column")
  expect_error(dfr_studd(list()), "classification")
  expect_error(dfr_studd(teacher, daltoolbox::cla_dtree("other", c("a", "b"))), "same attribute")
  expect_error(dfr_studd(teacher, detector = dfr_padd()), "univariate")
  expect_error(dfr_studd(teacher, window_size = 1), "window_size")
})
