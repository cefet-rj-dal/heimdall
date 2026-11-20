# Experiments of dos Reis et al. (2016) with heimdall's iks() / dfr_iks().
#   Rscript development/validation/iks/run_iks.R timing <out dir>
#   Rscript development/validation/iks/run_iks.R classify <datasets dir> <out dir> [cores]
#
# timing (Section 6.1, Figures 1 and 2): accumulated time of the KS statistic
#   for two always growing samples (up to 5000 observations) and for two sliding
#   windows of 100..1000 over streams of 10000, with iks(), with the standard KS
#   (stats::ks.test on the samples) and with the "optimized KS" (sorted samples
#   kept by linear insertion/removal, statistic by a linear merge), 30 repetitions
#   as the paper (10 for the standard KS with sliding windows).
#
# classify (Sections 4-6, Tables 2 and 3): the stream classification protocol of
#   the paper; the authors' code for it is not available, so it follows the text:
#   * the model is trained on the first W instances (W = 500 for Arabic and
#     Posture, 100 for the others), which are the reference sample of every
#     monitored feature; each later instance is predicted, then enters the
#     sliding window of the last W instances; KS with alpha = 0.001
#     (c(alpha) = 1.95, the reference code);
#   * BL1: never adapts. BL2: retrains on the last W instances at random moments,
#     as many as MR adaptations (100 runs). TL: retrained on the last W instances
#     before every prediction. MR: dfr_iks() on the features (decision-tree
#     features only for DT); on a drift, the labels of the window are requested,
#     the model is retrained on it and it becomes the reference. AB (1-NN): the
#     drifted features of the reference are transformed to the mean and standard
#     deviation of the window (f(v) = (v - mu_ref) sd_cur / sd_ref + mu_cur, the
#     reference repository's FAQ); if the transformed reference passes the test
#     the model is retrained on it without labels, otherwise MR. MRP (naive
#     Bayes): the monitored values are the class probabilities, the reference
#     being their leave-one-out estimates on the training data. CD (naive Bayes):
#     consecutive sliding windows of the class probabilities (Zliobaite), filled
#     again with 2 W new outputs after every retraining.
#     Adaptree (AT) is not reproduced (its description is not enough to
#     implement it).
#   * classifiers: 1-NN (Euclidean, raw features), Gaussian naive Bayes, and a
#     fully grown decision tree (rpart, cp = 0, minsplit = 2).
#   * accuracy on the instances after the first W; labels = W (1 + number of
#     retrainings with labels) / N, TL 100 %.
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
mode <- args[1]

if (mode == "timing") {
  out <- args[2]
  optimized_insert <- function(v, x) append(v, x, after = findInterval(x, v))
  optimized_remove <- function(v, x) v[-match(x, v)]
  optimized_ks <- function(a, b) {
    # both sorted; ECDF difference at every observation by a linear merge
    all <- c(a, b)
    ord <- order(all)
    step <- ifelse(ord <= length(a), 1 / length(a), -1 / length(b))
    max(abs(cumsum(step)))
  }
  rows <- list()
  add_row <- function(...) rows[[length(rows) + 1L]] <<- data.frame(...)
  for (rep in 1:30) {
    set.seed(rep)
    a <- rnorm(5000)
    b <- rnorm(5000)
    s <- iks(seed = rep)
    t0 <- proc.time()[3]
    for (i in 1:5000) {
      s <- iks_add(iks_add(s, a[i], 0, runif(1)), b[i], 1, runif(1))
      iks_statistic(s)
    }
    add_row(experiment = "growing", size = 5000, method = "IKS", rep = rep, seconds = proc.time()[3] - t0)
    va <- vb <- numeric(0)
    t0 <- proc.time()[3]
    for (i in 1:5000) {
      va <- optimized_insert(va, a[i])
      vb <- optimized_insert(vb, b[i])
      optimized_ks(va, vb)
    }
    add_row(experiment = "growing", size = 5000, method = "Optimized KS", rep = rep, seconds = proc.time()[3] - t0)
    t0 <- proc.time()[3]
    for (i in 1:5000) suppressWarnings(ks.test(a[1:i], b[1:i]))$statistic
    add_row(experiment = "growing", size = 5000, method = "Standard KS", rep = rep, seconds = proc.time()[3] - t0)
    cat("growing", rep, "\n")
  }
  for (rep in 1:30) {
    for (w in seq(100, 1000, 100)) {
      set.seed(1000 * rep + w)
      sa <- rnorm(10000)
      sb <- rnorm(10000)
      ta <- runif(10000)
      tb <- runif(10000)
      s <- iks(seed = rep)
      for (i in seq_len(w)) s <- iks_add(iks_add(s, sa[i], 0, ta[i]), sb[i], 1, tb[i])
      t0 <- proc.time()[3]
      for (i in (w + 1):10000) {
        s <- iks_remove(iks_remove(s, sa[i - w], 0, ta[i - w]), sb[i - w], 1, tb[i - w])
        s <- iks_add(iks_add(s, sa[i], 0, ta[i]), sb[i], 1, tb[i])
        iks_statistic(s)
      }
      add_row(experiment = "sliding", size = w, method = "IKS", rep = rep, seconds = proc.time()[3] - t0)
      va <- sort(sa[seq_len(w)])
      vb <- sort(sb[seq_len(w)])
      t0 <- proc.time()[3]
      for (i in (w + 1):10000) {
        va <- optimized_insert(optimized_remove(va, sa[i - w]), sa[i])
        vb <- optimized_insert(optimized_remove(vb, sb[i - w]), sb[i])
        optimized_ks(va, vb)
      }
      add_row(experiment = "sliding", size = w, method = "Optimized KS", rep = rep, seconds = proc.time()[3] - t0)
      if (rep <= 10) {
        t0 <- proc.time()[3]
        for (i in (w + 1):10000) ks.test(sa[(i - w + 1):i], sb[(i - w + 1):i])$statistic
        add_row(experiment = "sliding", size = w, method = "Standard KS", rep = rep, seconds = proc.time()[3] - t0)
      }
    }
    cat("sliding", rep, "\n")
  }
  utils::write.csv(do.call(rbind, rows), file.path(out, "timing_heimdall.csv"), row.names = FALSE)
}

# Classifiers ------------------------------------------------------------------------------------

nn_fit <- function(x, y) list(x = x, y = y)
nn_predict <- function(model, x) {
  out <- character(nrow(x))
  for (start in seq(1, nrow(x), by = 2000)) {
    rows <- start:min(nrow(x), start + 1999)
    d <- outer(rowSums(x[rows, , drop = FALSE]^2), rowSums(model$x^2), "+") - 2 * x[rows, , drop = FALSE] %*% t(model$x)
    out[rows] <- model$y[max.col(-d, ties.method = "first")]
  }
  out
}

nb_fit <- function(x, y, classes) {
  present <- classes[classes %in% y]
  eps <- 1e-9 * max(apply(x, 2, stats::var) * (nrow(x) - 1) / nrow(x), 0)
  mu <- t(vapply(present, function(k) colMeans(x[y == k, , drop = FALSE]), numeric(ncol(x))))
  v <- t(vapply(present, function(k) {
    xi <- x[y == k, , drop = FALSE]
    colMeans((xi - matrix(colMeans(xi), nrow(xi), ncol(xi), byrow = TRUE))^2)
  }, numeric(ncol(x)))) + max(eps, 1e-12)
  list(classes = classes, present = present, mu = matrix(mu, length(present)), v = matrix(v, length(present)),
       prior = as.numeric(table(factor(y, levels = present))) / length(y))
}
nb_proba <- function(model, x) {
  ll <- sapply(seq_along(model$present), function(k) {
    log(model$prior[k]) - 0.5 * rowSums(log(2 * pi * matrix(model$v[k, ], nrow(x), ncol(x), byrow = TRUE)) +
      (x - matrix(model$mu[k, ], nrow(x), ncol(x), byrow = TRUE))^2 / matrix(model$v[k, ], nrow(x), ncol(x), byrow = TRUE))
  })
  ll <- matrix(ll, nrow(x))
  p <- exp(ll - apply(ll, 1, max))
  p <- p / rowSums(p)
  full <- matrix(0, nrow(x), length(model$classes), dimnames = list(NULL, model$classes))
  full[, model$present] <- p
  full
}
nb_predict <- function(model, x) model$classes[max.col(nb_proba(model, x), ties.method = "first")]
nb_loo <- function(x, y, classes) {
  t(vapply(seq_len(nrow(x)), function(i) nb_proba(nb_fit(x[-i, , drop = FALSE], y[-i], classes), x[i, , drop = FALSE])[1, ],
           numeric(length(classes))))
}

dt_fit <- function(x, y) {
  d <- data.frame(x, check.names = FALSE)
  names(d) <- paste0("f", seq_len(ncol(x)))
  d$y <- factor(y)
  model <- rpart::rpart(y ~ ., data = d, method = "class",
                        control = rpart::rpart.control(cp = 0, minsplit = 2, minbucket = 1, xval = 0))
  used <- unique(as.character(model$frame$var))
  used <- as.integer(sub("f", "", used[used != "<leaf>"]))
  list(model = model, used = if (length(used)) sort(used) else seq_len(ncol(x)), single = nlevels(d$y) == 1,
       label = levels(d$y)[1])
}
dt_predict <- function(model, x) {
  if (model$single) return(rep(model$label, nrow(x)))
  d <- data.frame(x, check.names = FALSE)
  names(d) <- paste0("f", seq_len(ncol(x)))
  as.character(stats::predict(model$model, d, type = "class"))
}

classifiers <- list(
  NN = list(fit = function(x, y, classes) nn_fit(x, y), predict = nn_predict),
  NB = list(fit = function(x, y, classes) nb_fit(x, y, classes), predict = nb_predict),
  DT = list(fit = function(x, y, classes) dt_fit(x, y), predict = dt_predict))

# Data -------------------------------------------------------------------------------------------

load_dataset <- function(dir, name) {
  file <- switch(name, Arabic = "Arabic_Paper_Fixed.data", Posture = "Posture.data", Bike = "Bike.data",
                 Keystroke = "Keystroke.data", Insects = "Insects.data", AbruptInsects = "AbruptInsects.data")
  lines <- readLines(file.path(dir, file))
  lines <- lines[nzchar(lines) & !startsWith(lines, "@")]
  d <- utils::read.csv(text = lines, header = FALSE, stringsAsFactors = FALSE)
  list(x = as.matrix(d[, -ncol(d)]), y = as.character(d[[ncol(d)]]),
       window = if (name %in% c("Arabic", "Posture")) 500L else 100L)
}

# Monitors ---------------------------------------------------------------------------------------

# A dfr_iks() detector with a given reference matrix and window matrix.
monitor <- function(reference, window, W, seed, mode = "fixed") {
  det <- dfr_iks(window_size = W, ca = 1.95, reference = mode, seed = seed)
  det$state$features <- paste0("v", seq_len(ncol(reference)))
  det$state <- heimdall:::.iks_detector_build(det$state, reference, window)
  det
}
monitor_step <- function(det, x) {
  row <- as.data.frame(as.list(stats::setNames(x, det$state$features)))
  update_state(det, row)
}

# Protocols --------------------------------------------------------------------------------------

segment_predict <- function(clf, model, x, from, to) if (to >= from) clf$predict(model, x[from:to, , drop = FALSE]) else character(0)

run_method <- function(data, clf_name, method, seed = 1) {
  x <- data$x
  y <- data$y
  N <- nrow(x)
  W <- data$window
  classes <- sort(unique(y))
  clf <- classifiers[[clf_name]]
  pred <- character(N)
  retrain <- 0L
  ab <- 0L
  events <- integer(0)
  set.seed(seed)
  x_train_current <- x[1:W, , drop = FALSE]
  y_train_current <- y[1:W]
  model <- clf$fit(x_train_current, y_train_current, classes)

  if (method == "BL1") {
    pred[(W + 1):N] <- segment_predict(clf, model, x, W + 1, N)
  } else if (method == "TL") {
    for (t in (W + 1):N) {
      m <- clf$fit(x[(t - W):(t - 1), , drop = FALSE], y[(t - W):(t - 1)], classes)
      pred[t] <- clf$predict(m, x[t, , drop = FALSE])
    }
  } else if (method %in% c("MR", "AB")) {
    feats <- if (clf_name == "DT") model$used else seq_len(ncol(x))
    ref <- x[1:W, feats, drop = FALSE]
    det <- monitor(ref, ref, W, seed)
    start <- W + 1
    for (t in (W + 1):N) {
      out <- monitor_step(det, x[t, feats])
      det <- out$obj
      if (!out$drift) next
      pred[start:t] <- segment_predict(clf, model, x, start, t)
      start <- t + 1
      window <- x[(t - W + 1):t, , drop = FALSE]
      done <- FALSE
      if (method == "AB") {
        drifted <- feats[det$state$features %in% det$state$drifted_features]
        train <- x_train_current
        for (j in drifted) {
          sd_ref <- stats::sd(train[, j])
          train[, j] <- if (sd_ref > 0) (train[, j] - mean(train[, j])) * stats::sd(window[, j]) / sd_ref + mean(window[, j])
                        else train[, j] - mean(train[, j]) + mean(window[, j])
        }
        ok <- all(vapply(drifted, function(j) {
          s <- iks(seed = seed + t + j)
          for (i in seq_len(W)) s <- iks_add(iks_add(s, train[i, j], 0, stats::runif(1)), window[i, j], 1, stats::runif(1))
          !iks_test(s, 1.95)
        }, logical(1)))
        if (ok) {
          x_train_current <- train
          model <- clf$fit(train, y_train_current, classes)
          det <- monitor(train[, feats, drop = FALSE], window[, feats, drop = FALSE], W, seed + t)
          ab <- ab + 1L
          events <- c(events, -t)
          done <- TRUE
        }
      }
      if (!done) {
        x_train_current <- window
        y_train_current <- y[(t - W + 1):t]
        model <- clf$fit(window, y_train_current, classes)
        retrain <- retrain + 1L
        events <- c(events, t)
        if (clf_name == "DT") feats <- model$used
        det <- monitor(window[, feats, drop = FALSE], window[, feats, drop = FALSE], W, seed + t)
      }
    }
    pred[start:N] <- segment_predict(clf, model, x, start, N)
  } else if (method %in% c("MRP", "CD")) {
    # MRP: fixed reference of leave-one-out probabilities of the training data.
    # CD: two consecutive windows of the outputs on the stream, filled again
    # (2 W new outputs) after every retraining.
    start_monitor <- function(xw, yw, s) {
      if (method == "MRP") {
        ref <- nb_loo(xw, yw, classes)
        return(monitor(ref, ref, W, s, "fixed"))
      }
      det <- dfr_iks(window_size = W, ca = 1.95, reference = "consecutive", seed = s)
      det$state$features <- paste0("v", seq_along(classes))
      det
    }
    det <- start_monitor(x[1:W, , drop = FALSE], y[1:W], seed)
    for (t in (W + 1):N) {
      p <- nb_proba(model, x[t, , drop = FALSE])[1, ]
      pred[t] <- classes[which.max(p)]
      out <- monitor_step(det, p)
      det <- out$obj
      if (!out$drift) next
      window <- x[(t - W + 1):t, , drop = FALSE]
      model <- nb_fit(window, y[(t - W + 1):t], classes)
      retrain <- retrain + 1L
      events <- c(events, t)
      det <- start_monitor(window, y[(t - W + 1):t], seed + t)
    }
  }
  idx <- (W + 1):N
  list(dataset = NA, classifier = clf_name, method = method, accuracy = 100 * mean(pred[idx] == y[idx]),
       labels = if (method == "TL") 100 else 100 * W * (1 + retrain) / N, retrainings = retrain, ab_adaptations = ab,
       events = events)
}

run_bl2 <- function(data, clf_name, adaptations, runs = 100) {
  x <- data$x
  y <- data$y
  N <- nrow(x)
  W <- data$window
  classes <- sort(unique(y))
  clf <- classifiers[[clf_name]]
  idx <- (W + 1):N
  acc <- numeric(runs)
  for (r in seq_len(runs)) {
    set.seed(r)
    moments <- sort(sample((W + 1):N, adaptations))
    model <- clf$fit(x[1:W, , drop = FALSE], y[1:W], classes)
    pred <- character(N)
    start <- W + 1
    for (t in moments) {
      pred[start:t] <- segment_predict(clf, model, x, start, t)
      start <- t + 1
      model <- clf$fit(x[(t - W + 1):t, , drop = FALSE], y[(t - W + 1):t], classes)
    }
    if (start <= N) pred[start:N] <- segment_predict(clf, model, x, start, N)
    acc[r] <- 100 * mean(pred[idx] == y[idx])
  }
  list(classifier = clf_name, method = "BL2", accuracy = mean(acc), labels = 100 * W * (1 + adaptations) / N,
       retrainings = adaptations, ab_adaptations = 0L, events = integer(0))
}

if (mode == "classify") {
  dir <- args[2]
  out <- args[3]
  cores <- if (length(args) >= 4) as.integer(args[4]) else 4L
  datasets <- c("Keystroke", "Insects", "AbruptInsects", "Bike", "Arabic", "Posture")
  jobs <- list()
  for (d in datasets) {
    for (m in c("BL1", "TL", "MR", "AB")) jobs[[length(jobs) + 1L]] <- list(d = d, c = "NN", m = m)
    for (m in c("BL1", "TL", "MR", "MRP", "CD")) jobs[[length(jobs) + 1L]] <- list(d = d, c = "NB", m = m)
    for (m in c("BL1", "TL", "MR")) jobs[[length(jobs) + 1L]] <- list(d = d, c = "DT", m = m)
  }
  # longest jobs first
  cost <- vapply(jobs, function(j) (j$d == "Posture") * 10 + (j$m %in% c("MRP", "CD", "TL")) * 2 + (j$c == "DT"), numeric(1))
  jobs <- jobs[order(-cost)]
  run_job <- function(job) {
    file <- file.path(out, "runs", sprintf("%s_%s_%s.rds", job$d, job$c, job$m))
    if (file.exists(file)) return(readRDS(file))
    data <- load_dataset(dir, job$d)
    start <- proc.time()[3]
    res <- run_method(data, job$c, job$m)
    res$dataset <- job$d
    res$seconds <- proc.time()[3] - start
    saveRDS(res, file)
    cat(sprintf("%s %s %s: accuracy %.2f labels %.2f retrainings %d (%.0f s)\n", job$d, job$c, job$m, res$accuracy,
                res$labels, res$retrainings, res$seconds))
    res
  }
  dir.create(file.path(out, "runs"), showWarnings = FALSE, recursive = TRUE)
  results <- parallel::mclapply(jobs, function(j) tryCatch(run_job(j), error = function(e) {
    cat("FAILED", j$d, j$c, j$m, conditionMessage(e), "\n")
    NULL
  }), mc.cores = cores, mc.preschedule = FALSE)
  # BL2 with as many adaptations as MR.
  bl2 <- list()
  for (d in datasets) {
    data <- load_dataset(dir, d)
    for (cl in c("NN", "NB", "DT")) {
      mr <- file.path(out, "runs", sprintf("%s_%s_MR.rds", d, cl))
      if (!file.exists(mr)) next
      res <- run_bl2(data, cl, readRDS(mr)$retrainings)
      res$dataset <- d
      bl2[[length(bl2) + 1L]] <- res
      cat(sprintf("%s %s BL2: accuracy %.2f\n", d, cl, res$accuracy))
    }
  }
  all <- c(Filter(Negate(is.null), results), bl2)
  table <- do.call(rbind, lapply(all, function(r) data.frame(dataset = r$dataset, classifier = r$classifier,
                                                             method = r$method, accuracy = r$accuracy,
                                                             labels = r$labels, retrainings = r$retrainings,
                                                             ab_adaptations = r$ab_adaptations)))
  utils::write.csv(table, file.path(out, "classification_heimdall.csv"), row.names = FALSE)
  print(table)
  cat("CLASSIFY_DONE\n")
}
