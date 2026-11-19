# Validation: cosine similarity drift check for embeddings

Checks `cosine_score()` and `dfr_cosine()` against two sources:

* Nigenda et al. (2022), *Amazon SageMaker Model Monitor: A System for
  Real-Time Insights into Deployed Machine Learning Models* (KDD 2022),
  Section 6.2, "Detecting data drift in NLP models".
* The code of that use case:
  <https://github.com/aws-samples/detecting-data-drift-in-nlp-using-amazon-sagemaker-custom-model-monitor>.
  It consists of `detect-data-drift-nlp-custom-model-monitor.ipynb` and
  `docker/evaluation.py`.

The method has three steps:

1. Every input text gets a BERT sentence embedding.
2. The input is scored by its average cosine similarity with the embeddings
   of the training texts (the baseline).
3. The scores are averaged over the monitoring window (one hour of traffic),
   and a drift is reported when that average is below a threshold. The
   reference code checks each input on its own, with `THRESHOLD = 0.5`.

```sh
# from the package root
git clone https://github.com/aws-samples/detecting-data-drift-in-nlp-using-amazon-sagemaker-custom-model-monitor \
  data-real/smm/reference
# bert-base-uncased (config.json, vocab.txt, tokenizer files, model.safetensors) from
# https://huggingface.co/google-bert/bert-base-uncased into data-real/smm/bert-base-uncased
# inside the devcontainer: about 40 minutes
python3 -I development/validation/cosine/embed_reference.py data-real/smm/bert-base-uncased \
  data-real/smm/reference data-real/smm/embeddings > development/validation/results/cosine/embed.log
python3 -I development/validation/cosine/float_check.py data-real/smm/embeddings
Rscript development/validation/cosine/run_cosine.R > development/validation/results/cosine/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/cosine/`.

## Data

The data set is CoLA (the Corpus of Linguistic Acceptability), shipped in the
reference repository as `cola_public/`. It is the data of the paper's use
case: its sentences appear in Figure 4.

* **Baseline:** the 8551 sentences of `raw/in_domain_train.tsv`, as in the
  notebook.
* **Monitored sets:**
  * the 14 sentences that the notebook sends to the endpoint, which are
    machine-learning jargon and chat;
  * `raw/in_domain_dev.tsv`: 527 sentences from the same sources as the
    baseline;
  * `raw/out_of_domain_dev.tsv`: 516 sentences from other linguistics
    publications.

The embeddings are computed by `embed_reference.py`, with the code of the
notebook and of `evaluation.py`. Both use `bert-base-uncased`, take the
second-to-last hidden layer, and average it over the token positions:

* **Baseline:** padded to 64 tokens, so the mean includes the `[PAD]`
  positions.
* **Inputs:** not padded.

`pad_to_max_length=True` from transformers 2.x is written
`padding="max_length"` in transformers 4.57, the version in the container. No
CoLA sentence has more than 47 tokens, so the truncation at 64 never applies.
The scores are computed with the loop of `evaluation.py`, unchanged:
`sum(1 - scipy cosine(x, e)) / N`.

## 1. Equivalence (`equivalence.csv`)

| set | sentences | largest difference | violations at 0.5 (R / reference) |
|---|---|---|---|
| notebook | 14 | 1.4e-6 | 3 / 3 |
| in_domain_dev | 527 | 2.9e-6 | 11 / 11 |
| out_of_domain_dev | 516 | 2.7e-6 | 7 / 7 |

* **Origin of the differences:** the embeddings are float32, and scipy's
  `cosine()` computes in float32. Rerunning the same loop in float64 gives a
  largest difference of 1.4e-6 from the reference loop, and 4.1e-15 from the
  identity used by `cosine_score()` (`float_check.log`).
* **The identity:** the average cosine is the inner product of the normalised
  input with the mean of the normalised baseline. This makes the score one
  inner product per input instead of N. `cosine_score()` scores the 516
  sentences in 0.27 s; the reference loop takes 72 s.

## 2. Scores (`scores.csv`, `notebook_sentences.csv`)

| baseline | set | mean | sd | 1% quantile | violations at 0.5 |
|---|---|---|---|---|---|
| reference (padded) | baseline sentences | 0.644 | 0.058 | 0.472 | 1.9% |
| reference (padded) | in_domain_dev | 0.645 | 0.057 | 0.468 | 2.1% |
| reference (padded) | out_of_domain_dev | 0.656 | 0.056 | 0.492 | 1.4% |
| reference (padded) | notebook | 0.601 | 0.134 | 0.352 | 21% (3 of 14) |
| unpadded | in_domain_dev | 0.617 | 0.038 | 0.493 | 1.1% |
| unpadded | out_of_domain_dev | 0.624 | 0.037 | 0.508 | 0.8% |

* **Out-of-domain CoLA:** its texts come from other publications, but the
  same kind of sentences. They score as high as the in-domain dev set, and
  slightly higher. The check does not separate them: at 0.5 it flags fewer
  out-of-domain than in-domain sentences.
* **Notebook sentences:** they lower the mean (0.60), but only three cross
  0.5, and these three are the chat sentences. They are "Thanks so much for
  driving me home", "Thanks so much for cooking dinner…" and "Nice to meet
  you, Sergio…", scoring 0.35-0.38. The machine-learning jargon, the most
  foreign text to the CoLA baseline, scores 0.58-0.72.
* **Effect of padding the baseline:** padding the baseline and not the
  inputs (a mismatch in the reference code) raises the scores by about 0.03
  and their spread. It does not change the conclusions. `dfr_cosine()` scores
  whatever embeddings it is given, so the choice belongs to the embedding
  step.

## 3. Stream (`stream.csv`)

`dfr_cosine()` uses the baseline embeddings as `reference`. The stream is the
in-domain dev set, then the out-of-domain dev set, then the 14 notebook
sentences.

Two thresholds are used:

* 0.5, from the reference code;
* a calibrated one: the 1% quantile of the mean of `window_size` baseline
  scores, from 2000 random windows of baseline sentences embedded as inputs.

| window | threshold | drift rate in-domain | out-of-domain | notebook |
|---|---|---|---|---|
| 1 | 0.5 | 2.1% | 1.4% | 3 of 14 |
| 1 | calibrated (0.480) | 1.3% | 0.6% | 3 of 14 |
| 10 | 0.5 | 0 | 0 | 0 of 1 |
| 10 | calibrated (0.599) | 1.9% | 0 | 0 of 1 |
| 50 | 0.5 or calibrated (0.624) | 0 | 0 | — |

The table uses the reference (padded) baseline; the unpadded one gives the
same pattern.

* **Calibrated threshold:** the in-domain false-alarm rate of the windows
  stays near the nominal 1%.
* **Fixed 0.5:** averaging over a window makes it never fire, because the
  mean score is about 0.64 with a standard deviation of 0.06.
* **What it catches:** individual sentences very unlike the baseline (3 of
  the 14). The shift from in-domain to out-of-domain CoLA is not detected by
  windows at any of these settings.

## Not reproduced

* The paper gives no numbers for this use case (Figure 4 is a diagram), and
  the notebook does not print the result of its monitoring job. The
  validation therefore relies on the equivalence with the code, plus the
  behaviour on CoLA.
* The other monitors of the paper are not implemented. These are the t-test
  and Kolmogorov-Smirnov distance tests of Section 4.2, the bias metrics with
  bootstrap (Adult, Section 6.1) and the NDCG of feature attributions
  (Section 4.4). They are other techniques, and the code given is that of the
  NLP use case.
* The SageMaker infrastructure (data capture, S3, scheduling, CloudWatch) is
  not reproduced; `dfr_cosine()` is the analysis step.
