"""BERT sentence embeddings and drift scores of the reference code
(https://github.com/aws-samples/detecting-data-drift-in-nlp-using-amazon-sagemaker-custom-model-monitor).

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/cosine/embed_reference.py <bert_dir> <reference_dir> <out_dir>

<bert_dir> holds bert-base-uncased (config.json, vocab.txt, model.safetensors).

* Baseline (notebook, cell 37): the 8551 sentences of
  cola_public/raw/in_domain_train.tsv, encode_plus(max_length=64, padded to
  64, attention mask), BertModel(output_hidden_states=True), the second to
  last hidden layer, averaged over the 64 positions (padding included).
  pad_to_max_length=True of transformers 2.x is padding="max_length" in
  transformers 4; CoLA has no sentence over 64 tokens, so truncation is a
  no-op.
* Monitored inputs (docker/evaluation.py): encode_plus(max_length=64,
  padding=True, truncation=True) of a single sentence (so no padding), same
  layer, averaged over the tokens; the score is the loop
  sum(1 - scipy cosine(input, e)) / len(embeddings), and a violation is a
  score below THRESHOLD (0.5 in the notebook).
  Sets: the 14 sentences sent to the endpoint in the notebook (cell 30),
  CoLA in_domain_dev.tsv (527, same sources as the baseline) and
  out_of_domain_dev.tsv (516, other sources).
* The baseline is also embedded without padding (mean over its tokens), to
  measure the effect of the padding mismatch between baseline and inputs.

Writes <out_dir>/: baseline_padded.bin, baseline_unpadded.bin and
<set>.bin (float64, rows x 768) and reference_scores.json.
"""

import json
import sys
import time
from pathlib import Path

import numpy as np
import torch
from scipy.spatial.distance import cosine
from transformers import BertModel, BertTokenizer

NOTEBOOK_SENTENCES = [
    "CLI to download the zip file",
    "Thanks so much for driving me home",
    "construct the sub-embeddings and corresponding baselines",
    "our Bert model and interpret what the model",
    "Bert models using Captum library",
    "case study we focus on a fine-tuned Question Answering model on SQUAD datase",
    "we pretrain the model, we can load ",
    "need to define baselines / references, nu",
    "defines numericalized special tokens ",
    "Thanks so much for cooking dinner. I really appreciate it",
    "let's define the ground truth for prediction's start and en",
    "pre-computation of embeddings for the second option is necessary because",
    "to summarize attributions for each word token in the sequence.",
    "Nice to meet you, Sergio. So, where are you from",
]


def read_cola(path):
    sentences = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            sentences.append(parts[3])
    return sentences


def embed(model, tokenizer, sentence, padded):
    if padded:
        encoded = tokenizer.encode_plus(sentence, add_special_tokens=True, max_length=64, padding="max_length",
                                        truncation=True, return_attention_mask=True, return_tensors="pt")
    else:
        encoded = tokenizer.encode_plus(sentence, add_special_tokens=True, max_length=64, padding=True,
                                        return_attention_mask=True, return_tensors="pt", truncation=True)
    with torch.no_grad():
        outputs = model(encoded["input_ids"], encoded["attention_mask"])
        hidden_states = outputs[2]
        token_vecs = hidden_states[-2][0]
        return torch.mean(token_vecs, dim=0).numpy()


def main(bert_dir, reference_dir, out_dir, threshold=0.5):
    torch.set_num_threads(7)
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    tokenizer = BertTokenizer.from_pretrained(bert_dir)
    model = BertModel.from_pretrained(bert_dir, output_hidden_states=True)
    model.eval()
    cola = Path(reference_dir, "cola_public", "raw")
    train = read_cola(cola / "in_domain_train.tsv")
    lengths = [len(tokenizer.encode(s, add_special_tokens=True)) for s in train]
    print(f"baseline: {len(train)} sentences, longest {max(lengths)} tokens", flush=True)
    for name, padded in [("baseline_padded", True), ("baseline_unpadded", False)]:
        started = time.time()
        matrix = np.array([embed(model, tokenizer, s, padded) for s in train], dtype=np.float32)
        matrix.astype(np.float64).tofile(out / f"{name}.bin")
        print(f"{name}: {matrix.shape} in {time.time() - started:.0f} s", flush=True)
    embedding_list = np.fromfile(out / "baseline_padded.bin").reshape(-1, 768).astype(np.float32)
    sets = {"notebook": NOTEBOOK_SENTENCES, "in_domain_dev": read_cola(cola / "in_domain_dev.tsv"),
            "out_of_domain_dev": read_cola(cola / "out_of_domain_dev.tsv")}
    results = {"threshold": threshold, "n_baseline": len(train), "longest_baseline_tokens": max(lengths), "sets": {}}
    for name, sentences in sets.items():
        started = time.time()
        embedded = np.array([embed(model, tokenizer, s, False) for s in sentences], dtype=np.float32)
        embedded.astype(np.float64).tofile(out / f"{name}.bin")
        # The scoring loop of docker/evaluation.py, unchanged.
        scores = []
        for input_sentence_embedding in embedded:
            cosine_score = 0
            for embed_item in embedding_list:
                cosine_score += (1 - cosine(input_sentence_embedding, embed_item))
            cosine_score_avg = cosine_score / (len(embedding_list))
            scores.append(float(cosine_score_avg))
        results["sets"][name] = {"sentences": sentences, "scores": scores,
                                 "violations": [s < threshold for s in scores]}
        print(f"{name}: {len(sentences)} sentences, {sum(s < threshold for s in scores)} violations, "
              f"{time.time() - started:.0f} s", flush=True)
    json.dump(results, open(out / "reference_scores.json", "w"), indent=1)


if __name__ == "__main__":
    main(*sys.argv[1:4])
