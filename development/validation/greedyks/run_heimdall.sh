#!/bin/sh
# Runs the experiments with heimdall (run_greedyks.R) with <cores> cores.
# Usage (inside the devcontainer, from the package root):
#   sh development/validation/greedyks/run_heimdall.sh <cores>
c=${1:-4}
out=development/validation/results/greedyks
r="Rscript development/validation/greedyks/run_greedyks.R"
export OPENBLAS_NUM_THREADS=1
for e in memo_units mean_diff std_diff sample_size; do $r error $e $out $c; done
$r callcenter data-real/greedyks/callcenter/raw $out $c
for e in memo_units mean_diff std_diff sample_size; do $r effectiveness $e $out $c; done
echo HEIMDALL_DONE
