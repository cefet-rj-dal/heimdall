#!/bin/sh
# Runs the reference experiments (greedyks_reference.py): absolute error,
# effectiveness and call center, with <processes> processes.
# Usage (inside the devcontainer, from the package root):
#   sh development/validation/greedyks/run_reference.sh <processes>
p=${1:-4}
ref="python3 -I development/validation/greedyks/greedyks_reference.py data-real/greedyks/reference data-real/greedyks/work"
for e in memo_units mean_diff std_diff sample_size; do $ref error $e $p; done
$ref callcenter data-real/greedyks/callcenter/raw $p
for e in memo_units mean_diff std_diff sample_size; do $ref effectiveness $e $p; done
echo REFERENCE_DONE
