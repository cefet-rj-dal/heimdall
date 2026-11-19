#!/bin/sh
# Reference runs of the STUDD validation (inside the devcontainer, from the package root).
# Usage: sh development/validation/studd/run_reference.sh <parallel jobs>
set -e
jobs=${1:-6}
script=development/validation/studd/studd_reference.py
args="data-real/studd/reference data-real/studd/work/skmultiflow_053 data-real/studd/datasets data-real/studd/work"
for d in Keystroke AbruptInsects Insects; do python3 -I $script $args trace $d 1; done
{
  for s in 1 2 3; do echo "Posture $s"; done
  for d in Bike AbruptInsects Insects Keystroke; do for s in 1 2 3 4 5 6 7 8 9 10; do echo "$d $s"; done; done
} | xargs -P "$jobs" -L 1 sh -c "python3 -I $script $args run \$0 \$1 | tail -1"
echo REFERENCE_DONE
