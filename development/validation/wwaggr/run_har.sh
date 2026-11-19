#!/bin/sh
# Trains the members of the Human Activity ensembles with heimdall (har_member.R)
# and with the reference code (har_reference.py): 3 ensembles of 10 members for
# BCE, InDiD, TS-CP2 and SN-TS2Vec, and bootstrap BCE ensembles. Jobs whose
# output exists are skipped, so the script can be resumed. Two queues run side by
# side, to fit the memory of the devcontainer (7 GB): the SN-TS2Vec members of
# heimdall and of the reference (about 1.8 GB each) with <heavy> jobs, all the others with <light>.
# Usage (inside the devcontainer, from the package root):
#   sh development/validation/wwaggr/run_har.sh <heavy> <light>
heavy=${1:-3}
light=${2:-3}
work=data-real/wwaggr/work/har
har=data-real/wwaggr/har/har.rds
ref="data-real/wwaggr/reference data-real/wwaggr/betacal_src/python data-real/wwaggr/har/raw $work"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1
mkdir -p $work/members $work/members_r
run='
  if [ "$0" = R ]; then
    Rscript development/validation/wwaggr/har_member.R '"$har"' '"$work"' $1 $2 $3 2>&1 | tail -1 | sed "s/^/R /"
  else
    python3 -I development/validation/wwaggr/har_reference.py '"$ref"' train $1 $2 $3 2>&1 | tail -1 | sed "s/^/reference /"
  fi'
r_jobs() {
  for m in "$@"; do for e in 1 2 3; do for s in 0 1 2 3 4 5 6 7 8 9; do
    [ -f $work/members_r/${m}_${e}_${s}.rds ] || echo "R $m $e $s"
  done; done; done
}
p_jobs() {
  for m in "$@"; do for e in 1 2 3; do for s in 0 1 2 3 4 5 6 7 8 9; do
    [ -f $work/members/${m}_${e}_${s}.json ] || echo "P $m $e $s"
  done; done; done
}
{ r_jobs ts2vec; p_jobs ts2vec; } | xargs -P "$heavy" -L 1 sh -c "$run" &
{ r_jobs tscp; p_jobs tscp indid bce bce_boot; r_jobs indid bce bce_boot; } | xargs -P "$light" -L 1 sh -c "$run"
wait
echo HAR_DONE
