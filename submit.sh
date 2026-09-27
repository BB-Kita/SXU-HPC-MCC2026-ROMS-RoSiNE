#!/bin/bash
#SBATCH -p kshcexclu06
#SBATCH -N 4
#SBATCH -n 64
#SBATCH --exclusive
#SBATCH -w j04r2n[08-11]
#SBATCH -J MCC13_best
#SBATCH -t 01:30:00

set -euo pipefail
ulimit -s unlimited
ulimit -l unlimited

ROOT_SOURCE="${MCC_PACKAGE_ROOT:-${SLURM_SUBMIT_DIR:-}}"
if [[ -z "$ROOT_SOURCE" ]]; then
  echo "ERROR: neither MCC_PACKAGE_ROOT nor SLURM_SUBMIT_DIR is available" >&2
  exit 2
fi
ROOT="$(cd "$ROOT_SOURCE" && pwd -P)"
RUN_DIR="${MCC_RUN_DIR:-$ROOT/runs/${SLURM_JOB_ID}}"

bash "$ROOT/preflight.sh" static

module purge
module load mathlib/netcdf/4.4.1/intel \
            mpi/hpcx/2.7.4/intel-2017.5.239 \
            mathlib/hdf5/1.8.20/intel \
            compiler/intel/2017.5.239

bash "$ROOT/preflight.sh" run

if [[ -e "$RUN_DIR" ]]; then
  echo "ERROR: run directory already exists: $RUN_DIR" >&2
  exit 2
fi

RUN_PARENT="$(dirname "$RUN_DIR")"
mkdir -p "$RUN_PARENT"
AVAILABLE_KB="$(df -Pk "$RUN_PARENT" | awk 'NR==2 {print $4}')"
MIN_FREE_KB="${MCC_MIN_FREE_KB:-12000000}"
[[ "$AVAILABLE_KB" =~ ^[0-9]+$ ]] || {
  echo "ERROR: cannot determine available space for $RUN_PARENT" >&2
  exit 3
}
if (( AVAILABLE_KB < MIN_FREE_KB )); then
  echo "ERROR: insufficient free space: ${AVAILABLE_KB} KiB < ${MIN_FREE_KB} KiB" >&2
  exit 3
fi

mkdir -p "$RUN_DIR/output"
ln -s "$ROOT/ROMS" "$RUN_DIR/ROMS"
ln -s "$ROOT/oceanM" "$RUN_DIR/oceanM"
ln -s "$ROOT/coll_rules.conf" "$RUN_DIR/coll_rules.conf"
ln -s "$ROOT/Inputfiles" "$RUN_DIR/Inputfiles"
cd "$RUN_DIR"

export OMP_NUM_THREADS=2
export OMP_PROC_BIND=close
export OMP_PLACES=cores
export OMP_STACKSIZE=1G
export KMP_BLOCKTIME=0
export KMP_HOT_TEAMS=1
export KMP_HOT_TEAMS_MAX_PARALLEL=2

RANKFILE="$RUN_DIR/rankfile.${SLURM_JOB_ID}"
trap 'rm -f "$RANKFILE"' EXIT
mapfile -t NODES < <(scontrol show hostnames "$SLURM_JOB_NODELIST")
PAIRS=(0,1 2,3 8,9 10,11 16,17 18,19 24,25 26,27 4,5 6,7 12,13 14,15 20,21 22,23 28,29 30,31)
rank=0
for node in "${NODES[@]}"; do
  for pair in "${PAIRS[@]}"; do
    echo "rank ${rank}=${node} slot=${pair}" >> "$RANKFILE"
    rank=$((rank+1))
  done
done
[[ "$rank" -eq 64 ]] || {
  echo "ERROR: rankfile contains $rank ranks, expected 64" >&2
  exit 4
}

{
  echo "job_id=$SLURM_JOB_ID"
  echo "nodes=$SLURM_JOB_NODELIST"
  echo "run_dir=$RUN_DIR"
  echo "mpi_ranks=64"
  echo "omp_threads=2"
  echo "ntiles=8x8 per grid"
  echo "ntimes=2592,12960"
  sha256sum "$ROOT/oceanM" "$ROOT/Compilers/Linux-ifort.mk" \
            "$ROOT/ROMS/External/ocean_SCS_Dongsha60_bio15.in"
} > RUN_MANIFEST.txt

{ time mpirun -np 64 --rankfile "$RANKFILE" \
    -x OMP_NUM_THREADS -x OMP_PROC_BIND -x OMP_PLACES \
    -x OMP_STACKSIZE -x KMP_BLOCKTIME -x KMP_HOT_TEAMS \
    -x KMP_HOT_TEAMS_MAX_PARALLEL \
    --mca coll_hcoll_enable 0 \
    --mca coll_tuned_use_dynamic_rules 1 \
    --mca coll_tuned_dynamic_rules_filename "$ROOT/coll_rules.conf" \
    "$ROOT/oceanM" "$ROOT/ROMS/External/ocean_SCS_Dongsha60_bio15.in" \
    > model.log 2>&1; } 2> TIMING.txt

grep -q 'ROMS/TOMS: DONE' model.log

# Keep the packaged validator byte-identical to the official script. Only its
# user-editable test-directory line is changed in this per-run temporary copy.
sed "s|^dir_test = .*|dir_test = '$RUN_DIR/output/'|" "$ROOT/vali.py" > vali_job.py
grep -Fq "dir_test = '$RUN_DIR/output/'" vali_job.py || {
  echo "ERROR: failed to set validator output path" >&2
  exit 5
}
source /public/share/mcc2026_final/miniforge3/etc/profile.d/conda.sh
conda activate vali
python vali_job.py | tee vali.log
grep -q '最终判定：两组文件所有变量RMSE均在阈值范围内' vali.log

echo "RUN_AND_VALIDATION_PASS run_dir=$RUN_DIR"
