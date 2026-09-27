#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODE="${1:-static}"

fail() {
  echo "PREFLIGHT_ERROR: $*" >&2
  exit 2
}

require_file() {
  [[ -r "$1" ]] || fail "missing or unreadable file: $1"
}

require_dir() {
  [[ -d "$1" ]] || fail "missing directory: $1"
}

require_file "$ROOT/oceanM"
[[ -x "$ROOT/oceanM" ]] || fail "oceanM is not executable: $ROOT/oceanM"
require_file "$ROOT/coll_rules.conf"
require_file "$ROOT/vali.py"
require_file "$ROOT/pgo/pgopti.dpi"
require_file "$ROOT/ROMS/External/ocean_SCS_Dongsha60_bio15.in"
require_file "$ROOT/ROMS/External/bio_UMAINE15.in"
require_dir "$ROOT/Inputfiles"

INPUT="$ROOT/ROMS/External/ocean_SCS_Dongsha60_bio15.in"
grep -Eq '^[[:space:]]*NtileI[[:space:]]*==[[:space:]]*8[[:space:]]+8([[:space:]]|$)' "$INPUT" || \
  fail "NtileI is not 8 8 in $INPUT"
grep -Eq '^[[:space:]]*NtileJ[[:space:]]*==[[:space:]]*8[[:space:]]+8([[:space:]]|$)' "$INPUT" || \
  fail "NtileJ is not 8 8 in $INPUT"
grep -Eq '^[[:space:]]*NTIMES[[:space:]]*==[[:space:]]*2592[[:space:]]+12960([[:space:]]|$)' "$INPUT" || \
  fail "NTIMES is not the full 2592 12960 configuration in $INPUT"

for rel in \
  SCS/SCS_grd.nc \
  Dongsha60/Dongsha60_grd.nc \
  SCS/SCS_rst_year4_half2_ini_Chl12.nc \
  Dongsha60/Dongsha60_ini_from_SCS_rst_year4_half2.nc \
  Dongsha60/SCS_Dongsha60_contact.nc \
  SCS/SCS_bio_bry2006.nc \
  SCS/roms_frc_tide_SCS.nc \
  SCS/SCS_frc_shf2021_ERA5.nc \
  SCS/SCS_frc_swf2021_ERA5.nc \
  SCS/SCS_frc_swrad2021_ERA5.nc \
  SCS/SCS_frc_wind2021_CCMP6hourly.nc \
  SCS/SCS_frc_dQdSST2021_ERA5.nc \
  Dongsha60/Dongsha60_frc_shf2021_ERA5.nc \
  Dongsha60/Dongsha60_frc_swf2021_ERA5.nc \
  Dongsha60/Dongsha60_frc_swrad2021_ERA5.nc \
  Dongsha60/Dongsha60_frc_dQdSST2021_ERA5.nc \
  Dongsha60/Dongsha60_frc_wind2021_CCMP6hourly.nc; do
  require_file "$ROOT/Inputfiles/$rel"
done
require_file "$ROOT/ROMS/External/varinfo.dat"

[[ "$(grep -c '^dir_test = ' "$ROOT/vali.py")" -eq 1 ]] || \
  fail "vali.py must contain exactly one replaceable dir_test line"
grep -q '^#undef PROFILE' "$ROOT/ROMS/Include/globaldefs.h" || \
  fail "ROMS internal PROFILE must remain disabled"
grep -q -- '-fimf-use-svml=true:pow' "$ROOT/Compilers/Linux-ifort.mk" || \
  fail "GLS SVML pow flag is missing"

case "$MODE" in
  static)
    ;;
  build)
    command -v mpif90 >/dev/null 2>&1 || fail "mpif90 is unavailable after module load"
    command -v ifort >/dev/null 2>&1 || fail "ifort is unavailable after module load"
    [[ -x /opt/rh/devtoolset-7/root/usr/bin/gmake ]] || fail "required gmake is unavailable"
    ;;
  run)
    command -v mpirun >/dev/null 2>&1 || fail "mpirun is unavailable after module load"
    command -v scontrol >/dev/null 2>&1 || fail "scontrol is unavailable"
    [[ -n "${SLURM_JOB_ID:-}" ]] || fail "submit.sh must run inside a Slurm allocation"
    [[ -n "${SLURM_JOB_NODELIST:-}" ]] || fail "SLURM_JOB_NODELIST is empty"
    mapfile -t preflight_nodes < <(scontrol show hostnames "$SLURM_JOB_NODELIST")
    [[ "${#preflight_nodes[@]}" -eq 4 ]] || fail "expected exactly 4 nodes, got ${#preflight_nodes[@]}"
    ;;
  *)
    fail "unknown preflight mode: $MODE"
    ;;
esac

echo "PREFLIGHT_PASS mode=$MODE root=$ROOT"
