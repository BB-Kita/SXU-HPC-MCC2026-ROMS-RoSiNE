#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
cd "$ROOT"

if [[ ! -s "$ROOT/pgo/pgopti.dpi" ]]; then
  echo "ERROR: missing PGO database: $ROOT/pgo/pgopti.dpi" >&2
  exit 2
fi

if ! grep -q '^#undef PROFILE' "$ROOT/ROMS/Include/globaldefs.h"; then
  echo "ERROR: PROFILE must remain disabled for the submission build" >&2
  exit 3
fi

if ! grep -q -- '-fimf-use-svml=true:pow' "$ROOT/Compilers/Linux-ifort.mk"; then
  echo "ERROR: GLS SVML pow flag is missing" >&2
  exit 4
fi

bash "$ROOT/preflight.sh" static

module purge
module load compiler/intel/2021.3.0 \
            mpi/hpcx/2.7.4/intel-2017.5.239 \
            mathlib/netcdf/4.4.1/intel \
            mathlib/hdf5/1.8.20/intel

bash "$ROOT/preflight.sh" build

GMAKE=/opt/rh/devtoolset-7/root/usr/bin/gmake
"$GMAKE" clean
"$GMAKE" -j64

echo "Build completed: $ROOT/oceanM"
sha256sum "$ROOT/oceanM"
