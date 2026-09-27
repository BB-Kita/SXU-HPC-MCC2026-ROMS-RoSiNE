#!/bin/bash
#$ -V
#$ -j yes

cp ~/Outputfiles/bye24/bye24_frc_tides_M2.nc   ./bye24_frc_tides.nc
cp ~/Outputfiles/bye24/bye24_grd.nc ./bye24_grd.nc 

mpirun -np 20 ./oceanM ROMS/External/ocean_bye24bio13.in &> logGTbio_2006_0926.txt &


