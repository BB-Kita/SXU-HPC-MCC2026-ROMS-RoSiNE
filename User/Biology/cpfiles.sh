#!/bin/csh

set pth='/home/xiupeng/ROMS_1D'

echo ${pth}

set bkup=false


cp bio_UMAINE31.h       ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE31_def.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE31.in      ${pth}/ROMS/External/
cp bio_UMAINE31_inp.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE31_mod.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE31_var.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE31_wrt.h   ${pth}/ROMS/Nonlinear/Biology/

cp bio_UMAINE13.h       ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE13_def.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE13.in      ${pth}/ROMS/External/
cp bio_UMAINE13_inp.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE13_mod.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE13_var.h   ${pth}/ROMS/Nonlinear/Biology/
cp bio_UMAINE13_wrt.h   ${pth}/ROMS/Nonlinear/Biology/

cp ana_biology.h  ${pth}/ROMS/Functionals/
cp biology.F      ${pth}/ROMS/Nonlinear/Biology/
cp checkadj.F     ${pth}/ROMS/Utility/
cp checkdefs.F    ${pth}/ROMS/Utility/
cp cppdefs.h      ${pth}/ROMS/Include/
cp globaldefs.h   ${pth}/ROMS/Include/
cp def_diags.F    ${pth}/ROMS/Utility/
cp def_info.F     ${pth}/ROMS/Utility/
cp mod_biology.F  ${pth}/ROMS/Modules/
cp mod_diags.F    ${pth}/ROMS/Modules/
cp mod_ncparam.F  ${pth}/ROMS/Modules/
cp mod_ocean.F    ${pth}/ROMS/Modules/
cp mod_param.F    ${pth}/ROMS/Modules/
cp read_biopar.F  ${pth}/ROMS/Utility/
cp set_diags.F    ${pth}/ROMS/Utility/
cp wrt_diags.F    ${pth}/ROMS/Utility/
cp wrt_info.F     ${pth}/ROMS/Utility/
cp varinfo.dat    ${pth}/ROMS/External/
cp bye24bio13.h   ${path}/ROMS/Include/

if($bkup == 'true') then

cp ${pth}/ROMS/Functionals/ana_biology.h ${pth}/ROMS/Functionals/ana_biology.h.bak
cp ${pth}/ROMS/Nonlinear/Biology/biology.F ${pth}/ROMS/Nonlinear/Biology/biology.F.bak
cp ${pth}/ROMS/Utility/checkadj.F   ${pth}/ROMS/Utility/checkadj.F.bak
cp ${pth}/ROMS/Utility/checkdefs.F   ${pth}/ROMS/Utility/checkdefs.F.bak
cp ${pth}/ROMS/Include/cppdefs.h   ${pth}/ROMS/Include/cppdefs.h.bak
cp ${pth}/ROMS/Include/globaldefs.h  ${pth}/ROMS/Include/globaldefs.h.bak
cp ${pth}/ROMS/Utility/def_diags.F  ${pth}/ROMS/Utility/def_diags.F.bak
cp ${pth}/ROMS/Utility/def_info.F  ${pth}/ROMS/Utility/def_info.F.bak
cp ${pth}/ROMS/Modules/mod_biology.F  ${pth}/ROMS/Modules/mod_biology.F.bak
cp ${pth}/ROMS/Modules/mod_diags.F  ${pth}/ROMS/Modules/mod_diags.F.bak
cp ${pth}/ROMS/Modules/mod_ncparam.F  ${pth}/ROMS/Modules/mod_ncparam.F.bak
cp ${pth}/ROMS/Modules/mod_ocean.F  ${pth}/ROMS/Modules/mod_ocean.F.bak
cp ${pth}/ROMS/Modules/mod_param.F  ${pth}/ROMS/Modules/mod_param.F.bak
cp ${pth}/ROMS/Utility/read_biopar.F  ${pth}/ROMS/Utility/read_biopar.F.bak
cp ${pth}/ROMS/Utility/set_diags.F  ${pth}/ROMS/Utility/set_diags.F.bak
cp ${pth}/ROMS/Utility/wrt_diags.F  ${pth}/ROMS/Utility/wrt_diags.F.bak
cp ${pth}/ROMS/Utility/wrt_info.F  ${pth}/ROMS/Utility/wrt_info.F.bak
cp ${pth}/ROMS/External/varinfo.dat  ${pth}/ROMS/External/varinfo.dat.bak

endif


echo 'Done...'






