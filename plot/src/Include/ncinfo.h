#ifndef rmdocinc
!
!svn $Id: ncinfo.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  Tscale1     Scale factor to convert time of primary file to days.   !
!  Tscale2     Scale factor to convert time of secondary file to days. !
!  Vtransform1 Vertical transformation equation for primary file.      !
!  Vtransform2 Vertical transformation equation for secondary file.    !
!  dt          Time interval (sec) between first and last available    !
!                records.                                              !
!  fcst1       Forecast period in primary NetCDF file.                 !
!  fcst2       Forecast period in secondary NetCDF file.               !
!  gotangle1   Logical switch indicating availability of grid          !
!                rotation angle in primary file.                       !
!  gotrho1     Logical switch indicating availability of density in    !
!                primary file.                                         !
!  gotadtime1  Logical switch indicating availability of ocean_time    !
!                in adjoint primary file.                              !
!  gottime1    Logical switch indicating availability of ocean_time    !
!                in primary file.                                      !
!  gotsalt1    Logical switch indicating availability of salinity in   !
!                primary file.                                         !
!  gotzeta1    Logical switch indicating availability of free-surface  !
!                in primary file.                                      !
!  gotangle2   Logical switch indicating availability of grid          !
!                rotation angle in secondary file.                     !
!  gotrho2     Logical switch indicating availability of density in    !
!                secondary file.                                       !
!  gotadtime2  Logical switch indicating availability of ocean_time    !
!                in adjoint secondary file.                            !
!  gottime2    Logical switch indicating availability of scrum_time    !
!                secondary file.                                       !
!  gotsalt2    Logical switch indicating availability of salinity in   !
!                secondary file.                                       !
!  gotzeta2    Logical switch indicating availability of free-surface  !
!                in secondary file.                                    !
!  lev1        Input model levels in primary file.                     !
!  lev2        Input model levels in secondary file.                   !
!  maxvar      Maximum number of variables for input NetCDF files.     !
!  nvars       Number of variables defined in current input NetCDF     !
!                file.                                                 !
!  nvdims      Number of dimensions for each variables in current      !
!                input NetCDF file.                                    !
!  ncgrd       NetCDF ID for grid file.                                !
!  ncid1       NetCDF ID for primary file.                             !
!  ncid2       NetCDF ID for secondary file.                           !
!  nfcst1      Size of forecast period dimension in primary file.      !
!  nfcst2      Size of forecast period dimension in secondary file.    !
!  nlev1       number of input levels in primary file.                 !
!  nlev2       number of input levels in secondary file.               !
!  ntime1      Size of unlimited time dimension in primary file.       !
!  ntime2      Size of unlimited time dimension in secondary file.     !
!  tindx       Time record index in current input NetCDF file.         !
!  time1       Time coordinate values in primary NetCDF file.          !
!  time2       Time coordinate values in secondary NetCDF file.        !
!  type1       Global NetCDF attribute "type" for primary file.        !
!  type2       Global NetCDF attribute "type" for secondary file.      !
!  varid       Generic ID for arbitrary variables in NetCDF files.     !
!  varnam      Names of all variables in current input NetCDF file.    !
!  var_time1   Logical switch indicating availability of time          !
!                dimension in current variable from primary file.      !
!  var_time2   Logical switch indicating availability of time          !
!                dimension in current variable from primary file.      !
!  vdims       Dimension IDs for each of the variables in current      !
!                input NetCDF file.                                    !
!  vname       Variables names and attributes for NetCDF files.        !
!  zinp1       Input flat depths in primary file, if any.              !
!  zinp2       Input flat depths in secondary file, if any.            !
!                                                                      !
!***********************************************************************
!
#endif
      integer maxvar
      parameter (maxvar=500)

      common /lncinfo/ gotangle1, gotrho1, gottime1, gotsalt1, gotzeta1,&
     &                 gotangle2, gotrho2, gottime2, gotsalt2, gotzeta2,&
     &                 gotadtime1, gotadtime2, var_time1, var_time2
      logical gotangle1, gotrho1, gottime1, gotsalt1, gotzeta1,         &
     &        gotangle2, gotrho2, gottime2, gotsalt2, gotzeta2,         &
     &        gotadtime1, gotadtime2, var_time1, var_time2

      common /incinfo/ ncgrd, ncid1, ncid2, nfcst1, nfcst2, nlev1,      &
     &                 nlev2, ntime1, ntime2, nvars, tindx, varid,      &
     &                 vartyp, lev1, lev2, nvdims, vdims, Vtransform1,  &
     &                 Vtransform2
      integer ncgrd, ncid1, ncid2, nfcst1, nfcst2, nlev1, nlev2,        &
     &        ntime1, ntime2, nvars, tindx, varid, vartyp, Vtransform1, &
     &        Vtransform2
      integer lev1(NK), lev2(NK), nvdims(maxvar), vdims(5,maxvar)

      common /fncinfo/ zinp1, zinp2
      real zinp1(NK), zinp2(NK)

      common /dncinfo/ Tscale1, Tscale2, dt, timeref1, timeref2,        &
     &                 fcst1, fcst2, time1, time2
      real*8 Tscale1, Tscale2, dt, timeref1, timeref2,                  &
     &       fcst1(5000), fcst2(5000),                                  &
     &       time1(5000), time2(5000)

      common /cncinfo/ type1, type2, version, varnam
      character*5   version
      character*15  varnam(maxvar)
      character*80  type1, type2
