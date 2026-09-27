#ifndef rmdocinc
!
!svn $Id: pconst.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  Rules for parameter constants:                                      !
!                                                                      !
!  *  Use prefix of "c" for whole real numbers (c1 for 1.0)            !
!  *  Use "m" after prefix to designate negative values (cm1 for -1.0) !
!  *  Use prefix of "p" for non repeating fractions (p5 for 0.5)       !
!  *  Use prefix of "r" for reciprocals (r3 for 1.0/3.0)               !
!  *  Combine use of prefix above and "e" for scientific notation      !
!     (c1e4 for 1.0e+4, c1em4 for 1.0e-4).                             !
!                                                                      !
!=======================================================================
!
#endif
      integer cntplt, secplt
      parameter (cntplt=1, secplt=2)
!
      real cm1, cm3, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11,  &
     &     c20, c25, c50, c90, c100, c180, c200, c255, c300, c360, c366,&
     &     c500, c1000, c5000, c10000, c1em9, c1em10, c1em12, c1em20,   &
     &     c1ep30, p006, p009, p035, p015, p012, p08, p06, p5, p25, p75,&
     &     p98, r3, r10, r20, r33, r35, r40, r50, r80, r100, r200, r250,&
     &     r400, r1000
      real day2sec, deg2rad, grav, cm2m, cm2mm, m2cm, m2km, m2knot,     &
     &     m2mm, m2smallknot, pi, rad2deg, re, root2, sec2day, spval0,  &
     &     spval1, spval2, spvgeo
!
      parameter (cm1=-1.0, cm3=-3.0, c0=0.0, c1=1.0, c2=2.0, c3=3.0,    &
     &           c4=4.0, c5=5.0, c6=6.0, c7=7.0, c8=8.0, c9=9.0, c10=10,&
     &           c11=11.0, c20=20.0, c25=25.0, c50=50.0, c90=90.0,      &
     &           c100=100.0, c180=180.0, c200=200.0, c255=255.0,        &
     &           c300=300.0, c360=360.0, c366=366.0, c500=500,          &
     &           c1000=1000.0, c5000=5000.0, c10000=10000.0,            &
     &           c1em9=1.0e-9, c1em10=1.0e-10, c1em12=1.0e-12,          &
     &           c1em20=1.0e-20, c1ep30=1.0e+30, p006=0.006, p009=0.009,&
     &           p012=0.012, p015=0.015, p035=0.035, p06=0.06, p08=0.08,&
     &           p5=0.5, p25=0.25, p75=0.75, p98=0.98, r3=c1/c3,        &
     &           r10=0.1, r20=0.05, r33=c1/33.0, r35=c1/35.0, r40=0.025,&
     &           r50=0.02, r80=0.0125, r100=0.01, r200=0.005,           &
     &           r250=0.004, r400=0.0025, r1000=0.001)
      parameter (day2sec=86400.0, cm2m=r100, cm2mm=r10, grav=9.801,     &
     &           m2cm=c100, m2km=r1000, m2knot=1.9459, m2mm=c1000,      &
     &           m2smallknot=m2knot*c5, pi=3.14159265358979323846,      &
     &           re=637131500.0, root2=1.41421356237309504880,          &
     &           sec2day=c1/86400.0, spvgeo=999.0, spval0=0.99e+35,     &
     &           spval1=1.0e+35, spval2=0.99e+30)
      parameter (deg2rad=pi/c180, rad2deg=c180/pi)
