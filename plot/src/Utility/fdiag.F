      SUBROUTINE fdiag (f,im,jm,level,day,idfld)
!
!svn $Id: fdiag.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes field diagnostics.                            !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        Field (real array).                                     !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real).                                         !
!     day      Day (real)                                              !
!     idfld    Field identification.                                   !
!                                                                      !
!  Required common blocks: (only relevant variables documented)        !
!                                                                      !
!     /FDIAGN/                                                         !
!                                                                      !
!     fadv     average deviation (real; output)                        !
!     favg     mean (real; output)                                     !
!     fkur     kurtosis (real; output)                                 !
!     fmax     maximum value (real; output)                            !
!     fmin     minimum value (real; output)                            !
!     frms     root mean square (real; output)                         !
!     fsdv     standard deviation (real; output)                       !
!     fskw     skewness (real; output)                                 !
!     fvar     variance (real; output)                                 !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "fdiagn.h"
#include "fldsid.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, idfld, imax, im, imin, j, jm, jmax, jmin, lenfld, npts
      integer lenstr
      real day, level, p, s, small, term
      real f(im,jm)
      character*60 fldlab
      parameter (small=1.0e-20)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute the mean and get minimum and maximum values.
!-----------------------------------------------------------------------
!
      npts=0
      favg=c0
      fmin=spval1
      imin=1
      jmin=1
      fmax=-spval1
      imax=1
      jmax=1
      DO j=1,jm
        DO i=1,im
          IF (f(i,j).lt.spval0) THEN
            IF (f(i,j).lt.fmin) THEN
              fmin=f(i,j)
              imin=i
              jmin=j
            END IF
            IF (f(i,j).gt.fmax) THEN
              fmax=f(i,j)
              imax=i
              jmax=j
            END IF
            npts=npts+1
            favg=favg+f(i,j)
          END IF
        END DO
      END DO
      IF (npts.ne.0) favg=favg/FLOAT(npts)
!
!-----------------------------------------------------------------------
!  Compute the root mean square "frms", average deviation "fadev",
!  standard deviation "fsdv", variance "fvar", skewness "fskew", and
!  kurtosis "fkur".
!-----------------------------------------------------------------------
!
      frms=c0
      fadv=c0
      fvar=c0
      fskw=c0
      fkur=c0
      npts=0
      DO j=1,jm
        DO i=1,im
          IF (f(i,j).lt.spval0) THEN
            npts=npts+1
            frms=frms+f(i,j)*f(i,j)
            s=f(i,j)-favg
            fadv=fadv+ABS(s)
            p=s*s
            fvar=fvar+p
            p=p*s
            fskw=fskw+p
            p=p*s
            fkur=fkur+p
          END IF
        END DO
      END DO
      IF (npts.gt.1) THEN
        frms=SQRT(frms/FLOAT(npts))
        fadv=fadv/FLOAT(npts)
        fvar=fvar/FLOAT(npts-1)
        fsdv=SQRT(fvar)
        term=FLOAT(npts)*fsdv**3
        IF (term.gt.small) THEN
          fskw=fskw/term
        ELSE
          fskw=spval1
        END IF
        term=FLOAT(npts)*fvar**2
        IF (term.gt.small) THEN
          fkur=fkur/term
        ELSE
          fkur=spval1
        END IF
      ELSE
        favg=spval1
        frms=spval1
        fadv=spval1
        fvar=spval1
        fsdv=spval1
        fskw=spval1
        fkur=spval1
      END IF
!
!-----------------------------------------------------------------------
!  Report diagnostics.
!-----------------------------------------------------------------------
!
      fldlab=vlabel(idfld)
      lenfld=lenstr(fldlab)
      PRINT 10, day, fldlab(1:lenfld)
      IF (level.ne.20000.0) THEN
        IF (level.gt.c0) THEN
          IF (level.gt.c1000) THEN
            PRINT 20, level
          ELSE
            PRINT 30, INT(level)
          END IF
        ELSE
          PRINT 40, ABS(level)
        END IF
      END IF
      PRINT 50, npts, fmin, imin, jmin, fmax, imax, jmax, favg, frms,   &
     &          fadv, fvar, fsdv, fskw, fkur
!
  10  FORMAT (/,'  Field Diagnostics:',/,/                              &
     &        10x,'                Day = ',f10.3,/,                     &
     &        10x,'         Field type = ',a)
  20  FORMAT (10x,'Depths of Isopycnic = ',f7.2)
  30  FORMAT (10x,'        Model level = ',i2)
  40  FORMAT (10x,'              Depth = ',f7.2,' m')
  50  FORMAT (10x,'   Number of points = ',i7,/,                        &
     &        10x,'            Minimum = ',1pe15.8,                     &
     &         5x,'( ',i4,',',i4,' )',/,                                &
     &        10x,'            Maximum = ',1pe15.8,                     &
     &         5x,'( ',i4,',',i4,' )',/,                                &
     &        10x,'               Mean = ',1pe15.8,/,                   &
     &        10x,'                RMS = ',1pe15.8,/,                   &
     &        10x,'  Average Deviation = ',1pe15.8,/,                   &
     &        10x,'           Variance = ',1pe15.8,/,                   &
     &        10x,' Standard Deviation = ',1pe15.8,/,                   &
     &        10x,'           Skewness = ',1pe15.8,/,                   &
     &        10x,'           Kurtosis = ',1pe15.8,/)

      RETURN
      END
