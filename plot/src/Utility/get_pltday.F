      FUNCTION get_pltday (irec,idfld,file)
!
!svn $Id: get_pltday.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine returns the day to plot according to the field type    !
!  and the time irec counter.                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     irec         Time record to process (integer).                   !
!     idfld        Field identification (integer).                     !
!     file         Input NetCDF file (integer):                        !
!                   file=1  primary                                    !
!                   file=2  secondary                                  !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     GET_PLTDAY   day to plot (real).                                 !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "ncinfo.h"
#include "pltfld.h"
#include "pltio.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer file, i, idfld, irec
      real day, tscale
      real get_pltday
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  If appropriate, determine first and last day to plot.  Use primary
!  file time coordinate.
!
      IF (irec.eq.0) THEN
        IF (gottime1) THEN
          tscale=sec2day
          var_time1=.true.
        ELSE
          tscale=c1
        END IF
        IF (frstd.lt.c0) THEN
          IF (ABS(INT(frstd)).le.ntime1) THEN
            Lrecord=.true.
            frstr=ABS(INT(frstd))
            frstd=time1(frstr)*tscale
            IF (dskip.eq.c0) THEN
              rskip=1
            ELSE
              rskip=ABS(INT(dskip))
            END IF
          ELSE
            PRINT 10, ABS(INT(frstd)), ntime1
  10        FORMAT (/,'GET_PLTDAY - starting time record = ',i6,/,      &
     &              13x,'exceeds number of available records = ',i6)
            CALL crash ('DONE',1)
          END IF
        ELSE IF ((frstd.eq.c0).and.(time1(1)*tscale.ge.c0)) THEN
          frstr=1
          frstd=time1(frstr)*tscale
          IF (dskip.le.c0) THEN
            rskip=1
          ELSE
            rskip=0
            IF (gotadtime1) THEN
              DO i=1,ntime1
                IF ((time1(i)*tscale).lt.(frstd-dskip)) THEN
                  rskip=rskip+1
                END IF
              END DO
            ELSE
              DO i=1,ntime1
                IF ((time1(i)*tscale).lt.(frstd+dskip)) THEN
                  rskip=rskip+1
                END IF
              END DO
            END IF
          END IF
        ELSE
          frstr=1
          DO i=1,ntime1
            IF (frstd.eq.(time1(i)*tscale)) THEN
              frstr=i
            END IF
          END DO
          IF (dskip.le.c0) THEN
            rskip=1
          ELSE
            rskip=0
            IF (gotadtime1) THEN
              DO i=1,ntime1
                IF ((time1(i)*tscale).lt.(frstd-dskip)) THEN
                  rskip=rskip+1
                END IF
              END DO
            ELSE
              DO i=1,ntime1
                IF ((time1(i)*tscale).lt.(frstd+dskip)) THEN
                  rskip=rskip+1
                END IF
              END DO
            END IF
          END IF
        END IF
        IF (lastd.lt.c0) THEN
          IF (ABS(INT(lastd)).le.ntime1) THEN
            lastr=abs(int(lastd))
            lastd=time1(lastr)*tscale
          ELSE
            lastr=ntime1
            lastd=time1(lastr)
          END IF
        ELSE IF (lastd.eq.c0) THEN
          lastr=ntime1
          lastd=time1(lastr)*tscale
        ELSE
          lastr=0
          DO i=1,ntime1
            IF (lastd.eq.(time1(i)*tscale)) THEN
              lastr=i
            END IF
          END DO
        END IF
        get_pltday=frstd
        dt=lastd-frstd
      ELSE
!
!  Get time from primary NetCDF file.
!
        IF (file.eq.1) THEN
          day=time1(irec)*Tscale1
          dt=(time1(ntime1)-time1(1))*Tscale1
          IF (irec.gt.ntime1) CALL crash ('DONE',1)
!
!  Get time from secondary NetCDF file.
!
        ELSE IF (file.eq.2) THEN
          day=time2(irec)*Tscale2
          dt=(time2(ntime2)-time2(1))*Tscale2
          IF (irec.gt.ntime2) CALL crash ('DONE',1)
        END IF
        get_pltday=day
      END IF
      dt=dt*day2sec

      RETURN
      END
