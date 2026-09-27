      SUBROUTINE time_indx (var_time,tindex,day,idfld,file)
!
!svn $Id: time_indx.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2003 ROMS/TOMS Adjoint Group                          !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine determines the time dimension index for the current    !
!  field to read.                                                      !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     day      Day to read (real).                                     !
!     idfld    Identification of field to read (integer)               !
!     file     NetCDF file to read (primary or secondary; integer)     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     var_time Logical switch indicating availability of time          !
!              dimension in requested field.                           !
!     tindex   Time dimension index of field to read.                  !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!     crash                                                            !
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
#include "fldsid.h"
#include "ncinfo.h"
#include "netcdf.inc"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical var_time
      integer dsize, file, idfld, ldim, lvar, n, ncid, nvardim, ncvarid,&
     &        nvatts, status, tindex, i
      integer vardim(10)
      integer lenstr
      real day, tday
      character*20 dname, vname, found
!
!=======================================================================
!  Begin Executable code.
!=======================================================================
!
!  Check if requested variable has time-dimension.
!
      var_time=.false.
      IF (file.eq.1) THEN
        ncid=ncid1
        vname=vname1(idfld)
        lvar=lenstr(vname)
      ELSE IF (file.eq.2) THEN
        ncid=ncid2
        vname=vname1(idfld)
        lvar=lenstr(vname)
      END IF
!
      status=nf_inq_varid(ncid,vname(1:lvar),ncvarid)
      IF (status.ne.nf_noerr) THEN
        PRINT 10, vname(1:lvar)
        CALL crash ('TIME_INDX',status)
      END IF
!
      status=nf_inq_var(ncid,ncvarid,vname,vartyp,nvardim,vardim,       &
     &                  nvatts)
      IF (status.eq.nf_noerr) THEN
        lvar=lenstr(vname)
        DO n=1,nvardim
          status=nf_inq_dim(ncid,vardim(n),dname,dsize)
          ldim=lenstr(dname)
          IF (INDEX(dname(1:ldim),'time').ne.0) THEN
            var_time=.true.
          END IF
        END DO
      ELSE
        PRINT 20, vname(1:lvar)
        CALL crash ('TIME_INDX',status)
      END IF
!
!  Set time-index to process. I added writing of the found string
!  variable to avoid optimization error in some compilers (CYGWIN g77).
!
      tindex=0
      IF (var_time) THEN
        IF (file.eq.1.and.gottime1) THEN
          IF (gottime1) THEN
            DO n=1,ntime1
              tday=time1(n)*Tscale1 
              WRITE (found,'(a)') 'tday.eq.day'
              IF (tday.eq.day) THEN
                tindex=n
                GO TO 30
              END IF
            END DO
          END IF
        ELSE IF (file.eq.2) THEN
          IF (gottime2) THEN
            DO n=1,ntime2
              tday=time2(n)*Tscale2
              WRITE (found,'(a)') 'tday.eq.day'
              IF (tday.eq.day) THEN
                tindex=n
                GO TO 30
              END IF
            END DO
          END IF
        END IF
  30    CONTINUE
        IF (day.gt.tday) THEN
          CALL crash ('DONE',1)
        ELSE IF ((gottime1.or.gottime2).and.(tindex.eq.0)) THEN
          PRINT 40, tindex
          STOP
        END IF          
      END IF
!
  10  FORMAT (/,' TIME_INDX - cannot inquiring ID for variable: ',a)
  20  FORMAT (/,' TIME_INDX - error while inquiring information for',   &
     &        ' variable: ',a)
  40  FORMAT (/,' TIME_INDX - unable to determine NetCDF record to ',   &
     &        'plot, tindex = ', i2,/,13x,                              &
     &        'Possible optimization problem in this routine.')

      RETURN
      END
