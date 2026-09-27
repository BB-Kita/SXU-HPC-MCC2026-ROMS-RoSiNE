      SUBROUTINE crash (string,ierr)
!
!svn $Id: crash.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine is used to terminate execution during normal or     !
!  abnormal  computations.   All output units are properly flushed     !
!  and closed.                                                         !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "ncinfo.h"
#include "netcdf.inc"
#include "pltfld.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer ierr, status
      character*(*) string
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Close output NetCDF files.
!
      status=nf_close(ncid1)
      IF ((iref.eq.2).or.(iref.eq.3)) status=nf_close(ncid2)
!
!  Close GKS software.
!
      CALL clsgks
!
!  Terminate execution.
!
      IF (string(1:4).eq.'DONE') THEN
        PRINT 10, string
  10    FORMAT (a)
      ELSE
        IF (ierr.ne.nf_noerr) PRINT 10, nf_strerror (ierr)
        PRINT 20, string
  20    FORMAT (/,' Execution abnormally terminated in module: ',a,/)
      END IF
#ifdef NO_EXIT
      STOP
#else
      CALL exit (ierr)
#endif
      RETURN
      END
