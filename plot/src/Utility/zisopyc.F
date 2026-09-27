      SUBROUTINE zisopyc (day,depths,s1,s2,file,isoden,idfld)
!
!svn $Id: zisopyc.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the depths of requested isopycnal.  This      !
!  is used for plotting fields along isopycnic surfaces.               !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     day      Day to read (real).                                     !
!     s1       scratch array (real).                                   !
!     s2       scratch array (real).                                   !
!     file     NetCDF file to read (primary or secondary; integer).    !
!     isoden   Isopycnal value to process (real; kg/m3, >1000).        !
!     idfld    Identification of field to read (integer).              !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     depths    Depths of isopycnal (m).                               !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "fields.h"
#include "fldsid.h"
#include "ndimen.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer file, idfld, idsigt, igrid, n, np
      real day, dval, isoden, level
      real depths(NH), s1(NV), s2(NV)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Determine field ID for sigma-t.
!
      DO n=1,nvar
        IF (vcode(n).eq.'IDSIGT') idsigt=n
      END DO
!
!  Compute density: sigma-t.
!
      level=c0
      CALL get_3dfld (day,x,y,z,s1,s2,np,igrid,file,idsigt,level)
!
!  Extract depths of requested isopycnal. Notice that isopycnal is
!  concerted to density anomaly.
!
      dval=isoden-c1000
      CALL depth_iso (depths,dval,s1,z,im,jm,km,spval1)

      RETURN
      END
