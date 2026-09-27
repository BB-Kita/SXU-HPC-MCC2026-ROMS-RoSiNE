      SUBROUTINE slevels
!
!svn $Id: slevels.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws model vertical terrain-following levels in a     !
!  cross-section.                                                      !
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
#include "colpal.h"
#include "fields.h"
#include "ncinfo.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltncar.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, igrid, ip, k, kms_save, ks
      integer file, nc_gid
      real xmap(NX), ymap(NX)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Read in horizontal and vertical grid positions.
!
      file=1
      igrid=1
      IF (readgrd) THEN
        nc_gid=ncgrd
       ELSE
        nc_gid=ncid1
      END IF
      CALL get_nc3pos (z,NV,im,jm,km,nc_gid,ncid1,tindx,igrid,         &
     &                 igrid,gotzeta1,file)
      CALL get_nc2pos (f3,f4,NV,im,jm,nc_gid,igrid)
!
!  Extract native, terrain-following level for a cross-section.
!  Overwrite temporarily the number of vertical section levels
!  to allow different values to the data number of levels (km).
!
      kms_save=kms
      kms=km
      CALL xsection (f7,ims,ks,z,f3,f4,z,im,jm,km,vintrp,spval1)
      kms=kms_save
!
!-----------------------------------------------------------------------
!  Draw vertical, terrain-following levels.
!-----------------------------------------------------------------------
!
!  Initialize use of polylines.
!
      CALL sflush
      CALL gsplci (ncl+4)
!
!  Set viewport.
!
      CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
!
!  Draw model levels.
!
      DO k=1,km
        DO i=1,ims
          ip=i+(k-1)*ims
          xmap(i)=x(ip)
          ymap(i)=y(ip)
        END DO
        CALL curve (xmap,ymap,ims)
      END DO
      CALL sflush
      CALL gsplci (1)
      CALL gslwsc (c1)

      RETURN
      END
