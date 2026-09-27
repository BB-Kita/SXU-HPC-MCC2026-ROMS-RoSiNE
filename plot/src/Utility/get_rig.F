      SUBROUTINE get_rig (Rig,xpos,ypos,zpos,im,jm,km,igrid,ncgid,ncfid,&
     &                    idfld,tindex,level,gotzeta,gotsalt,gotrho,    &
     &                    file)
!
!svn $Id: get_rig.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes gradient Richardson number.                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     idfld    Identification of field to read (integer).              !
!     tindex   Time dimension index to read (integer).                 !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real). If LEVEL=0, the full 3D is passed.      !
!     gotzeta  Logical switch indicating availability of free-         !
!              surface.                                                !
!     gotsalt  Logical switch indicating availability of salinity.     !
!     gotrho   Logical switch indicating availability of density.      !
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Rig      Gradient Richardson number (real array).                !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     zpos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    get_bvf, get_nc3dat, get_nc3pos, ri_number                !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "domdat.h"
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
#include "pltfld.h"
#include "pltlab.h"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotrho, gotsalt, gotzeta
      integer file, i, idfld, igrid, im, imr, imu, imv, ip, ir, irgrid, &
     &        iugrid, ivgrid, iw, iwgrid, j, jm, jmr, jmu, jmv, k, km,  &
     &        kmr, ncfid, ncgid, nc_uid, nc_vid, tindex
      real level, uspv, vspv
      real Rig(NV), xpos(NV), ypos(NV), zpos(NV)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute Brunt-Vaisala frequency.  Store into f2.
!-----------------------------------------------------------------------
!
      CALL get_bvf (f2,x,y,z,imr,jmr,km,igrid,ncgid,ncfid,idfld,tindex, &
     &              0,gotzeta,gotsalt,gotrho,file)
!
!-----------------------------------------------------------------------
!  Compute gradient Richardson number.
!-----------------------------------------------------------------------
!
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
!
!  Read in total U-velocity component and store into f3.
!
      CALL get_nc3dat (f3,NV,imu,jmu,kmr,ncgid,ncfid,tindex,fldnam1,    &
     &                 iugrid,nc_uid,uspv)
!
!  Read in total V-velocity component and store into f4.
!
      CALL get_nc3dat (f4,NV,imv,jmv,kmr,ncgid,ncfid,tindex,fldnam2,    &
     &                 ivgrid,nc_vid,vspv)
!
!  Get depths at RHO-points.
!
      irgrid=1
      CALL get_nc3pos (z,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,irgrid,&
     &                 gotzeta,file)
!
!  Compute gradient Richarson number.
!
      CALL ri_number (Rig,f2,f3,f4,z,imr-1,jmr-1,Nr)
!
!  Get vertical positions at W-points.
!
      IF ((level.le.c0).or.(level.ge.1000)) THEN
        iwgrid=6
        CALL get_nc3pos (z,NV,imr,jmr,Nw,ncgid,ncfid,tindex,irgrid,     &
     &                   iwgrid,gotzeta,file)
      END IF
!
!  Extract positions of size (Lm,Mm,Nm) for Richardson number.  Also
!  extract bath of size (Lm,Mm).
!
      im=imr-2
      jm=jmr-2
      km=Nw-2
      igrid=7
      DO ip=1,imr*jmr
        f4(ip)=bath(ip)
      END DO
      ip=0
      DO j=2,jmr-1
        DO i=2,imr-1
          ip=ip+1
          ir=i+(j-1)*imr
          xpos(ip)=x(ir)
          ypos(ip)=y(ir)
          bath(ip)=f4(ir)
        END DO
      END DO
      IF ((level.le.c0).or.(level.ge.1000)) THEN
        ip=0
        DO k=2,Nw-1
          DO j=2,jmr-1
            DO i=2,imr-1
              ip=ip+1
              iw=i+(j-1)*imr+(k-1)*imr*jmr
              zpos(ip)=z(iw)
            END DO
          END DO
        END DO
      END IF
!
!  If appropriate, extract Richardson number at the requested level.
!
      IF ((1.le.int(level)).and.(int(level).le.km)) THEN
        DO ip=1,im*jm*km
          f4(ip)=Rig(ip)
        END DO
        ip=0
        DO j=1,jm
          DO i=1,im
            ip=ip+1
            iw=i+(j-1)*im+(int(level)-1)*im*jm
            Rig(ip)=f4(iw)
          END DO
        END DO
      END IF

      RETURN
      END
