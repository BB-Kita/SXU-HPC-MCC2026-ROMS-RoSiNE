      SUBROUTINE get_ertel (pv,xpos,ypos,zpos,im,jm,km,igrid,ncgid,     &
     &                      ncfid,idfld,tindex,level,gotzeta,file)
!
!svn $Id: get_ertel.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes Ertel potential vorticity from primary     !
!  fields in input NetCDF file.                                        !
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
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     pv        Requested 3D Ertel potential vorticity (real array).   !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     zpos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    ertel, get_nc2dat, get_nc3dat, get_nc2pos, get_nc3pos     !
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
#include "pltfld.h"
#include "pltlab.h"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotzeta
      integer file, i, idfld, igrid, im, imr, imu, imv, ip, ir, irgrid, &
     &        iugrid, ivgrid, iw, iwgrid, j, jm, jmr, jmu, jmv, k, km,  &
     &        kmr, ncfid, ncgid, nc_fid, nc_pid, nc_rid, nc_uid, nc_vid,&
     &        tindex
      real level, rspv, spv, uspv, vspv
      real f(NH), pv(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!!    levless=is2d(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
!
!  Read in Coordinate transformation metrics.
!
      fldnam='pm'
      CALL get_nc2dat (x,NV,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,      &
     &                 nc_pid,spv)
      fldnam='pn'
      CALL get_nc2dat (y,NV,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,      &
     &                 nc_pid,spv)
!
!  Get depths at RHO-points.
!
      irgrid=1
      CALL get_nc3pos (z,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,irgrid,&
     &                 gotzeta,file)
!
!  Read in total U-velocity component and store in f2.
!
      CALL get_nc3dat (f2,NV,imu,jmu,kmr,ncgid,ncfid,tindex,fldnam1,    &
     &                 iugrid,nc_uid,uspv)
!
!  Read in total V-velocity component and store in f3.
!
      CALL get_nc3dat (f3,NV,imv,jmv,kmr,ncgid,ncfid,tindex,fldnam2,    &
     &                 ivgrid,nc_vid,vspv)
!
!  Read in potential density anomaly and store in f4.
!
      fldnam='rho'
      CALL get_nc3dat (f4,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam1,    &
     &                 irgrid,nc_rid,rspv)
!
!  Read in Coriolis parameter.
!
      fldnam='f'
      CALL get_nc2dat (f,NH,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,      &
     &                 nc_fid,spv)
!
!-----------------------------------------------------------------------
!  Compute Ertel potential vorticity (1/s/m) at W-points.
!-----------------------------------------------------------------------
!
      CALL ertel (pv,imr-1,jmr-1,kmr,f2,f3,f4,f,x,y,z,f5,f6)
!
!  Get horizontal positions at RHO-points
!
      CALL get_nc2pos (x,y,NV,imr,jmr,ncgid,irgrid)
!
!  Get vertical positions at W-points.
!
      IF ((level.le.c0).or.(level.ge.1000)) THEN
        iwgrid=6
        CALL get_nc3pos (z,NV,imr,jmr,Nw,ncgid,ncfid,tindex,irgrid,     &
     &                   iwgrid,gotzeta,file)
      END IF
!
!  Extract positions of size (Lm,Mm,Nm) for Ertel potential vorticity.
!  Also extract bath of size (Lm,Mm).
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
!  If appropriate, extract Ertel potential vorticity at the requested
!  level.
!
      IF ((1.le.int(level)).and.(int(level).le.km)) THEN
        DO ip=1,im*jm*km
          f4(ip)=pv(ip)
        END DO
        ip=0
        DO j=1,jm
          DO i=1,im
            ip=ip+1
            iw=i+(j-1)*im+(int(level)-1)*im*jm
            pv(ip)=f4(iw)
          END DO
        END DO
      END IF

      RETURN
      END
