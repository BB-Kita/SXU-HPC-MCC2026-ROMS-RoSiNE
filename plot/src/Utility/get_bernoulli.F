      SUBROUTINE get_bernoulli (B,xpos,ypos,zpos,im,jm,km,igrid,ncgid,  &
     &                          ncfid,idfld,tindex,level,gotzeta,       &
     &                          gotsalt,gotrho,file)
!
!svn $Id: get_bernoulli.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes hydrostatic dynamic pressure.              !
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
!     file     NetCDF file to read (primary or secondary; integer)     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     B        Requested Bernoulli function (real array; Pa).          !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     ypos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    get_nc3dat, get_nc2pos, get_nc3pos, idens                 !
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
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotrho, gotsalt, gotzeta
      integer file, i, idfld, igrid, im, imr, imu, imv, ip, irgrid,     &
     &        iugrid, ivgrid, iwgrid, j, jm, jmr, jmu, jmv, k, km, kmr, &
     &        ncfid, ncgid, nc_rid, nc_sid, nc_tid, nc_uid, nc_vid,     &
     &        nc_zid, tindex
      real level, rspv, sspv, tspv, uspv, vspv, zspv
      real idens
      real B(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
!
!  Get depths (m) at RHO-points.
!
      irgrid=1
      CALL get_nc3pos (zpos,NV,Lr,Mr,Nr,ncgid,ncfid,tindex,irgrid,      &
     &                 irgrid,gotzeta,file)
!
!  Get density anomaly (kg/m^3).
!
      IF (level.gt.c1000) THEN
        fldnam='temp'
        CALL get_nc3dat (f3,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_tid,tspv)
        fldnam='salt'
        CALL get_nc3dat (f4,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_sid,sspv)
        ip=0
        DO k=1,kmr
          DO j=1,jmr
            DO i=1,imr
              ip=ip+1
              f2(ip)=idens(f3(ip),f4(ip),c0)
            END DO
          END DO
        END DO
      ELSE
        IF (gotrho) THEN
          CALL get_nc3dat (f2,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam1,&
     &                     irgrid,nc_rid,rspv)
        ELSE IF (gotsalt) THEN
          fldnam='temp'
          CALL get_nc3dat (f3,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam, &
     &                     irgrid,nc_tid,tspv)
          fldnam='salt'
          CALL get_nc3dat (f4,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam, &
     &                     irgrid,nc_sid,sspv)
          ip=0
          DO k=1,kmr
            DO j=1,jmr
              DO i=1,imr
                ip=ip+1
                f2(ip)=idens(f3(ip),f4(ip),zpos(ip))
              END DO
            END DO
          END DO
        ELSE
          PRINT 10
  10      FORMAT (/,' GET_BERNOULLI - unable to get/compute density ',  &
     &            'anomaly.')
          RETURN
        END IF
      END IF
!
!  Get free-surface.
!
      IF (gotzeta) THEN
        fldnam='zeta'
        CALL get_nc2dat (f3,NV,imr,jmr,ncgid,ncfid,tindex,fldnam,0,     &
     &                   irgrid,nc_zid,zspv)
      ELSE
        ip=0
        DO j=1,jmr
          DO i=1,imr
            ip=ip+1
            f3(ip)=c0
          END DO
        END DO
      END IF
!
!  Get depths (m) at W-points.
!
      iwgrid=6
      CALL get_nc3pos (f4,NV,imr,jmr,Nw,ncgid,ncfid,tindex,irgrid,      &
     &                 iwgrid,gotzeta,file)
!
!  Get horizontal velocity components
!
      fldnam='u'
      CALL get_nc3dat (f5,NV,imu,jmu,kmr,ncgid,ncfid,tindex,fldnam,     &
     &                 iugrid,nc_uid,uspv)
      fldnam='v'
      CALL get_nc3dat (f6,NV,imv,jmv,kmr,ncgid,ncfid,tindex,fldnam,     &
     &                 ivgrid,nc_vid,vspv)
!
!-----------------------------------------------------------------------
!  Compute Bernoulli function (Pa).
!-----------------------------------------------------------------------
!
      im=imr
      jm=jmr
      IF ((level.le.c0).or.(level.ge.c1000)) THEN
        km=1
        interpolate=.false.
      END IF
      CALL bernoulli (B,f2,f5,f6,f3,f4,zpos,ziso,imr-1,jmr-1,kmr,level)
!
!  Get horizontal positions (Lp,Mp) at RHO-points
!
      igrid=1
      CALL get_nc2pos (xpos,ypos,NV,imr,jmr,ncgid,igrid)

      RETURN
      END
