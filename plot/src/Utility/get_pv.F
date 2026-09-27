      SUBROUTINE get_pv (pv,xpos,ypos,zpos,im,jm,km,igrid,ncgid,ncfid,  &
     &                   idfld,tindex,level,gotzeta,gotsalt,gotrho,file)
!
!svn $Id: get_pv.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes potential vorticity from primary fields    !
!  in input NetCDF file.                                               !
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
!     pv       Requested 3D potential vorticity (real array).          !
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
      integer file, i, idfld, igrid, im, imr, ip, irgrid, iw, iwgrid,   &
     &        j, jm, jmr, k, km, kmr, kr, kpr, ncfid, ncgid, nc_fid,    &
     &        nc_rid, nc_sid, nc_tid, tindex
      integer idens
      real level, rspv, sspv, tspv, spv
      real f(NH), pv(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!     levless=is2d(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
!
!  Get depths at RHO-points.
!
      irgrid=1
      CALL get_nc3pos (zpos,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,    &
     &                 irgrid,gotzeta,file)
!
!  Get density anomaly (kg/m^3).
!
      IF (gotrho) THEN
        fldnam='rho'
        CALL get_nc3dat (f2,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_rid,rspv)
      ELSE IF (gotsalt) THEN
        CALL get_nc3dat (f3,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam1,  &
     &                   irgrid,nc_tid,tspv)
        CALL get_nc3dat (f4,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam2,  &
     &                   irgrid,nc_sid,sspv)
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
  10    FORMAT (/,' GET_PV - unable to get/compute density anomaly.')
        RETURN
      END IF
!
!  Read in Coriolis parameter.
!
      fldnam='f'
      CALL get_nc2dat (f,NH,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,      &
     &                 nc_fid,spv)
!
!-----------------------------------------------------------------------
!  Compute potential vorticity (1/s/m) at W-points.
!-----------------------------------------------------------------------
!
      im=imr
      jm=jmr
      km=Nw-2
      DO k=1,kmr
        DO j=1,jmr
          DO i=1,imr
            ip=i+(j-1)*imr
            kr=ip+(k-1)*imr*jmr
            kpr=ip+k*imr*jmr
            pv(kr)=f(ip)*(f2(kpr)-f2(kr))/(rho0*(zpos(kpr)-zpos(kr)))
            pv(kr)=abs(pv(kr))
          END DO
        END DO
      END DO
!
!  Get horizontal positions at RHO-points
!
      igrid=1
      CALL get_nc2pos (xpos,ypos,NV,imr,jmr,ncgid,igrid)
!
!  Get vertical positions at W-points.
!
      IF ((level.le.c0).or.(level.ge.1000)) THEN
        iwgrid=6
        CALL get_nc3pos (z,NV,imr,jmr,Nw,ncgid,ncfid,tindex,igrid,      &
     &                   iwgrid,gotzeta,file)
      END IF
!
!  Extract positions of size (Lp,Mp,Nm) for potential vorticity.
!
      IF ((level.le.c0).or.(level.ge.1000)) THEN
        ip=0
        DO k=2,Nw-1
          DO j=1,jmr
            DO i=1,imr
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
