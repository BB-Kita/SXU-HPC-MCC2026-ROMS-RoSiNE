      SUBROUTINE get_ape (ape,xpos,ypos,zpos,im,jm,km,igrid,ncgid,      &
     &                    ncfid,idfld,tindex,level,gotzeta,gotsalt,     &
     &                    gotrho,file)
!
!svn $Id: get_ape.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes available potential energy.                !
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
!     ape      Requested 3D available potential energy (real array).   !
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
      integer file, i, idfld, igrid, im, imr, ip, irgrid, iw, iwgrid,   &
     &        j, jm, jmr, k, km, kmr, kmz, kpr, kr, kz, ncfid, ncgid,   &
     &        nc_rid, nc_sid, nc_tid, tindex
      real g, gorho0, level, rspv, sspv, tspv
      real idens
      real ape(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam
      parameter (g=9.81)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
!
!  Get density anomaly (kg/m^3).
!
      IF (gotrho) THEN
        CALL get_nc3dat (f2,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam1,  &
     &                   irgrid,nc_rid,rspv)
      ELSE IF (gotsalt) THEN
        fldnam='temp'
        CALL get_nc3dat (f3,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_tid,tspv)
        fldnam='salt'
        CALL get_nc3dat (f4,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_sid,sspv)
        irgrid=1
        CALL get_nc3pos (z,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,     &
     &                   irgrid,gotzeta,file)
        ip=0
        DO k=1,kmr
          DO j=1,jmr
            DO i=1,imr
              ip=ip+1
              f2(ip)=idens(f3(ip),f4(ip),z(ip))
            END DO
          END DO
        END DO
      ELSE
        PRINT 10
  10    FORMAT (/,' GET_APE - unable to get/compute potential density.')
        RETURN
      END IF
!
!  Get depths (m) at RHO- and W-points.
!
      IF (gotrho) THEN
        irgrid=1
        CALL get_nc3pos (z,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,     &
     &                   irgrid,gotzeta,file)
      END IF
      iwgrid=6
      CALL get_nc3pos (f3,NV,imr,jmr,Nw,ncgid,ncfid,tindex,irgrid,      &
     &                 iwgrid,gotzeta,file)
!
!  Compute potential energy (Lp,Mp,Nm).
!
      ip=0
      gorho0=g/rho0
      DO k=1,kmr
        DO j=1,jmr
          DO i=1,imr
            ip=ip+1
            f4(ip)=-gorho0*f2(ip)*f3(ip)
            f5(ip)=f4(ip)
          END DO
        END DO
      END DO
!
!  Integrate vertically.
!
      DO k=kmr-1,1
        DO j=1,jmr
          DO i=1,imr
            ip=i+(j-1)*imr
            kr=ip+(k-1)*imr*jmr
            kpr=ip+k*imr*jmr
            kz=ip+k*imr*jmr
            kmz=ip+(k-1)*imr*jmr
            f5(kr)=f5(kpr)+f4(kr)/(f3(kz)-f3(kmz))
          END DO
        END DO
      END DO
      ip=0
      DO k=1,kmr-1
        DO j=1,jmr
          DO i=1,imr
            ip=ip+1
            kr=i+(j-1)*imr+(k-1)*imr*jmr
            ape(ip)=f5(kr)
          END DO
        END DO
      END DO
!
!  Get horizontal positions (Lp,Mp) at RHO-points
!
      im=imr
      jm=jmr
      km=Nw-2
      igrid=1
      CALL get_nc2pos (xpos,ypos,NV,imr,jmr,ncgid,igrid)
!
!  Get vertical positions (Lp,Mp,Nm) at W-points.
!
      IF ((level.le.c0).or.(level.ge.1000)) THEN
        ip=0
        DO k=2,Nw-1
          DO j=1,jmr
            DO i=1,imr
              ip=ip+1
              iw=i+(j-1)*imr+(k-1)*imr*jmr
              zpos(ip)=f3(iw)
            END DO
          END DO
        END DO
      END IF
!
!  If appropriate, extract available potential energy at the requested
!  level.
!
      IF ((1.le.int(level)).and.(int(level).le.km)) THEN
        DO ip=1,im*jm*km
          f4(ip)=ape(ip)
        END DO
        ip=0
        DO j=1,jm
          DO i=1,im
            ip=ip+1
            iw=i+(j-1)*im+(int(level)-1)*im*jm
            ape(ip)=f4(iw)
          END DO
        END DO
      END IF

      RETURN
      END
