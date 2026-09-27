      SUBROUTINE get_bvf (bvf,xpos,ypos,zpos,im,jm,km,igrid,ncgid,      &
     &                    ncfid,idfld,tindex,level,gotzeta,gotsalt,     &
     &                    gotrho,file)
!
!svn $Id: get_bvf.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes Brunt-Vaisala frequency from primary       !
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
!     gotsalt  Logical switch indicating availability of salinity.     !
!     gotrho   Logical switch indicating availability of density.      !
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     bvf      Brunt-Vaisala frequency (real array).                   !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     zpos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    bv_freq, get_nc3dat, get_nc2pos, get_nc3pos               !
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
     &        j, jm, jmr, k, km, kmr, kpr, kr, ncfid, ncgid, nc_rid,    &
     &        nc_sid, nc_tid, tindex
      real g, gorho0, level, rspv, sspv, tspv
      real bvf(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam
      parameter (g=9.808)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      levless=is2d(idfld)
      fldnam1='temp'
      fldnam2='salt'
!
!  Compute Brunt-Vaisala frequency from temperature and salinity.
!
      IF (gotsalt) THEN
        CALL get_nc3dat (f2,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam1,  &
     &                   irgrid,nc_tid,tspv)
        CALL get_nc3dat (f3,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam2,  &
     &                   irgrid,nc_sid,sspv)
        iwgrid=6
        CALL get_nc3pos (z,NV,imr,jmr,Nw,ncgid,ncfid,tindex,irgrid,     &
     &                   iwgrid,gotzeta,file)
        CALL bv_freq (bvf,f2,f3,z,imr-1,jmr-1,kmr)
      ELSE IF (gotrho) THEN
        fldnam='rho'
        CALL get_nc3dat (f2,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_rid,rspv)
        CALL get_nc3pos (z,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,     &
     &                   irgrid,gotzeta,file)
        gorho0=g/rho0
        DO k=1,kmr-1
          DO j=1,jmr
            DO i=1,imr
              kr=i+(j-1)*imr+(k-1)*imr*jmr
              kpr=i+(j-1)*imr+k*imr*jmr
              bvf(kr)=-gorho0*(f2(kpr)-f2(kr))/(z(kpr)-z(kr))
            END DO
          END DO
        END DO
      ELSE
        PRINT 10
  10    FORMAT (/,' GET_BVF - unable to compute Brunt-Vaisala ',        &
     &          'frequency')
        RETURN
      END IF
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
        iwgrid=6
        CALL get_nc3pos (f4,NV,imr,jmr,Nw,ncgid,ncfid,tindex,igrid,     &
     &                   iwgrid,gotzeta,file)
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
!  If appropriate, extract Brunt-Vaisala frequency at the requested
!  level.
!
      IF ((1.le.int(level)).and.(int(level).le.km)) THEN
        DO ip=1,im*jm*km
          f4(ip)=bvf(ip)
        END DO
        ip=0
        DO j=1,jm
          DO i=1,im
            ip=ip+1
            iw=i+(j-1)*im+(int(level)-1)*im*jm
            bvf(ip)=f4(iw)
          END DO
        END DO
      END IF

      RETURN
      END
